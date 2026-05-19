library(tidyverse)
library(maps)
library(patchwork)
library(sf)

dat <- read_csv("data/N2O_summary_data_04012026.csv", show_col_types = FALSE)
dir.create("plots/maps", showWarnings = FALSE, recursive = TRUE)

# ── Panel title labels ─────────────────────────────────────────────────────────
tgt_titles    <- c(Avg = "Average recommended pH", Max = "Maximum recommended pH")
ef_titles     <- c(Wang = "Linear model",          Qiu = "Quadratic model")
ph_mod_titles <- c(RF   = "Random Forest (RF)",    EBK = "Empirical Bayesian Kriging (EBK)")


# ══════════════════════════════════════════════════════════════════════════════
# STEP 1: Compute county-level avoided emissions
# ══════════════════════════════════════════════════════════════════════════════
# Cross every county-crop row with all 8 combinations of:
#   ef_model   (Wang, Qiu)       — emission factor model
#   n_scenario (NActual, Combined) — management scenario
#   target_ph  (Avg, Max)        — recommended pH target
#
# For each combination, avoided_kgN = current emissions - target emissions.
# Logic varies by whether the county is already above the pH target:
#
#   NActual scenario (pH management only):
#     - Above target: avoided = 0         (no liming needed)
#     - Below target: avoided = cur_mean - tgt_mean
#
#   Combined scenario (pH management + 50% N reduction):
#     - Above target: avoided = cur_mean - cur_nred   (N reduction benefit only)
#     - Below target: avoided = cur_mean - tgt_mean   (both benefits)
#
#   Qiu model: where liming results in a net increase in N2O (avoided emissions < 0),
#   clip avoided to 0 rather than reporting a negative value.
#
# Rows are then summed to county totals (kg → kt by dividing by 1e6).

county_avoided <- expand_grid(
    # Select only the columns needed for this calculation
    dat |> select(
      State, County, MeanEBK, pH_Avg, pH_Max,
      N2O_WangEBK_NActual_mean,  N2O_QiuEBK_NActual_mean,   # current emissions
      N2O_WangEBK_NReduced_mean, N2O_QiuEBK_NReduced_mean,  # current pH, reduced N
      N2O_WangAvg_NActual_mean,  N2O_WangAvg_NReduced_mean,  # Wang, avg target pH
      N2O_WangMax_NActual_mean,  N2O_WangMax_NReduced_mean,  # Wang, max target pH
      N2O_QiuAvg_NActual_mean,   N2O_QiuAvg_NReduced_mean,   # Qiu, avg target pH
      N2O_QiuMax_NActual_mean,   N2O_QiuMax_NReduced_mean    # Qiu, max target pH
    ),
    ef_model   = c("Wang", "Qiu"),
    n_scenario = c("NActual", "Combined"),
    target_ph  = c("Avg", "Max")
  ) |>
  mutate(
    # Is the county already at or above the recommended pH target?
    ph_tgt = case_when(target_ph == "Avg" ~ pH_Avg, target_ph == "Max" ~ pH_Max),
    above  = MeanEBK > ph_tgt,

    # Current emissions: actual N inputs at current (EBK) pH
    cur_mean = case_when(
      ef_model == "Wang" ~ N2O_WangEBK_NActual_mean,
      ef_model == "Qiu"  ~ N2O_QiuEBK_NActual_mean
    ),

    # Current emissions with reduced N inputs (used for above-target counties
    # in the Combined scenario, which still receive the N-reduction benefit)
    cur_nred = case_when(
      ef_model == "Wang" ~ N2O_WangEBK_NReduced_mean,
      ef_model == "Qiu"  ~ N2O_QiuEBK_NReduced_mean
    ),

    # Target emissions: at the recommended pH, with actual N (NActual) or
    # reduced N (Combined)
    tgt_mean = case_when(
      ef_model == "Wang" & target_ph == "Avg" & n_scenario == "NActual"  ~ N2O_WangAvg_NActual_mean,
      ef_model == "Wang" & target_ph == "Avg" & n_scenario == "Combined" ~ N2O_WangAvg_NReduced_mean,
      ef_model == "Wang" & target_ph == "Max" & n_scenario == "NActual"  ~ N2O_WangMax_NActual_mean,
      ef_model == "Wang" & target_ph == "Max" & n_scenario == "Combined" ~ N2O_WangMax_NReduced_mean,
      ef_model == "Qiu"  & target_ph == "Avg" & n_scenario == "NActual"  ~ N2O_QiuAvg_NActual_mean,
      ef_model == "Qiu"  & target_ph == "Avg" & n_scenario == "Combined" ~ N2O_QiuAvg_NReduced_mean,
      ef_model == "Qiu"  & target_ph == "Max" & n_scenario == "NActual"  ~ N2O_QiuMax_NActual_mean,
      ef_model == "Qiu"  & target_ph == "Max" & n_scenario == "Combined" ~ N2O_QiuMax_NReduced_mean
    ),

    # Avoided emissions
    avoided_kgN = case_when(
      n_scenario == "NActual"  &  above ~ 0,                   # already at pH target
      n_scenario == "NActual"  & !above ~ cur_mean - tgt_mean, # pH management benefit
      n_scenario == "Combined" &  above ~ cur_mean - cur_nred, # N reduction only
      n_scenario == "Combined" & !above ~ cur_mean - tgt_mean  # pH management + N reduction
    ),

    # Qiu model: set to 0 where liming results in a net increase in N2O (avoided < 0)
    avoided_kgN = if_else(ef_model == "Qiu", pmax(avoided_kgN, 0), avoided_kgN)
  ) |>
  # Sum crop-level rows to county totals (kg → kt)
  group_by(State, County, ef_model, n_scenario, target_ph) |>
  summarise(
    avoided_ktN  = sum(avoided_kgN, na.rm = TRUE) / 1e6,
    above_target = all(above, na.rm = TRUE),
    .groups = "drop"
  )


# ══════════════════════════════════════════════════════════════════════════════
# STEP 2: Join emissions data to map polygons
# ══════════════════════════════════════════════════════════════════════════════
# The maps package uses lowercase names with no "county" suffix and no
# punctuation. Virginia independent cities (e.g. "Suffolk City") are
# county-equivalent administrative units — strip " city" to match map polygons.
# Exception: "Charles City" is a real county name and must be preserved.

clean_name <- function(x) {
  result <- str_to_lower(x) |>
    str_remove("\\s+county$") |>
    str_remove_all("[\\'\\.]") |>
    str_trim()
  if_else(
    str_detect(result, "\\scity$") & result != "charles city",
    str_remove(result, "\\scity$"),
    result
  )
}

county_map_df <- map_data("county")
state_map_df  <- map_data("state")

joined <- county_map_df |>
  left_join(
    county_avoided |>
      mutate(region = clean_name(State), subregion = clean_name(County)),
    by           = c("region", "subregion"),
    relationship = "many-to-many"
  )

# Report how many counties matched (unmatched counties appear grey on maps)
n_matched <- joined |> filter(!is.na(ef_model)) |> distinct(region, subregion) |> nrow()
n_total   <- n_distinct(paste(county_map_df$region, county_map_df$subregion))
message(sprintf("County name join: %d / %d map polygons matched (%.0f%%)",
                n_matched, n_total, 100 * n_matched / n_total))


# ══════════════════════════════════════════════════════════════════════════════
# STEP 3: Shared theme and panel functions
# ══════════════════════════════════════════════════════════════════════════════

map_theme <- theme_void() %+replace% theme(
  plot.title      = element_text(size = 13, hjust = 0.5, face = "bold", margin = margin(b = 12)),
  plot.margin     = margin(2, 0, 2, 0, "mm"),
  legend.position = "bottom",
  legend.title    = element_text(size = 13),
  legend.text     = element_text(size = 13)
)

map_annotation_theme <- theme(
  plot.title      = element_text(size = 13, hjust = 0.5, face = "bold",
                                 margin = margin(b = 4, t = 6)),
  plot.caption    = element_text(size = 7.5, hjust = 0.5, color = "grey35",
                                 margin = margin(t = 6, b = 4)),
  plot.background = element_rect(fill = "white", color = NA)
)

# Avoided N2O panel: three fill categories —
#   grey      = county not in dataset
#   light blue = county already at or above pH target (avoided = 0)
#   color ramp = counties with emissions reduction opportunity
make_avd_panel <- function(df, title, scale_max) {
  no_data     <- filter(df, is.na(ef_model))
  above_tgt   <- filter(df, !is.na(ef_model) &  above_target & avoided_ktN == 0)
  opportunity <- filter(df, !is.na(ef_model) & !(above_target & avoided_ktN == 0))

  ggplot() +
    geom_polygon(data = no_data,      aes(long, lat, group = group), fill = "grey78",  color = NA) +
    geom_polygon(data = above_tgt,    aes(long, lat, group = group), fill = "#e7f0f8", color = NA) +
    geom_polygon(data = opportunity,  aes(long, lat, group = group, fill = avoided_ktN), color = NA) +
    geom_polygon(data = state_map_df, aes(long, lat, group = group),
                 fill = NA, color = "white", linewidth = 0.3) +
    scale_fill_gradientn(
      colors   = c("#fff7bc", "#fec44f", "#d95f0e", "#7f2704"),
      values   = scales::rescale(c(0, 0.25, 0.65, 1)),
      limits   = c(0, scale_max),
      oob      = scales::squish,
      na.value = "grey78",
      name     = "kt N\u2082O-N",
      guide    = guide_colorbar(barwidth = unit(9, "cm"), barheight = unit(0.8, "cm"),
                                title.position = "top", title.hjust = 0.5)
    ) +
    coord_sf(crs = st_crs("ESRI:102003"), default_crs = st_crs(4326)) +
    labs(title = title) +
    map_theme
}

# pH management share panel: fraction of combined avoided N2O from pH management —
#   grey      = county not in dataset
#   light blue = county already at or above pH target (ph_fraction = 0 or NA)
#   purple     = pH management dominates
#   green      = N reduction dominates
make_share_panel <- function(df, title) {
  no_data     <- filter(df, is.na(ef_model))
  above_tgt   <- filter(df, !is.na(ef_model) & (is.na(ph_fraction) | ph_fraction == 0))
  opportunity <- filter(df, !is.na(ef_model) & !is.na(ph_fraction) & ph_fraction > 0)

  ggplot() +
    geom_polygon(data = no_data,      aes(long, lat, group = group), fill = "grey78",  color = NA) +
    geom_polygon(data = above_tgt,    aes(long, lat, group = group), fill = "#e7f0f8", color = NA) +
    geom_polygon(data = opportunity,  aes(long, lat, group = group, fill = ph_fraction), color = NA) +
    geom_polygon(data = state_map_df, aes(long, lat, group = group),
                 fill = NA, color = "white", linewidth = 0.3) +
    scale_fill_gradient2(
      low = "#1b7837", mid = "#f7f7f7", high = "#762a83", midpoint = 0.5,
      limits = c(0, 1), name = "pH management share", labels = scales::percent,
      guide  = guide_colorbar(barwidth = unit(9, "cm"), barheight = unit(0.8, "cm"),
                              title.position = "top", title.hjust = 0.5)
    ) +
    coord_sf(crs = st_crs("ESRI:102003"), default_crs = st_crs(4326)) +
    labs(title = title) +
    map_theme
}


# ══════════════════════════════════════════════════════════════════════════════
# STEP 4: Avoided N2O maps — pH management only (NActual scenario)
# ══════════════════════════════════════════════════════════════════════════════
# 2×2 layout: rows = pH target (Avg, Max), columns = EF model (Wang, Qiu)
# Light blue: county already at or above pH target — no liming benefit.

sub_ph_only   <- filter(joined, n_scenario == "NActual" | is.na(ef_model))
scale_max_pho <- max(sub_ph_only$avoided_ktN, na.rm = TRUE)

p_pho_avg_wang <- make_avd_panel(
  filter(sub_ph_only, (ef_model == "Wang" & target_ph == "Avg") | is.na(ef_model)),
  title = paste(ef_titles["Wang"], "\u2014", tgt_titles["Avg"]), scale_max = scale_max_pho
)
p_pho_avg_qiu <- make_avd_panel(
  filter(sub_ph_only, (ef_model == "Qiu"  & target_ph == "Avg") | is.na(ef_model)),
  title = paste(ef_titles["Qiu"],  "\u2014", tgt_titles["Avg"]), scale_max = scale_max_pho
)
p_pho_max_wang <- make_avd_panel(
  filter(sub_ph_only, (ef_model == "Wang" & target_ph == "Max") | is.na(ef_model)),
  title = paste(ef_titles["Wang"], "\u2014", tgt_titles["Max"]), scale_max = scale_max_pho
)
p_pho_max_qiu <- make_avd_panel(
  filter(sub_ph_only, (ef_model == "Qiu"  & target_ph == "Max") | is.na(ef_model)),
  title = paste(ef_titles["Qiu"],  "\u2014", tgt_titles["Max"]), scale_max = scale_max_pho
)

fig_ph_only <-
  (p_pho_avg_wang | p_pho_avg_qiu) /
  (p_pho_max_wang | p_pho_max_qiu) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title   = "County-level avoided N\u2082O \u2014 pH management only",
    caption = paste0(
      "EBK soil pH prediction model. Quadratic model: counties where liming results in a net ",
      "increase in N\u2082O (computed avoided emissions < 0) set to zero. ",
      "Light blue: county already at or above pH target \u2014 no liming benefit. ",
      "Grey: county not in dataset. Shared color scale across all panels."
    ),
    theme = map_annotation_theme
  ) &
  theme(legend.position = "bottom")

ggsave("plots/maps/fig_county_map_2x2_pH_only.png", fig_ph_only,
       width = 14, height = 10, dpi = 300, bg = "white")
message("Saved: plots/maps/fig_county_map_2x2_pH_only.png")


# ══════════════════════════════════════════════════════════════════════════════
# STEP 5: Avoided N2O maps — combined scenario (pH management + N reduction)
# ══════════════════════════════════════════════════════════════════════════════
# Same 2×2 layout as above.
# Most counties above the pH target still have some avoided emissions from the
# N-reduction component. Light blue only appears where a county is both above
# the pH target AND has negligible N surplus — combined benefit is effectively 0.

sub_combined   <- filter(joined, n_scenario == "Combined" | is.na(ef_model))
scale_max_comb <- max(sub_combined$avoided_ktN, na.rm = TRUE)

p_comb_avg_wang <- make_avd_panel(
  filter(sub_combined, (ef_model == "Wang" & target_ph == "Avg") | is.na(ef_model)),
  title = paste(ef_titles["Wang"], "\u2014", tgt_titles["Avg"]), scale_max = scale_max_comb
)
p_comb_avg_qiu <- make_avd_panel(
  filter(sub_combined, (ef_model == "Qiu"  & target_ph == "Avg") | is.na(ef_model)),
  title = paste(ef_titles["Qiu"],  "\u2014", tgt_titles["Avg"]), scale_max = scale_max_comb
)
p_comb_max_wang <- make_avd_panel(
  filter(sub_combined, (ef_model == "Wang" & target_ph == "Max") | is.na(ef_model)),
  title = paste(ef_titles["Wang"], "\u2014", tgt_titles["Max"]), scale_max = scale_max_comb
)
p_comb_max_qiu <- make_avd_panel(
  filter(sub_combined, (ef_model == "Qiu"  & target_ph == "Max") | is.na(ef_model)),
  title = paste(ef_titles["Qiu"],  "\u2014", tgt_titles["Max"]), scale_max = scale_max_comb
)

fig_combined <-
  (p_comb_avg_wang | p_comb_avg_qiu) /
  (p_comb_max_wang | p_comb_max_qiu) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title   = "County-level avoided N\u2082O \u2014 pH management + N reduction",
    caption = paste0(
      "EBK soil pH prediction model. Quadratic model: counties where liming results in a net ",
      "increase in N\u2082O (computed avoided emissions < 0) set to zero. ",
      "Light blue: county already at or above pH target and N surplus is negligible \u2014 ",
      "combined benefit is effectively zero. ",
      "Grey: county not in dataset. Shared color scale across all panels."
    ),
    theme = map_annotation_theme
  ) &
  theme(legend.position = "bottom")

ggsave("plots/maps/fig_county_map_2x2_combined.png", fig_combined,
       width = 14, height = 10, dpi = 300, bg = "white")
message("Saved: plots/maps/fig_county_map_2x2_combined.png")


# ══════════════════════════════════════════════════════════════════════════════
# STEP 6: Soil pH gap maps
# ══════════════════════════════════════════════════════════════════════════════
# pH gap = recommended target − current predicted pH
#   Positive (red):  county is below target, liming would be beneficial
#   Negative (blue): county already meets or exceeds the target
#
# 2×2 layout: rows = pH target (Avg, Max), columns = pH model (RF, EBK)
# Color scale is symmetric around 0 and shared across all four panels.
#
# All four gap columns are precomputed here so the panel-building code below
# can reference them directly by name.

ph_gap_dat <- dat |>
  distinct(State, County, MeanRF, MeanEBK, pH_Avg, pH_Max) |>
  mutate(
    region        = clean_name(State),
    subregion     = clean_name(County),
    gap_RF_Avg    = pH_Avg - MeanRF,
    gap_EBK_Avg   = pH_Avg - MeanEBK,
    gap_RF_Max    = pH_Max - MeanRF,
    gap_EBK_Max   = pH_Max - MeanEBK
  )

ph_gap_joined <- county_map_df |>
  left_join(ph_gap_dat, by = c("region", "subregion"), relationship = "many-to-many")

# Symmetric color scale: limits set to ± the largest gap across all four panels
gap_lims <- c(-1, 1) * max(abs(c(ph_gap_dat$gap_RF_Avg, ph_gap_dat$gap_EBK_Avg,
                                  ph_gap_dat$gap_RF_Max, ph_gap_dat$gap_EBK_Max)),
                            na.rm = TRUE)

make_phgap_panel <- function(df, gap_col, title, lims) {
  ggplot(df, aes(long, lat, group = group, fill = .data[[gap_col]])) +
    geom_polygon(color = NA) +
    geom_polygon(data = state_map_df, aes(long, lat, group = group),
                 fill = NA, color = "white", linewidth = 0.3, inherit.aes = FALSE) +
    scale_fill_gradient2(
      low = "#4393c3", mid = "#f7f7f7", high = "#d6604d", midpoint = 0,
      limits = lims, oob = scales::squish, na.value = "grey78",
      name  = "pH gap (target \u2212 current)",
      guide = guide_colorbar(barwidth = unit(9, "cm"), barheight = unit(0.8, "cm"),
                             title.position = "top", title.hjust = 0.5)
    ) +
    coord_sf(crs = st_crs("ESRI:102003"), default_crs = st_crs(4326)) +
    labs(title = title) +
    map_theme
}

p_gap_rf_avg  <- make_phgap_panel(ph_gap_joined, "gap_RF_Avg",
                                   paste(ph_mod_titles["RF"],  "\u2014", tgt_titles["Avg"]), gap_lims)
p_gap_ebk_avg <- make_phgap_panel(ph_gap_joined, "gap_EBK_Avg",
                                   paste(ph_mod_titles["EBK"], "\u2014", tgt_titles["Avg"]), gap_lims)
p_gap_rf_max  <- make_phgap_panel(ph_gap_joined, "gap_RF_Max",
                                   paste(ph_mod_titles["RF"],  "\u2014", tgt_titles["Max"]), gap_lims)
p_gap_ebk_max <- make_phgap_panel(ph_gap_joined, "gap_EBK_Max",
                                   paste(ph_mod_titles["EBK"], "\u2014", tgt_titles["Max"]), gap_lims)

fig_phgap <-
  (p_gap_rf_avg  | p_gap_ebk_avg) /
  (p_gap_rf_max  | p_gap_ebk_max) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title   = "Soil pH gap (recommended pH \u2212 current pH)",
    caption = paste0(
      "Red: county pH below target \u2014 liming needed. ",
      "Blue: already at or above target. Grey: county not in dataset. ",
      "Shared scale across all panels."
    ),
    theme = map_annotation_theme
  ) &
  theme(legend.position = "bottom")

ggsave("plots/maps/fig_phgap_map_2x2.png", fig_phgap,
       width = 14, height = 10, dpi = 300, bg = "white")
message("Saved: plots/maps/fig_phgap_map_2x2.png")


# ══════════════════════════════════════════════════════════════════════════════
# STEP 7: pH management share maps
# ══════════════════════════════════════════════════════════════════════════════
# pH management share = NActual avoided / Combined avoided
# Answers: of the total combined benefit, how much comes from liming vs N reduction?
#   Purple (~1): pH management drives most of the benefit
#   Green  (~0): N reduction drives most of the benefit
#
# 2×2 layout: rows = pH target (Avg, Max), columns = EF model (Wang, Qiu)

ph_share <- county_avoided |>
  filter(n_scenario %in% c("NActual", "Combined")) |>
  pivot_wider(names_from = n_scenario, values_from = avoided_ktN) |>
  mutate(
    # Fraction of combined benefit attributable to pH management alone.
    # Set to NA where combined avoided = 0 (avoids division by zero).
    ph_fraction = if_else(Combined > 0, NActual / Combined, NA_real_),
    region      = clean_name(State),
    subregion   = clean_name(County)
  )

ph_share_joined <- county_map_df |>
  left_join(ph_share, by = c("region", "subregion"), relationship = "many-to-many")

p_share_avg_wang <- make_share_panel(
  filter(ph_share_joined, (ef_model == "Wang" & target_ph == "Avg") | is.na(ef_model)),
  title = paste(ef_titles["Wang"], "\u2014", tgt_titles["Avg"])
)
p_share_avg_qiu <- make_share_panel(
  filter(ph_share_joined, (ef_model == "Qiu"  & target_ph == "Avg") | is.na(ef_model)),
  title = paste(ef_titles["Qiu"],  "\u2014", tgt_titles["Avg"])
)
p_share_max_wang <- make_share_panel(
  filter(ph_share_joined, (ef_model == "Wang" & target_ph == "Max") | is.na(ef_model)),
  title = paste(ef_titles["Wang"], "\u2014", tgt_titles["Max"])
)
p_share_max_qiu <- make_share_panel(
  filter(ph_share_joined, (ef_model == "Qiu"  & target_ph == "Max") | is.na(ef_model)),
  title = paste(ef_titles["Qiu"],  "\u2014", tgt_titles["Max"])
)

fig_ph_share <-
  (p_share_avg_wang | p_share_avg_qiu) /
  (p_share_max_wang | p_share_max_qiu) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title   = "pH management share of combined avoided N\u2082O",
    caption = paste0(
      "EBK soil pH prediction model. Combined scenario (pH management + N reduction). ",
      "Purple: pH management dominates. Green: N reduction dominates. ",
      "Light blue: county already at or above pH target. Grey: county not in dataset. ",
      "Share = pH-management-only avoided / combined avoided."
    ),
    theme = map_annotation_theme
  ) &
  theme(legend.position = "bottom")

ggsave("plots/maps/fig_ph_share_map_2x2.png", fig_ph_share,
       width = 14, height = 10, dpi = 300, bg = "white")
message("Saved: plots/maps/fig_ph_share_map_2x2.png")

message("\nDone.")
