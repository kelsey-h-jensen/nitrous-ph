library(tidyverse)

dat <- read_csv("data/N2O_summary_data_04012026.csv", show_col_types = FALSE)

# ── Per-row avoided emissions (kg N2O-N) ──────────────────────────────────────
# One row per county × crop × EF model × pH model × pH target.
#
# pH_only:  avoided N2O from liming alone (N inputs unchanged).
#           Zero for counties already at or above the pH target.
# N_only:   avoided N2O from 50% N surplus reduction alone (no pH change).
#           All counties benefit; Qiu polynomial issue does not apply here.
# combined: avoided N2O from liming + N surplus reduction.
#           Above-target counties receive N-reduction benefit only.
#
# SD propagated assuming independence: sqrt(SD_current^2 + SD_target^2)

components <- map_dfr(c("Wang", "Qiu"), function(ef_model) {
  map_dfr(c("RF", "EBK"), function(ph_model) {
    map_dfr(c("Avg", "Max"), function(ph_target) {
      cur_actual     <- dat[[paste0("N2O_", ef_model, ph_model, "_NActual_mean")]]
      cur_reduced    <- dat[[paste0("N2O_", ef_model, ph_model, "_NReduced_mean")]]
      tgt_actual     <- dat[[paste0("N2O_", ef_model, ph_target, "_NActual_mean")]]
      tgt_reduced    <- dat[[paste0("N2O_", ef_model, ph_target, "_NReduced_mean")]]
      cur_actual_sd  <- dat[[paste0("N2O_", ef_model, ph_model, "_NActual_sd")]]
      cur_reduced_sd <- dat[[paste0("N2O_", ef_model, ph_model, "_NReduced_sd")]]
      tgt_actual_sd  <- dat[[paste0("N2O_", ef_model, ph_target, "_NActual_sd")]]
      tgt_reduced_sd <- dat[[paste0("N2O_", ef_model, ph_target, "_NReduced_sd")]]
      ph_cur <- dat[[ifelse(ph_model == "RF", "MeanRF", "MeanEBK")]]
      ph_tgt <- dat[[ifelse(ph_target == "Avg", "pH_Avg", "pH_Max")]]
      above  <- ph_cur > ph_tgt
      tibble(
        State = dat$State, County = dat$County, Crop = dat$Crop,
        ef_model, ph_model, ph_target,
        pH_only     = if_else(above, 0, cur_actual - tgt_actual),
        pH_only_sd  = if_else(above, 0, sqrt(cur_actual_sd^2 + tgt_actual_sd^2)),
        N_only      = cur_actual - cur_reduced,
        N_only_sd   = sqrt(cur_actual_sd^2 + cur_reduced_sd^2),
        combined    = if_else(above, cur_actual - cur_reduced,
                                     cur_actual - tgt_reduced),
        combined_sd = if_else(above, sqrt(cur_actual_sd^2 + cur_reduced_sd^2),
                                     sqrt(cur_actual_sd^2 + tgt_reduced_sd^2))
      )
    })
  })
})

# ── Qiu filter ────────────────────────────────────────────────────────────────
# The Qiu polynomial emission factor peaks at ~pH 5.64. In some county-crop
# combinations, liming increases N2O rather than reducing it (pH_only < 0).
# Those values are set to zero here — once — and the zeroed components are used
# for ALL downstream calculations (figures, table, uncertainty decomp).
#
# Applied only to pH_only and combined (both involve a pH change).
# N_only is unaffected — no pH change, so the Qiu polynomial issue does not apply.
# Filter is per combination: a county zeroed in Qiu RF→Avg may still have a
# positive value in Qiu RF→Max if liming across a wider range yields a net reduction.

components <- components |>
  mutate(
    pH_only_sd  = if_else(ef_model == "Qiu" & pH_only  < 0, 0, pH_only_sd),
    combined_sd = if_else(ef_model == "Qiu" & combined < 0, 0, combined_sd),
    pH_only     = if_else(ef_model == "Qiu", pmax(pH_only,  0), pH_only),
    combined    = if_else(ef_model == "Qiu", pmax(combined, 0), combined)
  )

# ── National summary ──────────────────────────────────────────────────────────
# Aggregated from per-row components. SD propagated assuming county independence:
# SD_total = sqrt(sum(SD_i^2)).
#
# N_only is identical across Avg and Max ph_target rows (no pH change involved);
# ph_target == "Max" is used as the source to avoid double-counting.

summary_total <- bind_rows(
  components |>
    group_by(ef_model, ph_model, ph_target) |>
    summarise(
      n_scenario       = "NActual",
      avoided_mean_kgN = sum(pH_only,        na.rm = TRUE),
      avoided_sd_kgN   = sqrt(sum(pH_only_sd^2, na.rm = TRUE)),
      .groups = "drop"
    ),
  components |>
    filter(ph_target == "Max") |>
    group_by(ef_model, ph_model) |>
    summarise(
      ph_target        = "None",
      n_scenario       = "NReductionOnly",
      avoided_mean_kgN = sum(N_only,        na.rm = TRUE),
      avoided_sd_kgN   = sqrt(sum(N_only_sd^2, na.rm = TRUE)),
      .groups = "drop"
    ),
  components |>
    group_by(ef_model, ph_model, ph_target) |>
    summarise(
      n_scenario       = "Combined",
      avoided_mean_kgN = sum(combined,        na.rm = TRUE),
      avoided_sd_kgN   = sqrt(sum(combined_sd^2, na.rm = TRUE)),
      .groups = "drop"
    )
) |>
  mutate(
    avoided_mean_CO2e = avoided_mean_kgN * 429,
    avoided_sd_CO2e   = avoided_sd_kgN   * 429
  )

# ── Save ──────────────────────────────────────────────────────────────────────
save(components, summary_total, file = "data/avoided_emissions_components.Rdata")
write_csv(summary_total, "data/avoided_emissions_total.csv")

message("Saved: avoided_emissions_components.Rdata")
message("Saved: avoided_emissions_total.csv")
