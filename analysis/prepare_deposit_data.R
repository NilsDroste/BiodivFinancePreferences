library(tidyverse)
library(readxl)
library(haven)
library(here)

# ==============================================================================
# Prepare anonymized microdata for public deposit
#
# Exclusions for re-identification protection:
#   - pid: panel provider identifier, links back to individual panel records
#   - q17: free-text open-ended responses, re-identification risk
#
# Added:
#   - total_time_seconds: survey completion time, merged from the provider's
#     separate timing file on pid before pid is dropped. Needed for the
#     preregistered speeder-exclusion robustness check (AsPredicted #287153),
#     which is otherwise not reproducible from the public deposit. Completion
#     time alone carries no re-identification risk.
#
# All remaining variables are Likert scales, choice indicators, donation amount,
# and basic demographics from a quota sample of n = 2,101 Swedish adults,
# balanced on age, gender and region. Re-identification risk is negligible.
#
# Ethics approval: Etikprövningsmyndigheten, ID 2025-04420-01
# Verify that this deposit scope is consistent with the approval before release.
# ==============================================================================

raw <- read_excel(
  here("data", "Full launch database", "4178_excel_databas.xlsx"),
  sheet = 2
)

# Completion times arrive in a separate file keyed on pid.
timing <- read_excel(here("data", "4178_time.xlsx"))
names(timing) <- c("pid", "total_time_seconds")

cat("Raw dimensions:", nrow(raw), "x", ncol(raw), "\n")

deposit <- raw |>
  mutate(pid = as.character(pid)) |>
  left_join(timing |> mutate(pid = as.character(pid)), by = "pid") |>
  select(-pid, -q17)

cat("Timing matched for", sum(!is.na(deposit$total_time_seconds)), "of",
    nrow(deposit), "respondents\n")
cat("Variables dropped: pid, q17; added: total_time_seconds\n")

cat("Deposit dimensions:", nrow(deposit), "x", ncol(deposit), "\n")
cat("Variables retained:", paste(names(deposit), collapse = ", "), "\n")

# Save as CSV (universally readable) and SPSS for compatibility
write_csv(deposit, here("deposit", "biodivfinance_microdata_anon.csv"))
write_sav(deposit, here("deposit", "biodivfinance_microdata_anon.sav"))
cat("\nSaved to deposit/biodivfinance_microdata_anon.csv and .sav\n")
