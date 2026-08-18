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
# All remaining variables are Likert scales, choice indicators, donation amount,
# and basic demographics collected for a nationally representative sample of
# n = 2,101 Swedish adults. Re-identification risk is negligible.
#
# Ethics approval: Etikprövningsmyndigheten, ID 2025-04420-01
# Verify that this deposit scope is consistent with the approval before release.
# ==============================================================================

raw <- read_excel(
  here("data", "Full launch database", "4178_excel_databas.xlsx"),
  sheet = 2
)

cat("Raw dimensions:", nrow(raw), "x", ncol(raw), "\n")
cat("Variables dropped: pid, q17\n")

deposit <- raw |>
  select(-pid, -q17)

cat("Deposit dimensions:", nrow(deposit), "x", ncol(deposit), "\n")
cat("Variables retained:", paste(names(deposit), collapse = ", "), "\n")

# Save as CSV (universally readable) and SPSS for compatibility
write_csv(deposit, here("deposit", "biodivfinance_microdata_anon.csv"))
write_sav(deposit, here("deposit", "biodivfinance_microdata_anon.sav"))
cat("\nSaved to deposit/biodivfinance_microdata_anon.csv and .sav\n")
