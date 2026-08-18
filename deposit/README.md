# Anonymized microdata — Biodiversity Finance Preferences (Sweden)

**Files**
- `biodivfinance_microdata_anon.csv` — comma-separated, UTF-8
- `biodivfinance_microdata_anon.sav` — SPSS format

**Sample:** n = 2,101 Swedish adults, nationally representative quota sample (Lysio panel), February 2025.

**Content:** DCE choice responses (8 tasks per respondent, 2 alternatives + opt-out), post-choice Likert scales (environmental concern, state trust, interpersonal trust, donation behaviour, policy consequentiality), and basic demographics (age cohort, gender, region, education, income, household size, party vote intention).

**Variables excluded from deposit**
- `pid` — panel provider identifier, links back to individual panel records
- `q17` — free-text open-ended responses (re-identification risk); coded subset available from the corresponding author on request

**Ethics:** Etikprövningsmyndigheten, approval ID 2025-04420-01.

**Citation:** See manuscript for full reference. Code to reproduce all analyses: see `analysis/` directory.

**Preparation script:** `analysis/prepare_deposit_data.R`
