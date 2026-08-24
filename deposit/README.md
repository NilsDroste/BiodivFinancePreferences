# Anonymized microdata — Biodiversity Finance Preferences (Sweden)

**Files**
- `biodivfinance_microdata_anon.csv` — comma-separated, UTF-8
- `biodivfinance_microdata_anon.sav` — SPSS format

**Sample:** n = 2,101 Swedish adults from an online panel (Lysio Research), quota-balanced to approximate the Swedish adult population by age, gender and region; no post-hoc weighting. Fielded in two waves: pilot 24 March 2026 (n = 122, of which 119 appear here) and full launch 20 April to 6 May 2026.

**Content:** DCE choice responses (8 tasks per respondent, 2 alternatives + opt-out), post-choice Likert scales (environmental concern, state trust, interpersonal trust, donation behaviour, policy consequentiality), and basic demographics (age cohort, gender, region, education, income, household size, party vote intention).

**Variables excluded from deposit**
- `pid` — panel provider identifier, links back to individual panel records
- `q17` — free-text open-ended responses (re-identification risk); coded subset available from the corresponding author on request

**Added variable:** `total_time_seconds` gives survey completion time, merged from the provider's separate timing file (matched for 2,091 of 2,101 respondents). It supports the preregistered speeder-exclusion check.

**Ethics:** Etikprövningsmyndigheten, approval ID 2025-04420-01.

**Citation:** See manuscript for full reference. Code to reproduce all analyses: see `analysis/` directory.

**Preparation script:** `analysis/prepare_deposit_data.R`
