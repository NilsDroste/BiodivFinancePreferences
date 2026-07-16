# Mandatory by Popular Demand: Biodiversity Finance Preferences in Sweden

**Nils Droste, Jens Christiansen, and Yohei Mitani**

Replication materials for the paper submitted to *Global Environmental Change*.

Pre-registered at AsPredicted: [#273748](https://aspredicted.org/im78mi.pdf) (February 2026) and [#287153](https://aspredicted.org/zs4y9e.pdf) (April 2026).

Ethics approval: Etikprövningsmyndigheten, ID 2025-04420-01.

---

## Repository structure

```
analysis/        R scripts for all analyses reported in the paper
design/          DCE attribute-level design matrix (Excel)
paper/           Quarto manuscript source and bibliography
```

## Data

Survey microdata are available as an anonymized dataset via [REPOSITORY DOI — to be added upon acceptance]. Open-ended responses (Q17) are excluded from the public deposit to protect respondent anonymity; the coded subset used for qualitative triangulation in Section 6.2 is available on request from the corresponding author (nd@ifro.ku.dk).

To run the analysis scripts, place the data file at:

```
data/Full launch database/4178_excel_databas.xlsx
```

## Software

| Component | Version used |
|---|---|
| R | 4.6.1 |
| Quarto | 1.9.37 |
| logitr | 1.2.0 |
| apollo | 0.3.8 |
| tidyverse | 2.0.0 |
| kableExtra | 1.4.1 |
| sandwich | 3.1-1 |
| lmtest | 0.9-40 |

This repository uses [renv](https://rstudio.github.io/renv/) to lock all 148 R package versions. To restore the exact environment:

```r
install.packages("renv")
renv::restore()
```

This will install all packages at the versions recorded in `renv.lock`. Requires R 4.6+ and an internet connection; Apollo is installed from CRAN.

The Quarto manuscript requires a LaTeX distribution. TeX Live 2024 or later is recommended:

- macOS: `brew install --cask mactex-no-gui`
- Linux: `apt install texlive-full`
- Windows: install [MiKTeX](https://miktex.org/)

## Analysis scripts

| Script | Content |
|---|---|
| `analysis/analysis.R` | Main analysis: data preparation, CL, MXL (via logitr), VP score, fractional logit, 2×2 descriptives, subgroup heterogeneity |
| `analysis/mxl_analysis.R` | Mixed logit estimation (Apollo) |
| `analysis/donation_analysis.R` | Dictator game donation distribution and descriptives |
| `analysis/heterogeneity_analysis.R` | Subgroup CL models by gender, income, environmental attitude |
| `analysis/instrument_pref_analysis.R` | Instrument-level WTP calculations |
| `analysis/lcm_analysis.R` | 2-class latent class MNL via Apollo (Appendix H) |
| `analysis/lcm_nclass.R` | 3- and 4-class LC-MNL specification tests; model fit comparison (Appendix H) |
| `analysis/heterogeneity_exploratory.R` | Exploratory subgroup CL models by party vote, state trust, policy consequentiality (Appendix I) |

Run in the order listed. `analysis.R` must run first as it produces objects used downstream.

## Manuscript

The Quarto source (`paper/manuscript_GEC.qmd`) renders to PDF with:

```bash
quarto render paper/manuscript_GEC.qmd --to pdf
```

Requires a LaTeX distribution (TeX Live 2024+ recommended) and all R packages above.

## License

Code: MIT. Data: CC BY 4.0 (upon deposit).
