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

- R ≥ 4.6
- Packages: `tidyverse`, `readxl`, `here`, `apollo` (≥ 0.3.8), `logitr`, `kableExtra`, `patchwork`, `sandwich`, `lmtest`, `scales`, `janitor`

Install all at once:

```r
install.packages(c(
  "tidyverse", "readxl", "here", "apollo", "logitr",
  "kableExtra", "patchwork", "sandwich", "lmtest",
  "scales", "janitor"
))
```

## Analysis scripts

| Script | Content |
|---|---|
| `analysis/analysis.R` | Main analysis: data preparation, CL, MXL (via logitr), VP score, fractional probit, 2×2 descriptives, subgroup heterogeneity |
| `analysis/mxl_analysis.R` | Mixed logit estimation (Apollo) |
| `analysis/donation_analysis.R` | Dictator game donation distribution and descriptives |
| `analysis/heterogeneity_analysis.R` | Subgroup CL models by gender, income, environmental attitude |
| `analysis/instrument_pref_analysis.R` | Instrument-level WTP calculations |
| `analysis/lcm_analysis.R` | 2-class latent class MNL via Apollo (Appendix H) |

Run in the order listed. `analysis.R` must run first as it produces objects used downstream.

## Manuscript

The Quarto source (`paper/manuscript_GEC.qmd`) renders to PDF with:

```bash
quarto render paper/manuscript_GEC.qmd --to pdf
```

Requires a LaTeX distribution (TeX Live 2024+ recommended) and all R packages above.

## License

Code: MIT. Data: CC BY 4.0 (upon deposit).
