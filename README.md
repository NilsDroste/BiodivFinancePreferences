# Citizens prefer mandatory over voluntary biodiversity financing even under favorable conditions

**Nils Droste, Jens Marl Christiansen, and Yohei Mitani**

Replication materials for the paper submitted to *Nature Ecology & Evolution*.

Pre-registered at AsPredicted: [#273748](https://aspredicted.org/im78mi.pdf) (February 2026) and [#287153](https://aspredicted.org/zs4y9e.pdf) (April 2026).

Ethics approval: Etikprövningsmyndigheten (Swedish Ethical Review Authority), ID 2025-04420-01. All participants gave informed consent.

---

## Repository structure

```
deposit/         Anonymized respondent-level microdata and the fitted mixed logit
analysis/        R scripts for all analyses and figures reported in the paper
design/          DCE design matrix and the participant information sheet
paper/           Quarto sources for the manuscript and supplementary information
preregistration/ The two AsPredicted registrations
```

## Data

The anonymized microdata are in `deposit/`, in CSV and SPSS format, with a
codebook note in `deposit/README.md`. **Every script and both Quarto documents
read from this file**, so the whole package runs as supplied, with no access to
restricted material required.

Three variables are withheld to preclude re-identification: the panel provider
identifier, the open-ended responses (Q17), and the postal-code item. None enters
any reported analysis. Survey completion times are included, since the
pre-registered speeder-exclusion check depends on them. Coded Q17 responses are
available from the corresponding author (nd@ifro.ku.dk) on request.

`analysis/prepare_deposit_data.R` documents how the deposit is derived from the
restricted raw file. It is included for transparency and is the one script that
cannot be run from this package.

## Software

| Component | Version used |
|---|---|
| R | 4.6.1 |
| Quarto | 1.9.37 |
| logitr | 1.2.0 |
| tidyverse | 2.0.0 |
| kableExtra | 1.4.1 |
| sandwich | 3.1-1 |
| lmtest | 0.9-40 |

This repository uses [renv](https://rstudio.github.io/renv/) to lock all 148 R package versions. To restore the exact environment:

```r
install.packages("renv")
renv::restore()
```

This will install all packages at the versions recorded in `renv.lock`. Requires R 4.6+ and an internet connection.

The Quarto manuscript requires a LaTeX distribution. TeX Live 2024 or later is recommended:

- macOS: `brew install --cask mactex-no-gui`
- Linux: `apt install texlive-full`
- Windows: install [MiKTeX](https://miktex.org/)

## Analysis scripts

Every script reads the anonymized deposit in `deposit/`, so the whole package
runs without access to restricted files.

| Script | Content |
|---|---|
| `analysis/prepare_deposit_data.R` | Builds the anonymized deposit from the restricted raw file: drops the panel identifier and open-ended Q17, merges survey completion times. **Requires restricted data; not runnable from the public package.** |
| `analysis/mxl_analysis.R` | Builds the choice database and estimates the reported mixed logit via `analysis/_mxl_runner.R` (logitr, 500 Sobol draws, 10 random starts, seed 42) |
| `analysis/_mxl_runner.R` | Subprocess invoked by `mxl_analysis.R`; writes `MXL_full_logitr.rds` |
| `analysis/instrument_pref_analysis.R` | Voluntary preference score, fractional logit (Papke-Wooldridge), 2x2 descriptives |
| `analysis/heterogeneity_analysis.R` | Pre-registered subgroup CL models by gender, income, environmental attitude |
| `analysis/heterogeneity_exploratory.R` | Exploratory subgroup CL models by party vote, state trust, policy consequentiality |
| `analysis/donation_analysis.R` | Donation experiment distribution and descriptives |
| `analysis/fetch_bestcase_data.R` | Fetches the Figure 1 country data from primary sources (Integrated Values Surveys via Our World in Data; Eurostat) into `analysis/bestcase_data.csv` |
| `analysis/bestcase_map.R` | Builds Figure 1 from that cached data |

The scripts are independent; none depends on objects created by another. Figure 1
requires `fetch_bestcase_data.R` to have been run at least once (its output is
cached in the repository). Data provenance for Figure 1 is documented in
`analysis/bestcase_data_verification.md`.

## Manuscript

```bash
quarto render paper/manuscript_NEE.qmd      --to pdf   # main manuscript
quarto render paper/manuscript_NEE_anon.qmd --to pdf   # anonymized version
quarto render paper/supplementary_NEE.qmd   --to pdf   # supplementary information
```

Requires a LaTeX distribution (TeX Live 2024+ recommended) and the R packages
above. The supplementary information re-estimates several choice models at
render time and takes a few minutes.

## License

Code: MIT. Data: CC BY 4.0 (upon deposit).
