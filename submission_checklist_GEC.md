# GEC Submission Checklist — Mandatory by Popular Demand

## Before submitting

- [ ] Confirm CRediT roles with Jens and Yohei (`paper/title_page_GEC.md`)
- [ ] Add acknowledgements and grant numbers to `paper/title_page_GEC.md`
- [ ] Add full postal address for corresponding author (Nils, IFRO) to `paper/title_page_GEC.md`
- [ ] Confirm AI declaration wording acceptable to all co-authors (in `manuscript_GEC_anon.qmd`)
- [ ] Verify DOIs for Cashore2002 and BernsteinCashore2007 in `paper/references.bib`

## OSF anonymous link (for double-blind review)

GEC uses double-blind review. The anonymized manuscript needs an anonymous repository link.

**Steps (browser, ~5 minutes):**

1. Go to osf.io and log in
2. Create a new project — title can be generic, e.g. "Biodiversity Finance Preferences DCE"
3. Upload files directly (do NOT use GitHub add-on — repo is private during review):
   - `analysis/` scripts
   - `design/Trial 3 - factorial grouped, svensk.xlsx`
   - `paper/manuscript_GEC.qmd` and `paper/references.bib`
   - `preregistration/` PDFs
   - `README.md`
4. Go to **Settings** → **View-only links** → **Create a view-only link**
5. Tick **Anonymize** (strips contributor names for anyone using the link)
6. Copy the link
7. Add the link to `paper/manuscript_GEC_anon.qmd` in the Data Availability section
   (replace "[REPOSITORY — to be provided upon acceptance]") and re-render

## Submission files

| File | Purpose |
|---|---|
| `paper/manuscript_GEC_anon.pdf` | Main manuscript (anonymized, no author names) |
| `paper/title_page_GEC.md` | Title page with author details, CRediT, acknowledgements |
| `paper/highlights_GEC.md` | 5 highlights (≤85 chars each) — paste into submission system |
| `paper/cover_letter_GEC.md` | Cover letter |

Submit manuscript as `.tex` (already generated as `paper/manuscript_GEC.tex`) or render to `.docx` with `quarto render paper/manuscript_GEC_anon.qmd --to docx`.

## GEC formatting requirements

- Word limit: 8,000 words (main body + captions, excluding references and appendices)
- Abstract: ≤250 words (current version: ~235 words ✓)
- Highlights: 3–5 bullet points, ≤85 characters each ✓
- Figures: submit as separate files (TIFF/EPS/PDF), min 300 dpi
- No author names/affiliations/acknowledgements in the anonymized manuscript ✓
- CRediT author contributions: on title page only ✓
- Preregistration URLs: on title page only (anonymized in manuscript) ✓

## Upon acceptance

1. Make GitHub repo public: github.com/NilsDroste/BiodivFinancePreferences
2. Deposit anonymized dataset on Zenodo (or OSF → Zenodo integration):
   - Include: anonymized survey data, analysis code, DCE design matrix
   - Exclude: Q17 open-ended responses (available on request to nd@ifro.ku.dk)
   - Note in README: "Open-ended responses (Q17) excluded to protect respondent anonymity; available on request from corresponding author"
3. Get Zenodo DOI and update:
   - `README.md` (replace placeholder)
   - `paper/manuscript_GEC.qmd` Data Availability section (replace "[REPOSITORY DOI — to be added upon acceptance]")
   - Re-render final manuscript
4. Make OSF project public and link to Zenodo DOI

## Data and ethics

- Ethics approval: Etikprövningsmyndigheten, ID 2025-04420-01
- Participant information sheet: `design/Participant information sheet, eng.pdf`
- Participants consented to online publication of anonymized data
- Q17 free-text responses excluded from public deposit (re-identification risk)
