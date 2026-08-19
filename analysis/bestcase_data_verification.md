# Figure 1 data provenance and verification

Verified 2026-08-18/19. Supersedes hand-entered approximations that were present
in `bestcase_map.R` until commit `cedfd0b`. Those values were labelled in-script
as "approximate; verify before publication" and, on checking, were materially
wrong. This note records what was checked, against what, and what changed.

Both series are now fetched from primary sources by
`analysis/fetch_bestcase_data.R` and cached to `analysis/bestcase_data.csv`.
No value in the figure is hand-entered.

---

## Series 1 — Interpersonal trust

**Measure.** Share agreeing "most people can be trusted" (WVS A165 / EVS
equivalent), against the alternative "you need to be very careful in dealing
with people".

**Source.** Integrated Values Surveys (2024), IVS v4 — the *joint* EVS–WVS file —
processed by Our World in Data.
Machine-readable: <https://ourworldindata.org/grapher/self-reported-trust-attitudes.csv>

**Wave / year.** Wave 7, fieldwork 2017–2022. OWID stamps all Wave 7 rows 2022.

**Coverage.** 91 countries.

**Note on the source file.** The Nordic countries appear in the *integrated*
EVS–WVS dataset, not in the standalone WVS Wave 7 release. An earlier version of
the script cited "WVS Wave 7", which was the wrong file. Belgium and South Africa
have no Wave 7 survey at all; the old script carried invented values for both.

### Corrections (old hand-entered → verified)

| Country | Old | Verified | Error |
|---|---|---|---|
| Sweden | 60 | 62.8 | +2.8 |
| Norway | 73 | 72.1 | −0.9 |
| Denmark | 74 | 73.9 | ~0 |
| Finland | 66 | 68.4 | +2.4 |
| Netherlands | 66 | 57.0 | **−9.0** |
| Spain | 27 | 41.0 | **+14.0** |
| Austria | 38 | 49.8 | **+11.8** |
| Bangladesh | 25 | 12.9 | **−12.1** |
| South Korea | 43 | 32.9 | **−10.1** |
| Ethiopia | 22 | 11.9 | **−10.1** |
| Russia | 32 | 22.9 | **−9.1** |
| Indonesia | 14 | 4.6 | **−9.4** |
| Belgium | 35 | *no Wave 7 data* | invented |
| South Africa | 23 | *no Wave 7 data* | invented |

**Sweden: 62.8%, rank 5 of 91** (behind Denmark 73.9, Norway 72.1, Finland 68.4,
China 63.5). The claim that Sweden ranks among the world's highest on
interpersonal trust is supported.

---

## Series 2 — Public expenditure on biodiversity protection

**Measure.** General government total expenditure on COFOG group **GF0504**,
"Protection of biodiversity and landscape". Converted to per capita and to USD.

**Source.** Eurostat `gov_10a_exp` (expenditure) and `tps00001` (population),
retrieved via the Eurostat REST API.

**Year.** 2023 (latest with near-complete coverage). EUR→USD at 1.082, the 2023
ECB average reference rate.

**Coverage.** 30 countries — EU27 plus Norway, Switzerland, Iceland. **No
comparable global series exists**, which is why the bivariate panel is European
and the global claim rests on the trust panel.

### Why GF0504 rather than GF05

GF05 is the whole environmental-protection division and is dominated by waste and
waste-water management, which are not biodiversity financing in the sense this
paper studies. On GF05 Sweden is mid-table (rank 15 of 34, near the EU average);
on the biodiversity-specific GF0504 it is clearly below average.

### Why absolute euro rather than the published % of GDP

Eurostat publishes GF0504 as a share of GDP rounded to 0.1pp. At that resolution
Sweden, Finland and Norway all display as "0.0", which cannot separate the low
spenders. Working from million euro and dividing by population preserves the
distinctions.

### Corrections (old hand-entered → verified)

The old script's `spending_data` was labelled "per-capita conservation
spending" with no defined source, unit year, or COFOG basis. It ranked Norway
680 > Sweden 520 > Switzerland 460 > Denmark 430 > Finland 350 > Netherlands 310.
The verified GF0504 ordering is materially different — Denmark and the
Netherlands are far higher, Sweden far lower:

| Country | Old (USD/cap) | Verified GF0504 (USD/cap, 2023) |
|---|---|---|
| Denmark | 430 | 126 |
| Netherlands | 310 | 96 |
| Norway | 680 | 39 |
| Finland | 350 | 27 |
| **Sweden** | **520** | **16** |

**Sweden: USD 16/capita, rank 22 of 30** (18 of the 26 countries that also have
trust data). Roughly one-eighth of Denmark's and one-sixth of the Netherlands'.

**Stability.** Not a single-year artefact. EUR per capita 2018–2024:
Sweden 20.0, 14.7, 14.0, 23.9, 21.5, 15.2, 17.3 — against Denmark 96–123 and
the Netherlands 56–98 across the same period.

**Assumption, checked with the author.** GF0504 measures *government
expenditure*. Sweden also protects forest biodiversity through regulation and
through voluntary set-asides by forest owners, neither of which books as
government spending, so a low figure could in principle reflect a different
delivery model rather than lower effort. NDroste (domain expert, Nordic forest
governance) confirmed 2026-08-19 that Sweden has neither a high protected-area
share nor a proactive government stance on forest biodiversity, and that the
spending figure does reflect a genuine lack of effort.

---

## Series dropped — institutional trust

The old Figure 1 used "confidence in the government" (WVS Q71) as the second
axis. It was checked and also found materially wrong (Finland scripted 72 vs
41.6 actual; Denmark 69 vs 39.2; Australia 54 vs 30.2; Sweden 68 vs 50.7).

On verified values **Sweden ranks 14 of 40 and falls out of the high–high cell
entirely**; that cell contains Norway, China, Switzerland and New Zealand. The
old figure's central visual claim was therefore false.

The axis was dropped rather than corrected, for a substantive reason as well as
an empirical one: high institutional trust makes *mandatory* instruments more
attractive, so it does not belong on a figure establishing favourable conditions
for *voluntary* provision. Verified values, had they been retained
(% "a great deal" + "quite a lot", IVS v4 / OWID, cross-checked against King's
College London Policy Institute's independent tabulation of the same item,
agreement within ~1pp on single-survey countries): Sweden 50.7, Norway 59.3,
Denmark 39.2, Finland 41.6, Switzerland 66.3, China 94.6.

---

## Resulting position of the case country

| | Value | Rank |
|---|---|---|
| Interpersonal trust | 62.8% | 5 of 91, worldwide |
| Biodiversity spending | USD 16/capita | 22 of 30, Europe |
| Bivariate cell | 3-1 (high trust, low spending) | shared only with Austria, at much lower trust (49.8) |
