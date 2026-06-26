# Cross-Fitted Survey-Weighted TMLE — results companion

An interactive companion website for the simulation and NHANES results behind
*Cross-Fitted Survey-Weighted TMLE with Design-Based Variance for Causal Machine
Learning* (M. Ehsan Karim). The manuscript and Web Appendix are the source of
record; this site is a convenience layer whose numbers are frozen to match them.

A **static [Quarto](https://quarto.org) website** (no server, free to host on
GitHub Pages). All numbers are pre-computed and frozen; every plot/table is
rebuilt from the CSVs in `data/`. Replaces the old Shiny teaching app at
`ehsank.shinyapps.io/survey-TMLE/`.

## Structure

```
_quarto.yml      site config (navbar, cosmo theme, output-dir: docs)
index.qmd        landing: thesis + three message cards
simulation.qmd   interactive coverage ladder (plotly) + filterable table (DT) + diagnostics
nhanes.qmd       per-question forest plots (plotly) + results table (DT)
methods.qmd      one-page plain-language method recap
about.qmd        citation, code/data, license, contact
R/load_data.R    shared loaders, factor levels, colour palette
data/            frozen CSV snapshots (see "Data" below)
docs/            rendered site (build output; committed for GitHub Pages)
```

## Build

Requires Quarto and R with: `ggplot2`, `plotly`, `DT`.

```bash
quarto render        # writes the site into docs/
quarto preview       # live local preview while editing
```

## Deploy (GitHub Pages, free)

1. `git init && git add -A && git commit -m "initial showcase"` (already done if
   you cloned this folder with history).
2. Create a GitHub repo and push.
3. Repo **Settings -> Pages -> Build and deployment -> Deploy from a branch ->
   `master` / `docs`**. The site goes live at `https://<user>.github.io/<repo>/`.
4. (Optional) Add a GitHub Action (`quarto-dev/quarto-actions`) to rebuild on
   every push instead of committing `docs/`.

## Data

`data/` holds ~30 frozen CSV snapshots of the manuscript result files, copied
byte-for-byte from the paper repo's `results/`, `results/arc/`, and
`Nhanes/results/` directories (simulation `sim_full_*` and `R0x`–`R22`/`A21`;
NHANES `R05`/`R06`/`R09`/`R15`/`R17`; per-example diagnostics). The NHANES forest
and five-arm table use the **m = 40 multiple-imputation** results
(`R06_mi_summary.csv`), matching the paper's Table 3 and Figure 2; the
over-adjustment sensitivity tables (`R09_sensitivity_*`) are single-imputation,
matching Web Appendix F. To refresh, re-copy the CSVs from the paper repo and
re-render.

## Status

Complete and synced to the manuscript: the data match the paper exactly (m = 40
multiple imputation; paper terminology), every view is labelled with its
main-text Table/Figure or Web Table location, and `docs/` is the rendered site
served from GitHub Pages.
