# Gaps and Progress in the Implementation of Law 21,180 (Chile)

This repository contains a Quarto report based on the **Digital Government Indicators Study 2023 (reference year 2022)**.
It describes gaps and progress in institutional capacities, IT staff, infrastructure, Law 21,180 implementation, and data governance.

## Project structure
- `informe_gobierno_digital.qmd`: main report
- `scripts/`: data cleaning + indicators + plots
- `data/`: input dataset (see note below)
- `_site/`: rendered website output

## Data
The dataset comes from the Chilean Open Data portal.  
Download the Excel file and place it in: `data/bbdd_estudio_indicadores.xlsx`.

## How to render
In RStudio:
- Open the `.qmd` and click **Render**  
or run:
```r
quarto::quarto_render("informe_gobierno_digital.qmd")
