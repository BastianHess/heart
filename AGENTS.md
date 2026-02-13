# AGENTS.md

Project-specific guidance for Codex and other agents working in this repo.

## Project Overview
- This is an R Shiny app centered on the `heart` dataset in `data/heart.rds`.
- Main app entrypoint: `app.R`.
- Shared helpers live in `R/` (e.g., `R/helpers.R`, `R/mod_download_plot.R`).
- Static assets live in `www/`.

## Key Files
- `app.R` - UI + server for the main dashboard.
- `R/helpers.R` - small utility functions (e.g., mortality calculation).
- `R/mod_download_plot.R` - reusable download module for plots.
- `data/heart.rds` - core dataset used by the app.
- `www/` - images and other static assets.

## Conventions
- Prefer reactive expressions for data filtering and plot generation.
- Reuse `mod_download_plot_*` for plot downloads instead of re-implementing.
- Keep UI layout in `app.R` and helper logic in `R/`.
- Avoid heavy dependencies unless needed for a clear feature.

## Working Practices
- When editing, default to small, targeted changes.
- If a change impacts filters or plots, ensure it remains compatible with `filtered_data()`.
- If you introduce new UI inputs, wire them through `filtered_data()` or document why they are independent.

