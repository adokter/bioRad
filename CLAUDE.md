# Project Overview
[bioRad]: [R Package for Biological Analysis and Visualization of Weather Radar Data]

# Key Commands
Run R commands via `Rscript -e "..."` in the terminal:
- Load package: `Rscript -e "devtools::load_all()"`
- Run all tests: `Rscript -e "devtools::test()"`
- Run single test file: `Rscript -e "testthat::test_file('tests/testthat/test-NAME.R')"`
- Update docs & NAMESPACE: `Rscript -e "devtools::document()"`
- Full R CMD check: `Rscript -e "devtools::check()"`
- Format code: `Rscript -e "styler::style_pkg()"`

# Code Style & API Conventions
- Follow the tidyverse style guide: 2-space indentation, `snake_case` for functions/variables, and `<-` for assignment.
- Use the native pipe `|>` rather than `%>%`.
- Do not add explanatory inline comments unless clarifying non-obvious edge cases.
- Use `cli::cli_abort()` for user-facing errors and `rlang::arg_match()` for validating string arguments.
- Explicitly namespace external functions (`pkg::fun()`) inside `R/` files instead of adding `@import` tags, except for core utilities like `rlang` or `cli`.
- Keep functions small, pure where possible, and focused on a single task.

# Documentation (roxygen2)
- Document all exported functions using `roxygen2` comments (`#'`).
- Always include `@param`, `@return`, and runnable `@examples` for exported functions.
- Never edit `NAMESPACE` or `man/*.Rd` files manually; always regenerate them with `devtools::document()`.

# Testing Guardrails (testthat 3e)
- Place unit tests in `tests/testthat/test-<source-file-name>.R`.
- Each new function exposed to the user should have its own new R file, has `<source-file-name>.R`
- Never delete, skip (`skip()`), or weaken existing failing tests to make a suite pass.
- Avoid excessive mocking; test real inputs, edge cases, and expected error classes (`expect_snapshot(error = TRUE)`).

# Architecture & Dependencies
- `R/`: Core package logic. Group helpers in `R/utils.R`.
- `inst/`: Static files bundled with the package.
- `mistnet/`: C++ function for linking to libtorch for the mistnet pytorch segmentation model.
- `src/`: C libraries, libhlhdf, librave, libiris2odim and librsl are external dependencies, only libvol2bird contains code specific to this r package.
- `src/includes`: includes the header files for the C libraries
- `tests/`: includes unit tests for testthat
- `tools/`: includes helper functions for finding installed dependencies, called as part of the C build system
- `vignettes/`: include a vol2birdR vignette
- Check `DESCRIPTION` before using a new package; do not introduce heavy dependencies without asking first.
