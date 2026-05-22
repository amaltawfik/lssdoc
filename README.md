
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lssdoc: Word review documents from LimeSurvey `.lss` files

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project Status:
WIP](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/amaltawfik/lssdoc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/amaltawfik/lssdoc/actions/workflows/R-CMD-check.yaml)
[![r-universe](https://amaltawfik.r-universe.dev/badges/lssdoc)](https://amaltawfik.r-universe.dev/lssdoc)
[![MIT
License](https://img.shields.io/badge/license-MIT-blue.svg?style=flat)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

The goal of **lssdoc** is to generate a professional Word (`.docx`)
review document from a LimeSurvey `.lss` export, displaying up to four
languages side by side. It is built for the human review of multilingual
questionnaires by ethics committees, translators, and methodologists: it
reproduces the questionnaire content faithfully and flags automatically
detectable anomalies that do not require any AI.

Processing is fully local. No questionnaire is uploaded to any
third-party service.

## Installation

You can install the development version of lssdoc from
[GitHub](https://github.com/amaltawfik/lssdoc) with:

``` r
# install.packages("pak")
pak::pak("amaltawfik/lssdoc")

# or, with remotes:
# install.packages("remotes")
# remotes::install_github("amaltawfik/lssdoc")
```

## Usage

``` r
library(lssdoc)

# Simple pipeline: .lss -> .docx
lss_to_docx("monquestionnaire.lss", "rapport.docx")

# With finer control
lss <- parse_lss("monquestionnaire.lss")
audit <- audit_lss(lss)
print(audit) # show detected anomalies

render_lss_docx(
  lss,
  output     = "rapport.docx",
  languages  = c("fr", "de", "en", "it"),
  layout     = "auto",
  show_audit = TRUE
)
```

> **Note**: the package is at an early scaffolding stage. The API above
> is defined and documented; the implementations are in progress.
