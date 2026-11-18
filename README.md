# EPL Value Finder — Player Recruitment & Valuation App

**Live Application:** [Launch Interactive Shiny App](https://aryangandhi.shinyapps.io/Aryan_Gandhi_Final_Project/)

## Overview
An interactive R Shiny web application designed to identify undervalued Premier League (EPL) players by benchmarking on-pitch performance percentiles against current market valuations. Built to help recruitment analysts and decision-makers spot cost-versus-performance anomalies across positions.

## Methodology & Features
* **Data Collection & ETL:** Scraped, cleaned, and structured 500+ historical Premier League player records into a unified dataset, resolving schema inconsistencies and missing valuation values.
* **Quantitative Valuation Model:** Calculated position-adjusted performance percentile distributions across key attacking, progression, and defensive metrics to flag mispriced assets.
* **Interactive Scouting & Stress-Testing:** Built dynamic `ggplot2` radar charts and filtering controls allowing users to compare player profiles, isolate market inefficiencies, and evaluate roster fit against transfer budgets.

## Tech Stack
* **Application & Visualization:** R (`Shiny`, `ggplot2`, `dplyr`)
* **Data Scraping & Cleaning:** Python (`pandas`, web scraping libraries)
* **Deployment:** shinyapps.io
