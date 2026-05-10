# COFINFAD Lab: Interactive Shiny App for Colombian Fintech Analysis

This project was developed as part of ISSS608 — Visual Analytics and Applications coursework for the Master of IT in Business programme at Singapore Management University.

🔗 [Live App](https://isss608-vs.shinyapps.io/COFINFAD_LAB/)  
🔗 [Project Website](https://cofinfad-lab.netlify.app/)

**COFINFAD Lab** is an interactive data exploration and clustering project built in **R Shiny** to analyse customer behaviour in a Colombian fintech dataset. The app enables users to explore demographic, transactional, app usage, and satisfaction patterns through structured exploratory analysis and customer segmentation workflows.

## Overview

The COFINFAD dataset contains customer-level information from a Colombian fintech platform, with data from **48,723 customers**, collected over **12 months in 2023**, and covering **57 variables** across demographics, transactions, product adoption, satisfaction, and digital engagement.

This project was designed to answer three main questions:

- What do customer profiles look like across demographic and behavioural dimensions?
- How do different variables relate to customer satisfaction, product usage, and app engagement?
- Can the customer base be segmented into meaningful groups for business interpretation?

## App Sections

The Shiny app is organised into the following sections:

### Introduction
Provides the project motivation, objectives, and business context.

### Univariate Analysis
Explores the distributions of key demographic, financial, transactional, and behavioural variables.

### Bivariate Analysis
Examines pairwise relationships and patterns between selected variables.

### Clustering Analysis
Applies segmentation methods to identify distinct customer groups and compare their characteristics.

## Key Features

- Interactive Shiny interface for exploratory fintech analysis
- Modular workflow across introduction, data preparation, univariate, bivariate, clustering, and proposal sections
- Customer segmentation insights using clustering techniques
- Visual analytics for comparing satisfaction, product usage, and app engagement patterns
- Business-oriented interpretation of customer groups for fintech decision-making

## Data

The analysis uses the **COFINFAD: Colombian Fintech Financial Analytics Dataset**, available [here](https://data.mendeley.com/datasets/mhb4zn3258/1).

The dataset includes:

- Demographic variables
- Financial product ownership
- Transactional activity
- Digital/app engagement measures
- Customer satisfaction indicators
- Geographic distribution across Colombian cities

## Methods Used

- Data cleaning and preprocessing
- Exploratory data analysis
- Univariate and bivariate visualisation
- Customer segmentation and clustering

## Tools Used

- R
- Shiny
- ggplot2
- plotly
- tidyverse
- clustering packages

## Key Insights

The analysis shows that customer behaviour differs across demographic and product-usage groups, and that stronger digital engagement tends to be associated with richer feature usage and higher satisfaction. 
Clustering also helps reveal distinct customer segments with different behavioural and transactional profiles, supporting more targeted interpretation of customer needs and value.

These insights can support fintech teams in:

- Tailoring product offerings
- Improving app features
- Identifying high-value customer segments
- Designing more targeted customer strategies

## Team Members

- Akshaya Vijayakumar Sivakami
- Nazia Faisalkhan Faruqui
