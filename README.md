# nfl-draft
The goal of this project is use web scraping in R and machine learning tools in Python
to analyze and make predictions about the 2018 NFL draft. I provide preliminary analysis
and draft predictions using Linear Regression and XGBoost.

## Getting Started
The only file that would need to explicitly be interacted with for scraping is the 
scrape_data.R file. This scrapes draft and combine information from http://profootballreference.com.
The current version of that file imputes data; however, in my Jupyter Notebook I provide an
imputation strategy using KNN that may give better results. There are some broken links on the
website, which I have explicitly accounted for. These may not be fixed for later or earlier years,
so some behavior may not work as expected.
