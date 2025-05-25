# My Data

1. **COLDAT_dyads.csv** and **COLDAT_colonies.csv**: [Colonial Dates Dataset (2019)](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/T9SDEW). These datasets contains colonies, colonizers, as well as start and end dates of the colonization period. 'Dyads' is a longer version and 'colonies' is a wider version.  
2. **GDP.csv**: [World Development Indicators](https://databank.worldbank.org/reports.aspx?source=2&series=NY.GDP.MKTP.CD&country#) from the World Bank Group. I will only be using 2023 GDP from this dataset. 
3. **WVS7.csv**: [World Values Survey](https://www.worldvaluessurvey.org/WVSDocumentationWV7.jsp). This dataset, which is from Wave 7 of the World Values Survey, contains responses by nation to a number of values based questions. The csv file is too large to upload to GitHub. It may be accessed [here](https://drive.google.com/file/d/1QNOcg5RX0RwbtunwqESCfhnVqR4Hd3oW/view?usp=sharing). *See WVS_Codebook.pdf for a guide to the variables in this dataset.*
4. **gapminder_gini.xlsx** and **country_gini.csv**: [Gini Data](https://www.gapminder.org/data/documentation/gini/) from Gapminder. This dataset contains Gini data, or measures of income inequality, from countries around the world and from a number of sources. The csv file is directly pulled from the original file's sheet entitled "data-for-country-by-year-in."
5. **ccpcnc_v5.csv**: [Comparative Constitutions Project](https://comparativeconstitutionsproject.org/download-data/). This data set contains characteristics of each of the world's constitutions throughout the dates each iteration existed. The csv file is too large to upload to GitHub. It may be accessed [here](https://drive.google.com/file/d/1DnwYejskDbd0zYNEnYf8O9UT-IyVVEZV/view?usp=sharing). *See ccpcnc\codebook_v5.pdf for a guide to the variables in this data set.*
6. **country_codes.csv**: countries and their reference country codes; *scraped from WVS_Codebook.pdf*

## Intermediate Datasets (.rds files; may be found in 'colonizations' folder)
*See 'data_preparation.rmd' for all data transformations.*
1. **colonies.rds**
   - Adapted from: COLDAT_dyads
   - Used to create other intermediate data sets (see below) and "Empires Over Time"
2. **density_count.rds**
     - Adapted from: colonies.rds
     - Used to generate "Empires Over Time" and "Constitutions"
3. **gdp_colonies.rds**
     - Adapted from: GDP.csv and colonies.rds
     - Used to generate "GDP" in "Economic Effects"
4. **gini_long.rds**
     - Adapted from: country_gini.csv and colonies.rds
     - Used to generate "Gini" in "Economic Effects"
5. **plot_map.rds**
     - Adapted from: rworldmap package and colonies.rds
     - Used to generate "World Map"
6. **rights.rds**
     - Adapted from: density_count.rds and ccpcnc_v5.csv
     - Used to generate "Constitutions"
7. **wvs_colonies.rds**
     - Adapted from: WVS7.csv and colonies.rds
     - Used to generate "Global Values"