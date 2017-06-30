# PSTAT131
## Data Mining Fall 2016 at UCSB with Professor Sang Oh
This repo is dedicated to showing my work in the Data Mining class PSTAT 131 at UCSB taught by Professor Sang Oh during Fall 2016. All homework assignments, some section code and my final project are included in this repo.
## What is Data Mining?
Data Mining includes the processes of:

 * Loading in data
 * Reformatting it as necessary to create desired columns of data, deal with missing/corrupted data values, convert certain columns to different data types as necessary, standardizing various columns and more
 * Exploratory analysis of the data (data visualizations of relationships between variables or frequencies of variables, analyze if the variables in the dataset are correlated, etc.)
 * Analysis of the data types and the overall data set and deciding on a model to fit data as well as choosing what variable what we are trying to predict (if we haven't already)
 * Fit a model and look at residuals of the model; see if there are influential points that are worth removing
 * Try other models as necessary
 * Tune parameters of the model as necessary
 * Draw conclusions

In this repo I will touch on my homeworks and section work but the main focus will be on the final project.

## Homeworks

In the homeworks I practice the various data mining methods used in class from Principal Component Analysis to Support Vector Machines and more.

## Section Work

Here we practice some work in sections on classwork.

## Final Projects

For my final project, I took a "Student Alcohol Consumption" dataset and attempted to predict student final grades as a function of many other indicators (time to school, parent income, alcohol consumption, and more). 

What ended up happening was that the midway grades were extremely influential in the final model and ended up explaining most of the variance. Removing the midway grade ended up with a weak predictive model. Thus, I left the midway grade in the model and concluded that this dataset simply did not have the potential to give good insights.

