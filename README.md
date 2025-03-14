# BigDataProject

![GitHub repo size](https://img.shields.io/github/repo-size/FStiffler/BigDataProject)

## The Repository 

This repository is for our class in Big Data Analytics. Find the course description on the course catalog of the University of Lucerne [here](https://portal.unilu.ch/details?code=FS211088). The course materials are on Professor Matter's GitHub repo [here](https://github.com/umatter/BigData).

## About this project

For the Part II of our group projects, we evalute a simple research question for a large (>2GB) dataset of our choice. We use the popular [Yelp dataset from Kaggle](https://www.kaggle.com/yelp-dataset/yelp-dataset). This dataset is a subset of Yelp's businesses, reviews, and user data. We implement our analysis in R.

Due to the group size, we take on two seperate [analyses](https://github.com/FStiffler/BigDataProject/blob/master/Analysis.R): 
* For the first one, we try to estimate the effect of stars assigned on average in reviews on the probability of being a elite user (Uluru OLS).
* The second analysis tries to predict the most important factors for restaurants to have good reviews (forward selection).

## Getting Started

1. Download the dataset from  [Kaggle](https://www.kaggle.com/yelp-dataset/yelp-dataset) (Version 3).
2. Convert the dataset from json format to csv format using the [JSONtoCSV.R](https://github.com/FStiffler/BigDataProject/blob/master/JSONtoCSV.R) file.
3. Since the .csv files are still to big use the [largeToSmallCSV.R](https://github.com/FStiffler/BigDataProject/blob/master/largeToSmallCSV.R) file, to convert it into smaller .csv files.

## Analysing the data

After you did all the preparation steps you should have a few .csv files in your working directory. With these data sets you can start the analysis. All the R packages that we used are sourced in the [packageDependencies.R](https://github.com/FStiffler/BigDataProject/blob/master/packageDependencies.R). They are sourced at the beginning of the files so you dont need to add anything. If you use additional packages, make sure you add them to the packageDependencies file.

For our project we did a Uluru OLS regression and a forward selection.
The OLS uses the user data, while the forward selection uses the business data.
You can find both of the analyses in the [Analysis.R](https://github.com/FStiffler/BigDataProject/blob/master/Analysis.R) file.

## Results

As a part of the course we had to present some final results.
You can find the presentation of our results as an [RMarkdownfile](https://github.com/FStiffler/BigDataProject/blob/master/presentation.Rmd) or a ready-to-go [HTML](https://github.com/FStiffler/BigDataProject/blob/master/presentation.html) file.
To generate some of the plots in the presentation we sourced the [readFilesBenchmark.R](https://github.com/FStiffler/BigDataProject/blob/master/readFilesBenchmark.R) file.


## Feedback

Feel free to reach out to any of us [contributors](https://github.com/https://github.com/FStiffler/BigDataProject/graphs/contributors)!


