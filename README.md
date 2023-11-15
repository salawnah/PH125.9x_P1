Capstone Project 1 : The MovieLens
================
2023-11-15

# Overview

Recommendation systems are very crucial in various business segments,
including but not limited to online shopping and movies streaming
services. The key idea is that given the user history in rating the
items and the other users history in rating the items in additions to
the information about the items themselves and other factors, what is
expected user rating for the items that he/she never see or have?
estimating this rating will help the businesses to recommend other items
that the user most probably interested in.

In this project I have a data set of 10 million ratings for different
users on different movies, this data set includes the following: the
rating value out of 5, the user id, the movie information and the time
at which the user rated that particular movie, this data set is splitted
into two parts: the model building part (*edx*) of 90% of the entries,
and the final testing part (*final_holdout_test*) of 10% of the entries,
the *edx* data set is then splitted into model training part
(*edx_train*) and model testing part (*edx_test*).

The *edx_train* and *edx_test* data sets will be used to build,
evaluate, and select the best performing model that predicts the movie
rating with the minimum root mean square error (RMSE), after selecting
the best model it will be tested using the *final_holdout_test* data set
that is never used in training or evaluating the model performance
before.

After splitting the data sets, I started with the features engineering
to select the features of interest and to extract some new features
based on the existing ones, I did the following steps:

- Extract the movie release year from the movie title, the release year
  is the last part of the movie title in this format (release_year).
- Extract the rating year, rating month, rating week of the year, rating
  day of the week from the rating timestamp.
- Transform the movie genres to one-hot-coding, where each genre will be
  in a separate column.
- Did some correlation analysis to remove some of the features that are
  highly correlated with the others.

Then, I start using the features in incremental manner to build
different models and evaluate them using the testing data set, after
selecting the best model I used regularization to to improve the model
performance.

The best regularized model is then used with the whole model building
data set *edx* and evaluated against the final testing data set
*final_holdout_test*, the RMSE of 0.864299 was achieved.

## Please open the Capstone_Project1.pdf for the full report
