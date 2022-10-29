## What is this?

This is a collection repo that contains some abstacted functions I've written to pre-process data and to fit multiple supervised learning models

## What are the main exported functions?

- VVfitClassificationModels and VVfitRegressionModels will train, validate and report accuracy metrics for the classification and regression models respectively
- Helper/Secondary contains preprocessing functions that will take an unclean dataset and will get it ready for modelling

## How do I use it?

Just clone the repo, source the file VVPreProcessFramework.R and use the functions you want

## Useful links

- The transformation problem: https://stats.stackexchange.com/questions/55692/back-transformation-of-an-mlr-model/58077#58077
- All available Caret models https://topepo.github.io/caret/available-models.html 

## Owner

Vasilis Vasileiou

## Future improvements

- Create another main function that will select the winner candidate model and will fit it in the whole dataset.
- Add additional accuracy metrics so that the user can select what they want to use.
- Add functions that would provide some transparency on which variables are the most predictive ones in the final model.