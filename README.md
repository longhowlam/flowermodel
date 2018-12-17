# flowermodel

shiny app to predict flower species.

First a deeplearning model with pytorch is created, see the jupyter notebook. A Resnet is finetuned on flower images.

Second, the model is saved as a *.pt file

Third, In R a shiny app is created that uses the reticulate package to use the pytorch model to score new uploaded flower images
