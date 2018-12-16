# helper function
library(reticulate)
reticulate::use_condaenv("my_py36")

flowermodel  = torch$load("data/flowermodel.pt")

TorchPrediction = function(model, picture){
  torch = import("torch")
  Image = import("PIL")$Image
  
  transforms = import("torchvision")$transforms
  
  preprocess = transforms$Compose(c(
    transforms$Resize(256L),
    transforms$CenterCrop(224L),
    transforms$ToTensor(),
    transforms$Normalize(c(0.485, 0.456, 0.406), c(0.229, 0.224, 0.225))
  ))

  IMG = Image$open(picture)
  inputs = preprocess(IMG)
  inputs$unsqueeze_(0L)
  outputs = flowermodel(inputs)

  sm = torch$nn$Softmax()
  probabilities = sm(outputs) 
  P = t(probabilities$detach()$numpy())[,1]
  
  results = tibble::tibble(Prob = P, flower = c( 'bluebell', 'daffodil', 'lilyvally', 'snowdrop'))
  results
}


TorchPrediction(flowermodel, "www/image_0112.jpg")
