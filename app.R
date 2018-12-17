#### Description ###############################################################
##
## Skeleton Shiny app  using miniUI 
## This will look just better on mobile devices
##


#### Libraries needed #########################################################
library(shiny)
library(miniUI)
library(jpeg)
library(dplyr)
library(DT)
library(reticulate)

##### Initals / startup code #################################################

# restrict to 3 MB per file
#options(shiny.maxRequestSize=3*1024^2)

#setup python env en imports
reticulate::use_condaenv("my_py36")
torch = import("torch")
Image = import("PIL")$Image

#load the pytorch model
flowermodel  = torch$load("data/flowermodel.pt")

# helper function to use a pytorch model for scoring in shiny
TorchPrediction = function(model, picture){
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
  
  results = tibble::tibble(Prob = P, flower = c( 'bluebell', 'crocus', 'daffodil', 'lilyvally', 'snowdrop'))
  results$image = paste0(
    "<img src='",
    results$flower, ".jpg",
    "'",
    "height='80' width='90' </img>"
  )
  
  results
}

#### MINIPAGE #################################################################

ui <- miniPage(
  gadgetTitleBar(
    left = NULL, 
    right = NULL,
    "Check Flower Species"
  ),
  
  miniTabstripPanel(
    
    #### introduction tab ############
    miniTabPanel(
      "introduction", icon = icon("area-chart"),
      miniContentPanel(
        htmlOutput("intro")
      )
    ),
    
    #### parameters tab ##############
    miniTabPanel(
      "Take_picture", icon = icon("sliders"),
      miniContentPanel(
        fileInput('file1', 'Choose an image (max 5MB)'),
        img(SRC="coverimage.png", height = 340)
      )
    ),
 
    #### image tab ##################
    miniTabPanel(
      "Image_taken", icon = icon("file-image-o"),
      miniContentPanel(
        padding = 0,
        imageOutput("plaatje")
      )
    ),
    
    #### Tabel resultaat ###########
    miniTabPanel(
      "Best_Matches", icon = icon("table"),
      miniContentPanel(
        dataTableOutput("ResultaatTabel")
      )
    )
    
  )
)

################################################################################

#### SERVER FUNCTION ###########################################################

server <- function(input, output, session) {
  
  #### observe functions ####################
  observe({
    # Create a list of new options, where the name of the items is something
    # like 'option label x 1', and the values are 'option-x-1'.
    s_options <- as.list(c("I feel Lucky"))
    
    updateSelectInput(session, "typeselect", choices = s_options)
    
  })
  
  
  #### reactive functions ###################
  ProcessImage <- reactive({
    
    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())
    
    progress$set(
      message = 'in progress', 
      detail = 'This may take a few seconds...'
    )
    
    inFile = input$file1
    if(is.null(inFile)){
      imgfile  = "www/image_0112.jpg"
    }else{
      imgfile = inFile$datapath
    }
    
    TorchPrediction(flowermodel, imgfile)
  })
  
  
  ###### OUTPUT ELEMENTS ######################################################
  
  #### intro ####
  output$intro <- renderUI({
    list(
        h3("Introduction"),
        h4("When uncertain of a flower species, take a picture of the flower, 
         then the picture will be scored using a fine tuned resnet model"), 
        h4("With pytorch a resnet model was finetuned on flower images, 
         then this model is used in this shiny app"), 
        h4("See",  tags$a(href="https://github.com/longhowlam/flowermodel", "GitHub Repo", target="_blank"), "repo for details:"),
        h4(" Cheers Longhow Lam"),
        img(SRC="coverimage.png", height = 340)
      )
  })
  
  
  #### plaatje ####
  output$plaatje <- renderImage({
    
    if (!is.null(input$file1))
    {
      
      width  <- session$clientData$output_plaatje_width
      height <- session$clientData$output_plaatje_height
      list(
        src = input$file1$datapath,
        width=width,
        height=height
      )
    }
    else
    {
      list(src="www/picturetaken.jpg")
    }
  },
  deleteFile = FALSE
  )
  
  
  #### ResultaatOut ####
  output$ResultaatOut = renderPrint({
    pp = ProcessImage()
    print(pp)
  })
  
  
  #### ResultaatTabel ####
  output$ResultaatTabel =  renderDataTable({
    
    out = ProcessImage() %>% arrange(desc(Prob))
    pp = list.files("www", pattern = "imagetaken*")
    file.remove(paste0("www/",pp))
    tofile = paste0("imagetaken", paste(sample(letters, size=5), collapse=""), ".jpg")
   
    
    if (!is.null(input$file1))
    {
      inFile = input$file1
      src = inFile$datapath
      file.copy(src, paste0("www/",tofile) , overwrite = TRUE)
    }
    
    out$image_to_score = paste0(
      "<img src='",
      tofile,
      "'",
      "height='80' width='90' </img>"
    )
    
    datatable(
      escape = FALSE,
      rownames = FALSE, 
      data = out,options = list(dom = 't')
    ) %>% formatPercentage('Prob', 4) 
    
  })
  
  observeEvent(input$done, {
    stopApp(TRUE)
  })
}


##### SHINY APP CALL ###########################################################

shinyApp(ui, server)
