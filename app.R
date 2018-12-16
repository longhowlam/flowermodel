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
library(text2vec)
library(jsonlite)
library(reticulate)

##### Initals / startup code #################################################

reticulate::use_condaenv("my_py36")

flowermodel  = torch$load("data/flowermodel.pt")


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
        img(SRC="image_0112.jpg", height = 340)
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
    
    #### Resultaat tab ############
#    miniTabPanel(
#      "Resultaat", icon = icon("table"),
#      miniContentPanel(
#        verbatimTextOutput("ResultaatOut")
#      )
#    ),
    
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
      h4("When uncertain of a flower species, take a picture of the flower, then the picture will be scored using a fine tuned resnet model"), 
      img(SRC="image_0112.jpg", height = 340)
    )
  })
  
  
  #### plaatje ####
  output$plaatje <- renderImage({
    
    inFile = input$file1
    print(inFile)
    if (!is.null(inFile))
    {
      
      width  <- session$clientData$output_plaatje_width
      height <- session$clientData$output_plaatje_height
      list(
        src = inFile$datapath,
        width=width,
        height=height
      )
    }
    else
    {
      list(src="www/kast.png")
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
   
    out = ProcessImage()
    datatable(
      escape = FALSE,
      rownames = FALSE, 
      data = out
    ) %>% formatPercentage('Prob', 4) 
  })
  
  observeEvent(input$done, {
    stopApp(TRUE)
  })
}


##### SHINY APP CALL ###########################################################

shinyApp(ui, server)
