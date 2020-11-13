library(shiny)
library(ggplot2)

isGenerated <- FALSE
static <- c()

ui <- fluidPage(

  title = "Random Variables",


  fluidRow(




    tabsetPanel(id = "rvs",
                tabPanel("Uniform between 0-1",  sidebarPanel(
                                                        tabsetPanel(id = "p/g",
                                                          tabPanel("generate", numericInput("cugen", "count", 5),actionButton("gen1","generate"),textInput("excel1","choose file name", "data.csv"),actionButton("eb1","export")),
                                                          tabPanel("plot",br(), actionButton("plot1", "plot"), sliderInput("brick1", "break",5,100, 10), selectInput("colors1", "choose color", c("deepskyblue4","red","blue", "yellow","azure","orange", "green", "white"))
                                                                   )
                                                        )


                )),
                tabPanel("Uniform", sidebarPanel(
                                                        tabsetPanel(id = "p/g",
                                                                    tabPanel("generate", numericInput("dugen", "count", 5),
                                                                             sliderInput("unifRange", "Range", min = -100, max = 100, value = c(0,10)),
                                                                             actionButton("gen2","generate"),textInput("excel2","choose file name", "data.csv"),actionButton("eb2","export")),
                                                                    tabPanel("plot",sliderInput("unifRange1", "Range", min = -100, max = 100, value = c(0,10)),
                                                                             actionButton("plot2","plot") , sliderInput("brick2", "break",5,100, 10), selectInput("colors2", "choose color", c("deepskyblue4","red","blue", "yellow","azure","orange", "green", "white"))
                                                                             ), tabPanel("estimate", fileInput("text1","please choose sample file to upload", accept = '.txt'))
                                                                    ))),

                tabPanel("Bernoulli",sidebarPanel(
                                                      tabsetPanel(id = "p/g",
                                                                  tabPanel("generate", numericInput("bern", "count", 5),
                                                                           numericInput("pBern", "probability", 0.5),
                                                                           actionButton("gen3","generate"),textInput("excel3","choose file name", "data.csv"),actionButton("eb3","export")),
                                                                  tabPanel("plot",
                                                                           numericInput("pBern1", "probability", 0.5),
                                                                           actionButton("plot3", "plot") ,
                                                                           sliderInput("brick3", "break",5,100, 10), 
                                                                           selectInput("colors3", "choose color", c("deepskyblue4","red","blue", "yellow","azure","orange", "green", "white"))
                                                                           ),
                                                                  tabPanel("estimate", fileInput("text2","please choose sample file to upload", accept = '.txt'))

                                                      )

                                            )),
                tabPanel("Binomial",sidebarPanel(
                                            tabsetPanel(id = "p/g",
                                                        tabPanel("generate",numericInput("bigen", "count", 5),
                                                                 numericInput("pBi", "probability", 0.5),
                                                                 numericInput("biNum", "Number of trials",100),
                                                                 actionButton("gen4","generate"),
                                                                 textInput("excel4","choose file name", "data.csv"),
                                                                 actionButton("eb4","export")),
                                                        tabPanel("plot",numericInput("pBi1", "probability", 0.5),
                                                                 numericInput("biNum1", "Number of trials",100),
                                                                 actionButton("plot4", "plot"),
                                                                 sliderInput("brick4", "break",5,100, 10), 
                                                                 selectInput("colors4", "choose color", c("deepskyblue4","red","blue", "yellow","azure","orange", "green", "white"))
                                                                 ),
                                                        tabPanel("estimate", fileInput("text3","please choose sample file to upload", accept = '.txt'))

                                                        )

                                           )),
                tabPanel("Geometric",sidebarPanel(
                                                tabsetPanel(id = "p/g",
                                                            tabPanel("generate",  numericInput("geo", "count", 5),
                                                                     numericInput("pGeo", "probability", 0.5),
                                                                     actionButton("gen5","generate"),
                                                                     textInput("excel5","choose file name", "data.csv"),
                                                                     actionButton("eb5","export")),
                                                            tabPanel("plot", numericInput("pGeo1", "probability", 0.5),

                                                                     actionButton("plot5", "plot"),
                                                                     sliderInput("brick5", "break",5,100, 10), 
                                                                     selectInput("colors5", "choose color", c("deepskyblue4","red","blue", "yellow","azure","orange", "green", "white"))
                                                            ),
                                                            tabPanel("estimate", fileInput("text4","please choose sample file to upload", accept = '.txt'))
                                                          
                                                            )

                                            )),
                tabPanel("Exponential",sidebarPanel(
                                                  tabsetPanel(id = "p/g",
                                                              tabPanel("generate",numericInput("exp", "count", 5),
                                                                       numericInput("Lambda", "lambda", 1),
                                                                       actionButton("gen6","generate"),
                                                                       textInput("excel6","choose file name", "data.csv"),
                                                                       actionButton("eb6","export")),
                                                              tabPanel("plot",numericInput("Lambda1", "lambda", 1),
                                                                       actionButton("plot6", "plot"),
                                                                       sliderInput("brick6", "break",5,100, 10), 
                                                                       selectInput("colors6", "choose color", c("deepskyblue4","red","blue", "yellow","azure","orange", "green", "white"))
                                                              ),
                                                              tabPanel("estimate", fileInput("text5","please choose sample file to upload", accept = '.txt'))
                                                              )

                                              )),

                tabPanel("Gamma",sidebarPanel(
                                            tabsetPanel(id = "p/g",tabPanel("generate", numericInput("gamma", "count", 5),
                                                                            numericInput("Lambda2", "lambda", 1),
                                                                            numericInput("k", "k", 1),
                                                                            actionButton("gen7","generate"),
                                                                            textInput("excel7","choose file name", "data.csv"),
                                                                            actionButton("eb7","export")),
                                                        tabPanel("plot", numericInput("Lambda3", "lambda", 1),
                                                                 numericInput("k1", "k", 1),
                                                                 actionButton("plot7", "plot"),
                                                                 sliderInput("brick7", "break",5,100, 10), 
                                                                 selectInput("colors7", "choose color", c("deepskyblue4","red","blue", "yellow","azure","orange", "green", "white"))
                                                        ),
                                                        tabPanel("estimate", fileInput("text6","please choose sample file to upload", accept = '.txt'))
                                                                 
                                                        )

                 )),

                tabPanel("Poisson",sidebarPanel(
                                              tabsetPanel(id = "p/g", tabPanel("generate", numericInput("poi", "count", 5),
                                                                               numericInput("Lambda4", "lambda/t", 1),
                                                                               numericInput("t", "t", 1),
                                                                               actionButton("gen8","generate"),
                                                                               textInput("excel8","choose file name", "data.csv"),
                                                                               actionButton("eb8","export")),
                                                          tabPanel("plot", numericInput("Lambda5", "lambda/t", 10),
                                                                   numericInput("t1", "t", 1),
                                                                   actionButton("plot8", "plot"),
                                                                   sliderInput("brick8", "break",5,100, 10), 
                                                                   selectInput("colors8", "choose color", c("deepskyblue4","red","blue", "yellow","azure","orange", "green", "white"))
                                                          ),
                                                          tabPanel("estimate", fileInput("text7","please choose sample file to upload", accept = '.txt'))
                                                          )

                  )),
                tabPanel("Normal",sidebarPanel(
                                      tabsetPanel(id = "p/g", tabPanel("generate", numericInput("norm", "count", 5),
                                                                       numericInput("Lambda6", "expected value", 1),
                                                                       numericInput("var", "variance", 1),
                                                                       actionButton("gen9","generate"),
                                                                       textInput("excel9","choose file name", "data.csv"),
                                                                       actionButton("eb9","export")),
                                                  tabPanel("plot", numericInput("Lambda7", "expected value", 1),
                                                           numericInput("var1", "variance", 1),
                                                           actionButton("plot9", "plot"),
                                                           sliderInput("brick9", "break",5,100, 10), 
                                                           selectInput("colors9", "choose color", c("deepskyblue4","red","blue", "yellow","azure","orange", "green", "white"))
                                                  ),
                                                  tabPanel("estimate", fileInput("text8","please choose sample file to upload", accept = '.txt'))
                                                  )

                 )),
                
             
                tabPanel("about us",
                         mainPanel(
                           br(),
                           h5("this app is an open source project done by Aryan Mikaeili, Ashkan Soleymani, Soroosh Baselizadeh and Amirhossein Bahrami as a Project for Probability and Statistics for Computer Engineering Course in Sharif
                              University Of technology."),
                           br(),
                           h5("Visit our GitHub account for more information and the source code."),
                           actionLink("link", "GitHub")
                           
                         )
                         
                         
                         )


    ),


    h3(textOutput("message")),
    tableOutput("table"),
    column(width = 10,
           plotOutput("plot"),
           plotOutput("ggplot")
           ),
    
    tableOutput(("estim"))













))


server <- function(input,output)
{
  observeEvent(input$link, {
    browseURL("https://github.com/abahram77/familiarDistribiution")
  })

  observeEvent(input$text1, {
    file <- input$text1
    sample_vector <- read.table(file$datapath)
    v<- as.vector(sample_vector[,1])
    output$table <- renderTable(
      return()
    )
    output$message <- renderText(
      return()
    )
    
    res <- uniformEstimation(v)
    output$estim <- renderTable(res , rownames = TRUE)
    output$plot <- renderPlot(visualizeDu(res[1,1] , res[2,1] , 10 , "red"))
    output$ggplot <- renderPlot(visualizeDuPlot(res[1,1] , res[2,1]))
    
    
  })
  
  
  observeEvent(input$text2, {
    file <- input$text2
    sample_vector <- read.table(file$datapath)
    v<- as.vector(sample_vector[,1])
    output$table <- renderTable(
      return()
    )
    output$message <- renderText(
      return()
    )
    output$plot <- renderPlot(
      return()
    )
    output$ggplot <- renderPlot(
      return()
    )
    res <- bernulliEstimation(v)
    output$estim <- renderTable(res , rownames = TRUE)
    
    output$plot <- renderPlot(visualizebr(res[1,1] , 10 , "red"))
    output$ggplot <- renderPlot(visualizebrPlot(res[1,1]))
    
  })
  observeEvent(input$text3, {
    file <- input$text3
    sample_vector <- read.table(file$datapath)
    v<- as.vector(sample_vector[,1])
    output$table <- renderTable(
      return()
    )
    output$message <- renderText(
      return()
    )
    output$plot <- renderPlot(
      return()
    )
    output$ggplot <- renderPlot(
      return()
    )
    res <- binomialEstimation(v)
    output$estim <- renderTable(res , rownames = TRUE)
    output$plot <- renderPlot(visualizebi(res[1,1] , 10 , 10 , "red"))
    output$ggplot <- renderPlot(visualizebiPlot(res[1,1], 10))
    
  })
  
  observeEvent(input$text4, {
    file <- input$text4
    sample_vector <- read.table(file$datapath)
    v<- as.vector(sample_vector[,1])
    output$table <- renderTable(
      return()
    )
    output$message <- renderText(
      return()
    )
    output$plot <- renderPlot(
      return()
    )
    output$ggplot <- renderPlot(
      return()
    )
    res <- geometricEstimation(v)
    output$estim <- renderTable(res , rownames = TRUE)
    output$plot <- renderPlot(visualizegeo(res[1,1] , 10 , "red"))
    output$ggplot <- renderPlot(visualizegeoPlot(res[1,1]))
    
    
  })
  observeEvent(input$text5, {
    file <- input$text5
    sample_vector <- read.table(file$datapath)
    v<- as.vector(sample_vector[,1])
    output$table <- renderTable(
      return()
    )
    output$message <- renderText(
      return()
    )
    output$plot <- renderPlot(
      return()
    )
    output$ggplot <- renderPlot(
      return()
    )
    res <- exponentialEstimation(v)
    output$estim <- renderTable(res , rownames = TRUE)
    output$plot <- renderPlot(visualizeEXP(res[1,1] , 10 , "red"))
    output$ggplot <- renderPlot(visualizeEXPPlot(res[1,1]))
    
    
  })
  
  observeEvent(input$text6, {
    file <- input$text6
    sample_vector <- read.table(file$datapath)
    v<- as.vector(sample_vector[,1])
    output$table <- renderTable(
      return()
    )
    output$message <- renderText(
      return()
    )
    output$plot <- renderPlot(
      return()
    )
    output$ggplot <- renderPlot(
      return()
    )
    res <- gammaEstimation(v)
    output$estim <- renderTable(res, rownames = TRUE)
    output$plot <- renderPlot(visualizeGAMMA(res[2,1], res[1,1] , 10 , "red"))
    output$ggplot <- renderPlot(visualizeGAMMAPlot(res[2,1], res[1,1]))
    
  })
  
  observeEvent(input$text7, {
    file <- input$text7
    sample_vector <- read.table(file$datapath)
    v<- as.vector(sample_vector[,1])
    output$table <- renderTable(
      return()
    )
    output$message <- renderText(
      return()
    )
    output$plot <- renderPlot(
      return()
    )
    output$ggplot <- renderPlot(
      return()
    )
    res <- poissonEstimation(v)
    output$estim <- renderTable(res, rownames = TRUE)
    output$plot <- renderPlot(visualizePOISSON(res[1,1], 1 , 10 , "red"))
    output$ggplot <- renderPlot(visualizePOISSONPlot(res[1,1], 1))
    
    
  })
  
  observeEvent(input$text8, {
    file <- input$text8
    sample_vector <- read.table(file$datapath)
    v<- as.vector(sample_vector[,1])
    output$table <- renderTable(
      return()
    )
    output$message <- renderText(
      return()
    )
    
    output$plot <- renderPlot(
      return()
    )
    output$ggplot <- renderPlot(
      return()
    )
    res <- normalEstimation(v)
    output$estim <- renderTable(res, rownames = TRUE)
    
    output$plot <- renderPlot(visualizeNORMAL(res[1,1], res[2,1] , 10 , "red"))
    output$ggplot <- renderPlot(visualizeNORMALPlot(res[1,1], res[2,1]))
    
  })

    observeEvent(input$gen1,{
        if(is.numeric(input$cugen) == FALSE)
        {
          output$table <- renderTable(
            return()
          )
          output$estim <- renderText(
            return()
          )
          output$plot <- renderPlot(
            return()
          )
          output$ggplot <- renderPlot(
            return()
          )
          output$message <- renderText("input is not numeric")

          return()

        }
        data <- cugen(input$cugen)

        output$plot <- renderPlot(
          return()
        )
        output$estim <- renderText(
          return()
        )
        output$ggplot <- renderPlot(
          return()
        )
        output$message <- renderText(
          return()
        )
        output$table <- renderTable(data, hover = TRUE, bordered = TRUE)
    })
  observeEvent(input$eb1,{
    if(is.numeric(input$cugen) == FALSE)
    {
      output$table <- renderTable(
        return()
      )
      output$estim <- renderText(
        return()
      )
      output$plot <- renderPlot(
        return()
      )
      output$ggplot <- renderPlot(
        return()
      )
      output$message <- renderText("input is not numeric")
      return()

    }
    data <- cugen(input$cugen)
    data <- data.frame(data)
    colnames(data) <- "Uniform between 0-1"
    write.csv(data, input$excel1)
  })


  observeEvent(input$plot1,{

    output$table <- renderTable(
      return()
    )
    output$estim <- renderText(
      return()
    )
    output$message <- renderText(
      return()
    )

      output$plot <- renderPlot(visualizeCu(input$brick1, input$colors1))
      output$ggplot <- renderPlot(visualizeCuPlot())



  })

  observeEvent(input$gen2,{
    if(is.numeric(input$dugen) == FALSE){
      
      output$table <- renderTable(
        return()
      )
      output$estim <- renderText(
        return()
      )
      output$plot <- renderPlot(
        return()
      )
      output$ggplot <- renderPlot(
        return()
      )
      output$message <- renderText("input is not numeric")
      
      return()
      
    }
      
    data <- dugen(input$unifRange[1],input$unifRange[2],input$dugen)
    output$plot <- renderPlot(
      return()
    )
    output$estim <- renderText(
      return()
    )
    output$ggplot <- renderPlot(
      return()
    )
    output$message <- renderText(
      return()
    )
    output$table <- renderTable(data, hover = TRUE, bordered = TRUE)
  })
  
  observeEvent(input$eb2,{
    if(is.numeric(input$cugen) == FALSE)
    {
      output$table <- renderTable(
        return()
      )
      output$estim <- renderText(
        return()
      )
      output$plot <- renderPlot(
        return()
      )
      output$ggplot <- renderPlot(
        return()
      )
      output$message <- renderText("input is not numeric")
      return()
      
    }
    data <- dugen(input$unifRange[1],input$unifRange[2],input$dugen)
    data <- data.frame(data)
    colnames(data) <- paste0("Uniform between " , input$unifRange[1] , " & " , input$unifRange[2])
    write.csv(data, input$excel2)
  })
  
  
  observeEvent(input$plot2,{

    output$table <- renderTable(
      return()
    )
    output$estim <- renderText(
      return()
    )
    output$message <- renderText(
      return()
    )
    
    output$plot <- renderPlot(visualizeDu(input$unifRange1[1],input$unifRange1[2] , input$brick2, input$colors2))
    output$ggplot <- renderPlot(visualizeDuPlot(input$unifRange1[1],input$unifRange1[2]))
  })
  
  
  
  observeEvent(input$gen3,{
    if(is.numeric(input$bern) == FALSE){
      
      output$table <- renderTable(
        return()
      )
      output$estim <- renderText(
        return()
      )
      output$plot <- renderPlot(
        return()
      )
      output$ggplot <- renderPlot(
        return()
      )
      output$message <- renderText("input is not numeric")
      
      return()
      
    }
    data <- brgen(input$pBern,input$bern)
    output$plot <- renderPlot(
      return()
    )
    output$estim <- renderText(
      return()
    )
    output$ggplot <- renderPlot(
      return()
    )
    output$message <- renderText(
      return()
    )
    output$table <- renderTable(data, hover = TRUE, bordered = TRUE)
  })
  
  observeEvent(input$eb3,{
    if(is.numeric(input$bern) == FALSE)
    {
      output$table <- renderTable(
        return()
      )
      output$estim <- renderText(
        return()
      )
      output$plot <- renderPlot(
        return()
      )
      output$ggplot <- renderPlot(
        return()
      )
      output$message <- renderText("input is not numeric")
      return()
      
    }
    data <- brgen(input$pBern,input$bern)
    data <- data.frame(data)
    colnames(data) <- paste0("Bernouli with p =" , input$pBern)
    write.csv(data, input$excel3)
  })
  
  observeEvent(input$plot3,{
    output$table <- renderTable(
      return()
    )
    output$estim <- renderText(
      return()
    )
    output$message <- renderText(
      return()
    )
    
    output$plot <- renderPlot(visualizebr(input$pBern1 , input$brick3, input$colors3))
    output$ggplot <- renderPlot(visualizebrPlot(input$pBern1))

  })
  
  observeEvent(input$gen4,{
    if(is.numeric(input$bigen) == FALSE){
      
      output$table <- renderTable(
        
        return()
      )
      output$estim <- renderText(
        return()
      )
      output$plot <- renderPlot(
        return()
      )
      output$ggplot <- renderPlot(
        return()
      )
      output$message <- renderText("input is not numeric")
      
      return()
      
    }
    data <- c()
    for(i in 1:input$bigen)
    {
      data <- c(data, bigen(input$pBi, input$biNum, input$bigen))
    }

    output$plot <- renderPlot(
      return()
    )
    output$estim <- renderText(
      return()
    )
    output$ggplot <- renderPlot(
      return()
    )
    output$message <- renderText(
      return()
    )
    output$table <- renderTable(data, hover = TRUE, bordered = TRUE)
  })
  
  observeEvent(input$eb4,{
    if(is.numeric(input$bigen) == FALSE)
    {
      output$table <- renderTable(
        return()
      )
      output$estim <- renderText(
        return()
      )
      output$plot <- renderPlot(
        return()
      )
      output$ggplot <- renderPlot(
        return()
      )
      output$message <- renderText("input is not numeric")
      return()
      
    }
    data <- c()
    for(i in 1:input$bigen)
    {
      data <- c(data, bigen(input$pBi, input$biNum, input$bigen))
    }
    data <- data.frame(data)
    colnames(data) <- paste0("Binomial with n =" , input$biNum ," & p =" ,input$pBi)
    write.csv(data, input$excel4)
  })
  
  observeEvent(input$plot4,{
    output$table <- renderTable(
      return()
    )
    output$message <- renderText(
      return()
    )
    output$estim <- renderText(
      return()
    )
    output$plot <- renderPlot(visualizebi(input$pBi1 , input$biNum1 , input$brick4, input$colors4))
    output$ggplot <- renderPlot(visualizebiPlot(input$pBi1, input$biNum1))

  })
  
  observeEvent(input$gen5,{
    if(is.numeric(input$geo) == FALSE){
      
      output$table <- renderTable(
        return()
      )
      output$estim <- renderText(
        return()
      )
      output$plot <- renderPlot(
        return()
      )
      output$ggplot <- renderPlot(
        return()
      )
      output$message <- renderText("input is not numeric")
      
      return()
      
    }
    data <- c()
    for(i in 1:input$geo)
    {
      data <- c(data,gegen3(input$pGeo))
    }
    output$plot <- renderPlot(
      return()
    )
    output$estim <- renderText(
      return()
    )
    output$ggplot <- renderPlot(
      return()
    )
    output$message <- renderText(
      return()
    )
    output$table <- renderTable(data, hover = TRUE, bordered = TRUE)

  })
  
  observeEvent(input$eb5,{
    if(is.numeric(input$geo) == FALSE)
    {
      output$table <- renderTable(
        return()
      )
      output$estim <- renderText(
        return()
      )
      output$plot <- renderPlot(
        return()
      )
      output$ggplot <- renderPlot(
        return()
      )
      output$message <- renderText("input is not numeric")
      return()
      
    }
    data <- c()
    for(i in 1:input$geo)
    {
      data <- c(data,gegen3(input$pGeo))
    }
    data <- data.frame(data)
    colnames(data) <- paste0("Geometrics with P =" , input$pGeo)
    write.csv(data, input$excel5)
  })

  observeEvent(input$plot5,{

    output$table <- renderTable(
      return()
    )
    output$message <- renderText(
      return()
    )
    output$estim <- renderText(
      return()
    )
    
    output$plot <- renderPlot(visualizegeo(input$pGeo1 , input$brick5, input$colors5))
    output$ggplot <- renderPlot(visualizegeoPlot(input$pGeo1))


  })

  observeEvent(input$gen6,{
    if(is.numeric(input$Lambda) == FALSE){
      
      output$table <- renderTable(
        return()
      )
      output$plot <- renderPlot(
        return()
      )
      output$ggplot <- renderPlot(
        return()
      )
      output$estim <- renderText(
        return()
      )
      output$message <- renderText("input is not numeric")
      return()
      
    }
      
    data <- c()
    for(i in 1:input$exp)
    {
      data <- c(data,expgen(input$Lambda))

    }
    output$plot <- renderPlot(
      return()
    )
    output$estim <- renderText(
      return()
    )
    output$ggplot <- renderPlot(
      return()
    )
    output$message <- renderText(
      return()
    )
    
    output$table <- renderTable(data, hover = TRUE, bordered = TRUE)

  })
  
  observeEvent(input$eb6,{
    if(is.numeric(input$Lambda) == FALSE)
    {
      output$table <- renderTable(
        return()
      )
      output$estim <- renderText(
        return()
      )
      output$plot <- renderPlot(
        return()
      )
      output$ggplot <- renderPlot(
        return()
      )
      output$message <- renderText("input is not numeric")
      return()
      
    }
    data <- c()
    for(i in 1:input$exp)
    {
      data <- c(data,expgen(input$Lambda))
    }
    data <- data.frame(data)
    colnames(data) <- paste0("Exp with lambda =" , input$Lambda)
    write.csv(data, input$excel6)
  })
  
  observeEvent(input$plot6,{

    output$table <- renderTable(
      return()
    )
    output$estim <- renderText(
      return()
    )
    output$message <- renderText(
      return()
    )
    
    output$plot <- renderPlot(visualizeEXP(input$Lambda1 , input$brick6, input$colors6))
    output$ggplot <- renderPlot(visualizeEXPPlot(input$Lambda1))

  })
  
  observeEvent(input$gen7,{
    if(is.numeric(input$gamma) == FALSE){
      
      output$table <- renderTable(
        return()
      )
      output$estim <- renderText(
        return()
      )
      output$plot <- renderPlot(
        return()
      )
      output$ggplot <- renderPlot(
        return()
      )
      output$message <- renderText("input is not numeric")
      return()
      
    }
    
    data <- c()
    for(i in 1:input$gamma)
    {
      data <- c(data,gagen(input$Lambda2, input$k))
    }
    output$plot <- renderPlot(
      return()
    )
    output$estim <- renderText(
      return()
    )
    
    output$ggplot <- renderPlot(
      return()
    )
    output$message <- renderText(
      return()
    )
    output$table <- renderTable(data, hover = TRUE, bordered = TRUE)

   })
  
  observeEvent(input$eb7,{
    if(is.numeric(input$gamma) == FALSE)
    {
      output$table <- renderTable(
        return()
      )
      output$estim <- renderText(
        return()
      )
      output$plot <- renderPlot(
        return()
      )
      output$ggplot <- renderPlot(
        return()
      )
      output$message <- renderText("input is not numeric")
      return()
      
    }
    data <- c()
    for(i in 1:input$gamma)
    {
      data <- c(data,gagen(input$Lambda2, input$k))
    }
    data <- data.frame(data)
    colnames(data) <- "Gamma"
    write.csv(data, input$excel7)
  })
  
  observeEvent(input$plot7,{
    
    output$table <- renderTable(
      return()
    )
    output$estim <- renderText(
      return()
    )
    output$message <- renderText(
      return()
    )
    
    output$plot <- renderPlot(visualizeGAMMA(input$Lambda3, input$k1 , input$brick7, input$colors7))
    output$ggplot <- renderPlot(visualizeGAMMAPlot(input$Lambda3, input$k1))

  })
  
  observeEvent(input$gen8,{
    if(is.numeric(input$poi) == FALSE){
      
      output$table <- renderTable(
        return()
      )
      output$estim <- renderText(
        return()
      )
      output$plot <- renderPlot(
        return()
      )
      output$ggplot <- renderPlot(
        return()
      )
      output$message <- renderText("input is not numeric")
      return()
      
    }
    data <- c()
    for(i in 1:input$poi)
    {
      data <- c(data,pogen(input$Lambda4, input$t))
    }
    output$plot <- renderPlot(
      return()
    )
    output$estim <- renderText(
      return()
    )
    output$ggplot <- renderPlot(
      return()
    )
    output$message <- renderText(
      return()
    )
    output$table <- renderTable(data, hover = TRUE, bordered = TRUE)

  })
  
  observeEvent(input$eb8,{
    if(is.numeric(input$poi) == FALSE)
    {
      output$table <- renderTable(
        return()
      )
      output$estim <- renderText(
        return()
      )
      output$plot <- renderPlot(
        return()
      )
      output$ggplot <- renderPlot(
        return()
      )
      output$message <- renderText("input is not numeric")
      return()
      
    }
    data <- c()
    for(i in 1:input$poi)
    {
      data <- c(data,pogen(input$Lambda4, input$t))
    }
    data <- data.frame(data)
    colnames(data) <- paste0("Poisson with lambda = " , input$Lambda4)
    write.csv(data, input$excel8)
  })
  
  observeEvent(input$plot8,{

    output$table <- renderTable(
      return()
    )
    output$message <- renderText(
      return()
    )
    output$estim <- renderText(
      return()
    )
    
    output$plot <- renderPlot(visualizePOISSON(input$Lambda5, input$t1 , input$brick8, input$colors8))
    output$ggplot <- renderPlot(visualizePOISSONPlot(input$Lambda5, input$t1))

  })
  observeEvent(input$gen9,{
    if(is.numeric(input$norm) == FALSE){
      
      output$table <- renderTable(
        return()
      )
      output$estim <- renderText(
        return()
      )
      output$plot <- renderPlot(
        return()
      )
      output$ggplot <- renderPlot(
        return()
      )
      output$message <- renderText("input is not numeric")
      return()
      
    }
    data <- c()
    for(i in 1:input$norm)
    {
      data <- c(data,nogen(input$Lambda6, input$var))
    }
    output$plot <- renderPlot(
      return()
    )
    output$estim <- renderText(
      return()
    )
    output$ggplot <- renderPlot(
      return()
    )
    output$message <- renderText(
      return()
    )
    output$table <- renderTable(data, hover = TRUE, bordered = TRUE)

  })
  observeEvent(input$plot9,{
    output$table <- renderTable(
      return()
    )
    output$message <- renderText(
      return()
    )
    output$estim <- renderText(
      return()
    )
    
    output$plot <- renderPlot(visualizeNORMAL(input$Lambda7, input$var1 , input$brick9, input$colors9))
    output$ggplot <- renderPlot(visualizeNORMALPlot(input$Lambda7, input$var1))

  })

  observeEvent(input$eb9,{
    if(is.numeric(input$norm) == FALSE)
    {
      output$table <- renderTable(
        return()
      )
      output$estim <- renderText(
        return()
      )
      output$plot <- renderPlot(
        return()
      )
      output$ggplot <- renderPlot(
        return()
      )
      output$message <- renderText("input is not numeric")
      return()
      
    }
    data <- c()
    for(i in 1:input$norm)
    {
      data <- c(data,nogen(input$Lambda6, input$var))
    }
    data <- data.frame(data)
    colnames(data) <- "normal"
    write.csv(data, input$excel9)
  })

}


shinyApp(ui, server)
