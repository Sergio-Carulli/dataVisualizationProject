library(shiny)
library(readr)
library(plotly)
library(ggplot2)
library(apcluster)

wine <- read.table("winequality-white.csv", header=TRUE, sep=";", as.is=TRUE, na.strings="?")
wine <- na.omit(wine)
wine <- wine[1:100,]
# Scaling
wine['alcohol']=as.numeric(wine$alcohol)
wine <- as.data.frame(scale(wine))


# Define UI for application that draws a histogram
ui <- navbarPage(title="Wine Analysis",
                 tabPanel("Bubble Plot",fluidPage(
                     # Application title
                     titlePanel("Bubble Plot"),
                     sidebarLayout(
                         sidebarPanel(
                     tabPanel("Ejercicio1",selectInput(inputId ='var', 
                                                       label="Eje X",
                                                       choices=c("chlorides","alcohol","fixed.acidity","volatile.acidity","citric.acid","residual.sugar","free.sulfur.dioxide","total.sulfur.dioxide","density","pH","sulphates","quality"))),
                     tabPanel("Ejercicio1",selectInput(inputId ="bar", 
                                                       label="Eje Y",
                                                       choices=c("alcohol","chlorides","fixed.acidity","volatile.acidity","citric.acid","residual.sugar","free.sulfur.dioxide","total.sulfur.dioxide","density","pH","sulphates","quality"))),
                     tabPanel("Ejercicio1",selectInput(inputId ="kar", 
                                                       label="Size",
                                                       choices=c("residual.sugar","chlorides","alcohol","volatile.acidity","citric.acid","fixed.acidity","free.sulfur.dioxide","total.sulfur.dioxide","density","pH","sulphates","quality"))),
                     
                     # Sidebar with a slider input for number of bins 
                         ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         plotOutput("distPlot", width = "100%")
                     )
                    )
                     
                 )),
                 tabPanel("Histogram",fluidPage(
                     
                     # Application title
                     titlePanel("Histogram"),
                     sidebarLayout(
                         sidebarPanel(
                             conditionalPanel(
                                 'input.dataset == "Wine"',
                             
                         
                            sliderInput("quality", "Quality:",
                                 min = 1, max = 10, value = 5
                            ),
                         
                        
                        tabPanel("Ejercicio1",selectInput(inputId ='tar', 
                                                       label="Attribute",
                                                       choices=c("chlorides","alcohol","fixed.acidity","volatile.acidity","citric.acid","residual.sugar","free.sulfur.dioxide","total.sulfur.dioxide","density","pH","sulphates"))),
                        sliderInput("bins",
                                 "Number of bins:",
                                 min = 1,
                                 max = 50,
                                 value = 20),
                         ),
                         ),
                        # Show a plot of the generated distribution
                        mainPanel(
                            tabsetPanel(
                                tabPanel("", plotOutput(outputId = "distPlottt"))
                            
                            #plotOutput("distPlottt", width = "100%")
                         )
                     )
                     )
                 )
                          ),
                 tabPanel("Heatmap",fluidPage(
                     
                     # App title ----
                     titlePanel("Affinity Propagation Heatmap"),
                     
                     # Sidebar layout with input and output definitions ----
                     sidebarLayout(
                         
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                             conditionalPanel(
                                 'input.dataset == "Wine"',
                                 checkboxGroupInput("show_vars", "Attributes which are selected for clustering:",
                                                    names(wine), selected = names(wine))
                             )
                         ),
                         
                         # Main panel for displaying outputs ----
                         mainPanel(
                             tabsetPanel(
                                 id = 'dataset',
                                 tabPanel("Wine", plotOutput(outputId = "distPlott"))
                             )
                         )
                     )
                 )
                 )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        datos<-read.csv("winequality-white.csv",sep=";")
        datos<-datos[1:100,]
        #x    <- faithful[, 2]
        #bins <- seq(min(x), max(x), length.out = input$bins + 1)
        # draw the histogram with the specified number of bins
        
        a<-dplyr::pull(datos[input$var], input$var)
        b<-dplyr::pull(datos[input$bar], input$bar)
        c<-dplyr::pull(datos[input$kar], input$kar)
       
        x<-as.numeric(a)
        y<-as.numeric(b)
        Size<-as.numeric(c)
        
       
        #plot_ly(data, x = a, y = b, text = ~quality, type = 'scatter', mode = 'markers', color = ~quality, colors = 'Reds',
        #              marker = list(size = ~residual.sugar, opacity = 0.5))
        #axis(2, at = c(5, 6, 7, 8, 9,10))
        ggplot(datos,aes(x=x,y=y,size=Size,color=quality))+geom_point(alpha=0.2)+scale_size(range=c(.1,30))+scale_color_gradient(low = 'blue', high = 'red')#+scale_y_continuous(breaks=seq(0, 3, 0.5))
        #plot(b)
        #plot(data$chlorides)
        
        
        
        
        
        #plot()
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
    },height = 600, width = 800 )
    output$distPlott <- renderPlot({
        vote = wine[,input$show_vars]
        a <- apcluster(negDistMat(r = 2), vote)
        heatmap(a)
        
    },height = 600, width = 800)
    output$distPlottt <- renderPlot({
        swine<- read.csv("winequality-white.csv", header = TRUE, sep = ";") 
        
        attribute <-input$tar
        swine <- subset(swine, quality == input$quality)
        att <- swine[, c(input$tar)]
        #hist(wine, col = 'salmon', border = 'white',breaks =9)
        ggplot(swine, aes(att)) +
            geom_histogram(bins = input$bins,
                           fill = "salmon",
                           colour = "grey30") +
            xlab("attribute") +
            theme_minimal()
        
    },height = 600, width = 800 )
   
}

# Run the application 
shinyApp(ui = ui, server = server)
