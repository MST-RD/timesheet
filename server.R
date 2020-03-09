#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(readxl)
library(ggplot2)
library(shiny)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
    
    # Boxplot of the Group timesheet ----
    # with requested group
    # This expression that generates a boxplot is wrapped in a call
    # to renderPlot to indicate that:
    #
    # 1. It is "reactive" and therefore should be automatically
    #    re-executed when inputs (input$bins) change
    # 2. Its output type is a plot
    output$boxPlot <- renderPlot({
        
        ggplot(dataset()[which(dataset()$x3==input$datasetSelector),], aes(as.factor(x1))) + geom_bar(aes(fill=x5, weight=x16), position = "stack") +
            scale_y_continuous(breaks=seq(0,64,8)) +
            theme(text = element_text(family = 'SimHei', face = "bold"),legend.text = element_text(size=10))+
            labs(fill="Sponsor") + ylab("Hour") + xlab(NULL) +
            theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12)) + coord_flip()
        
        
    })
    
    #动态创建个数据框
    dataset <- reactive({
        readdata <- read_excel(input$xlsx$datapath)
        cnames=paste("x",1:length(readdata),sep="")
        colnames(readdata)=cnames
        return(readdata)
        
    })
    
    
    
    #动态创建个链表
    fileOptions <- reactiveValues(currentOptions=c())
    
    #动态处理，把input的excel的group与链表连接
    observeEvent(input$xlsx, {
        fileOptions$currentOptions = c(
            fileOptions$currentOptions, 
            unique(dataset()$x3)
        )
    })
    
    #创建动态选择栏
    output$checkbox<-renderUI({
        selectInput("datasetSelector","Group:", 
                    choices  = fileOptions$currentOptions,
                    selected = fileOptions$currentOptions)
    })
    
    
}
