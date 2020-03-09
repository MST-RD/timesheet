#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(readxl)
library(ggplot2)
library(shiny)


#R-shiny code
ui <- fluidPage(
    
    # App title ----
    titlePanel("工时数据分析"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            #读入文件
            fileInput("xlsx", "Choose File"),
            #空一行
            tags$hr(),
            
            #Input: Select the group of group ----
            uiOutput("checkbox")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Boxplot ----
            plotOutput(outputId = "boxPlot")
            
        )
    )
)
