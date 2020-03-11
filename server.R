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
library(scales)
library(RColorBrewer)

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
        
        ggplot(dataset()[[1]], aes(x=中文名, fill=申办方, weight=工时)) + geom_bar(stat="count", position = "stack") +
            geom_text(stat='count',aes(label=..count..), position=position_stack(0.5)) +
            scale_y_continuous(breaks=breaks_width(ybreak())) +
            guides(fill = guide_legend(reverse = T)) +
            theme(text = element_text(family = 'simhei', face = "bold"),legend.text = element_text(size=10))+
            labs(fill="Sponsor") + ylab("Hour") + xlab(NULL) + coord_flip() +
            theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12)) +
            geom_hline(yintercept = 40, linetype="dashed") +
            scale_fill_manual(values = color()) +
            theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
            
        
        
    })
    #issue :breaks_width(8) or 40, 如果加上了取8或者取40的判断，则不需要expand = c(0,0)
    
    #动态创建个数据框
    dataset <- reactive({
        readdata <- read_excel(input$xlsx$datapath)
        #处理申办方为NA的情况
        
        order_sp <- c('not Bill hour',sort(unique(readdata$申办方)))
        readdata$申办方[is.na(readdata$申办方)] <- 'not Bill hour'  
        
        #cnames=paste("x",1:length(readdata),sep="")
        #colnames(readdata)=cnames
        dataend <- readdata[which(readdata$人员组别==input$datasetSelector),]
        outdata <- list(dataend,order_sp)
        return(outdata)
        
    })
    
    #根据Y轴最大值来判断坐标间隔
    ybreak <- reactive({
        sumhour <- aggregate(dataset()[[1]]$工时, list(中文名 = dataset()[[1]]$中文名), sum)
        maxhour <- max(sumhour$x)
        if (maxhour>80) {return(40)}
        else {return(8)}
    })
    
    #color
    color <- reactive({     
        
        #Get sponsor
        sponsor <- as.data.frame(dataset()[[2]])
        
        #Get color
        sponsor_col <- merge(sponsor, top10_cols, by.x = "order_sp", by.y = "top10", all.x = T, sort = F) 
        
        #assign color
        sp_colna_n <- length(sponsor_col[is.na(sponsor_col$cols),1])
        getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
        sponsor_col[is.na(sponsor_col$cols),]$cols <- getPalette(sp_colna_n)
        
        #Get order color
        sponsor_col$order_sp <- factor(sponsor_col$order_sp,level=rev(order_sp))
        sponsor_col <- sponsor_col[order(sponsor_col$order_sp),]
        col <- sponsor_col$cols
        return(col)

        })
    
    
    #动态创建个链表
    fileOptions <- reactiveValues(currentOptions=c())
    
    #动态处理，把input的excel的group与链表连接
    observeEvent(input$xlsx, {
        fileOptions$currentOptions = c(
            fileOptions$currentOptions, 
            unique(read_excel(input$xlsx$datapath)$人员组别)
        )
    })
    
    #创建动态选择栏
    output$checkbox<-renderUI({
        selectInput("datasetSelector","Group:", 
                    choices  = fileOptions$currentOptions,
                    selected = fileOptions$currentOptions)
    })
    
    
}
