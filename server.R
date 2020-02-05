#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(plotly)
comb.sarc<-read.csv(file = "Combined dataset for SARC.csv",
                    header = TRUE,stringsAsFactors = FALSE)
comb.sarc <- as.data.frame(apply(comb.sarc,2,as.numeric))
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
       #-------------------plot2d regression model-------------------------------
    output$plot2dmodel <- renderPlotly ({
        
       #--0. Subset data according to user input
        subsetupper<-subset(comb.sarc,comb.sarc[,input$varx] > quantile(comb.sarc[,input$varx],input$Upper))
        subsetlower<-subset(comb.sarc,comb.sarc[,input$varx] <= quantile(comb.sarc[,input$varx],input$lower))

        #--a. dataset to use--
        if (input$up & input$low) {
            data<-comb.sarc
        }  else if (input$up) {
            data<-subsetupper
        }  else if (input$low) {
            data<-subsetlower
        }  else {
            data<-comb.sarc
        }
        
        #--b. log scare setting-- 
        #b1. x log
        if(input$logx){
            x<-log2(data[,input$varx])
        } else {
            x<-data[,input$varx]
        }      

        #b2. y log
        if(input$logy){
            y<-log2(data[,input$vary])
        } else {
            y<-data[,input$vary]
        }       

        #b3. color log
        if(input$logc){
            c<-log2(data[,input$varc])
        } else {
            c<-data[,input$varc]
        }             

        #--c.2d regression plot--    
        fit<-glm(y~x ,data=data)
        plot_ly(x=~x) %>%
            add_markers(y=~y,color = ~c) %>%
            add_lines(x=~x, y=fitted(fit))  %>%
            layout(showlegend=FALSE,
                   title=paste(input$vary,"~",input$varx,
                               sep="")
            ) %>%
            layout(xaxis=list(title=if(input$logx){paste("log(",input$varx,")")
                                                  } else{input$varx},
                              titlefont=list(
                                  family = "Courier New, monospace",
                                  size = 18,
                                  color = "#7f7f7f")
            ),
            yaxis=list(title=if(input$logy){paste("log(",input$vary,")")
                                           } else{input$vary},
                       titlefont=list(
                           family = "Courier New, monospace",
                           size = 18,
                           color = "#7f7f7f"))
            )
        
    })
    
    #Table with all the statistics of the regression model
    output$table2dmodel <- renderPlotly ({
        
        subsetupper<-subset(comb.sarc,comb.sarc[,input$varx] > quantile(comb.sarc[,input$varx],input$Upper))
        subsetlower<-subset(comb.sarc,comb.sarc[,input$varx] <= quantile(comb.sarc[,input$varx],input$lower))
        
        if (input$up & input$low) {
            data<-comb.sarc
        }  else if (input$up) {
            data<-subsetupper
        }  else if (input$low) {
            data<-subsetlower
        }  else {
            data<-comb.sarc
        }
        
        if(input$logx){
            x<-log2(data[,input$varx])
        } else {
            x<-data[,input$varx]
        }      
        if(input$logy){
            y<-log2(data[,input$vary])
        } else {
            y<-data[,input$vary]
        }       
        if(input$logc){
            c<-log2(data[,input$varc])
        } else {
            c<-data[,input$varc]
        }             

        fit<-glm(y~x -1,data=data)
        summary<-summary(fit)
        table<-signif(as.data.frame(summary$coef),digits=3)
        rownames(table)<-input$varx
        plot_ly(
            type = 'table',
            header = list(
                values = c("",names(table)),
                align = c('left', rep('center', ncol(table))),
                line = list(width = 1, color = 'black'),
                font = list(family = "Arial", size = 14)
            ),
            cells = list(
                values = rbind(
                    rownames(table), 
                    t(as.matrix(unname(table)))
                ),
                align = c('left', rep('center', ncol(table))),
                line = list(color = "black", width = 1),
                font = list(family = "Arial", size = 12, color = c("black"))
            )
        )
        
    })
    
  })
