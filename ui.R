#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(plotly)
library(ggpubr)
library(grid)

comb.sarc<-read.csv(file = "Combined dataset for SARC.csv",
                    header = TRUE,stringsAsFactors = FALSE)
comb.sarc <- apply(comb.sarc,2,as.numeric)
droplist<-colnames(comb.sarc)

# Define UI for application that draw the 2d regression plot
shinyUI(fluidPage(
    
    # Application title
    titlePanel("TCGA-CDR data Analysis"),
    

    sidebarLayout(
        sidebarPanel(
#########################################################################        
###1. User can decide whether they want to look at the whole dataset  ###
###Or a data subset by percentile of variable x                       ###
#########################################################################

    # Sidebar with a slider input for upper percentile of the dataset we are looking at            
            sliderInput("Upper",
                        "Upper percentile of variable x:\n (please remember to tick the box below)",
                        min = 0,
                        max = 0.5,
                        value = 0.5),
    # Checkbox for user to decide wether only this subset of data is used for analysis
            checkboxInput("up","Use this subset(upper percentile) only",value=FALSE),
    
    # Sidebar with a slider input for lower percentile of the dataset we are looking at        
            sliderInput("lower",
                        "Lower percentile of variable x:\n (please remember to tick the box below)",
                        min = 0,
                        max = 0.5,
                        value = 0.5),
    # Checkbox to decide wether only this subset of data is used for analysis
            checkboxInput("low","Use this subset(lower percentile) only",value=FALSE
            ),

################################################################################        
###2. User can define the variable displayed on the x/y-axis                 ###
###They can also define the variable on which thecolor of the markers depend ###
################################################################################

        #Dropdown list for variable x
            selectInput("varx", 
                        label = "Choose the x-variable",
                        choices = droplist,
                        selected = "gene.HNRNPA1"),
        #Checkbox to decide whether or not to plot x on log scale
            checkboxInput("logx","x on log scale", value=FALSE
            ),
        
        #Dropdown list for variable Y
            selectInput("vary", 
                        label = "Choose the y-variable",
                        choices = droplist,
                        selected = "gene.TP53"),
        #Checkbox to decide whether or not to plot y on log scale
            checkboxInput("logy","y on log scale", value=FALSE
            ),

        #Dropdown list for color variable
            selectInput("varc", 
                        label = "Choose the color",
                        choices = droplist,
                        selected = "OS.time"),   
        #Checkbox to decide whether or not to plot color on log scale
            checkboxInput("logc","color on log scale", value=FALSE
            )
        ),
        
        
        # Show a plot of the regression model along with a table of statistics
        mainPanel(plotlyOutput("plot2dmodel"),
                  plotlyOutput("table2dmodel")
                  
                  
        )
    )
))
