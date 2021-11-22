
library(shiny)
library(aod)
library(ggplot2)
library(foreign)
library(nnet)
library(reshape2)

dt_output = function(title, id) {
    fluidRow(column(
        12, h1(paste0('Table ', sub('.*?([0-9]+)$', '\\1', id), ': ', title)),
        hr(), DTOutput(id)
    ))
}
render_dt = function(data, editable = 'cell', server = TRUE, ...) {
    renderDT(data, selection = 'none', server = server, editable = editable, ...)
}

shinyUI(fluidPage(
    
    headerPanel('Logistic Regression'),
    titlePanel(title = div(img(src="logo.png",align='right')), "Binomial Logit"),
    
    
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Upload Input data:", placeholder = "binary.csv"),
            
            htmlOutput("yvarselect"),
            htmlOutput("xvarselect"),
            htmlOutput("fxvarselect"),
            
            sliderInput('sample','Validation Sample Proportion',10,50,30),
            
        ),
        
        
        mainPanel(
            tabsetPanel(type = "tabs", 
                        
                        tabPanel("Overview",
                                 h4(p("Logistic Regression")),
                                 p("This type of analysis can help you predict 
                                   the likelihood of an event happening or a choice 
                                   being made. For example, you may want to know the 
                                   likelihood of a visitor choosing an offer 
                                   made on your website — or not (dependent variable). 
                                   Your analysis can look at known characteristics of visitors, 
                                   such as sites they came from, repeat visits to your site, 
                                   behavior on your site (independent variables). "),
                                   br(),
                                   p("Logistic regression models help you determine a
                                   probability of what type of visitors are likely 
                                   to accept the offer — or not. As a result, 
                                   you can make better decisions about promoting 
                                   your offer or make decisions about the offer itself."),
                                p(a("Source", href="https://www.ibm.com/topics/logistic-regression"), align="right"),
                                 h4(p("Download Sample Input File")),
                                 downloadButton('downloadData', 'Download Example file'),
                                 
                                 
                        ),
                        
                        tabPanel("Data", 
                                 DT::dataTableOutput('contents')
                         ),
                        
                        tabPanel("Summary",
                                 h4(p("Summary for Chosen Y variable:")),
                                 verbatimTextOutput('summaryY'),
                                 h4(p("Barplot for Chosen Y")),
                                 plotOutput('YVarPlot'),
                                 h4(p("Summary for Chosen X Variable(s):")),
                                 DT::dataTableOutput('summaryX'),
                                 h4(p("Two-way Contingency Table")),
                                 p('To ensure no 0 cells exist'),
                                 DT::dataTableOutput('CrossValidnTable')
                        ),
                        
                        tabPanel("Regression Results",
                                 h4(p("Coefficients")),
                                 DT::dataTableOutput('OLSResult'),
                                 h4(p("Variable Importance")),
                                 plotOutput('VarImp'),
                                 verbatimTextOutput('ConfMatrx')
                                
                        ),
                        tabPanel("Predicted Probabilities",
                                 h4("Output"),
                                 downloadButton('downloadData4', 
                                                'Download Output File'), br(),
                                 DT::dataTableOutput("Prob"))
            )
        )
    )))
