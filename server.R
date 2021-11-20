

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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    
    myData <- reactive({
        inFile <- input$file
        if (is.null(inFile)) return(NULL)
        data <- read.csv(inFile$datapath, header = TRUE)
        data
    })
    
    output$contents <- DT::renderDataTable({
        DT::datatable(myData())       
    })
    
    output$yvarselect <- renderUI({
        if (is.null(input$file)) {return(NULL)}
        
        selectInput("yAttr", "Select Y variable",multiple = FALSE,
                    selectize = TRUE,
                    colnames(myData()))
        
    })
    
    output$xvarselect <- renderUI({
        if (identical(myData(), '') || identical(myData(),data.frame())) return(NULL)
        
        selectInput("xAttr", label = "Select X variables",
                           multiple = TRUE, selectize = TRUE,
                    selected = setdiff(colnames(myData()),input$yAttr),choices = setdiff(colnames(myData()),input$yAttr)
        )
        
    })
    
    output$fxvarselect <- renderUI({
        if (identical(myData(), '') || identical(myData(),data.frame())) return(NULL)
        
        selectInput("fxAttr", label = "Select X(Factor) variables",multiple = TRUE,
                    selectize = TRUE,
                    selected = setdiff(colnames(myData()),input$yAttr),choices = setdiff(colnames(myData()),input$yAttr)
        )
        
    })
    
    output$summaryY <- renderPrint({
        df <- myData()
        print("Summary for Selected Y variable(s).")
        summary(df[,input$yAttr])
        
    })
    
    output$YVarPlot <- renderPlot(
        if (is.null(input$file)) {return(NULL)}
        else{
            barplot(table(myData()[input$yAttr]))
            
        }
    )
    
    output$summaryX <- DT::renderDataTable({
        df <- myData()
        print("Summary for Selected X variable(s).")
        
        DT::datatable(do.call(cbind, lapply(df[, input$xAttr], summary)))
        
    })    
    
    
    output$CrossValidnTable <- renderDataTable(
        if (is.null(input$file)) {return(NULL)}
        else{
        df <- myData()
        #f <- as.formula(~input$yAttr + input$fxAttr)
        DT::datatable(xtabs(as.formula(paste0("~",input$fxAttr,"+",input$yAttr)), data = df))
    })
    
    output$OLSResult <- DT::renderDataTable({
        x <-input$xAttr
        y <- input$yAttr
        fx <- input$fxAttr
        
        # Reformulate command concatenates termlabels with + in between 
        # with response as dependent variable as a formula; 
        # to force factor variables to be read as.factor() 
        # is attached to the list of fx variables
        
        
        for (i0 in (which(x %in% fx == TRUE))){x[i0] <- paste('as.factor(',x[i0],')')}
        #f <- as.formula(paste(y, paste(x, collapse = '+'), sep = " ~ "))
        
        #f <- reformulate(termlabels = c(x), response = y)
        
        f <- paste(paste(y, collapse = "+"),'~', paste(x, collapse = "+"))
        
        fit_ols <- summary(glm(f, myData(), family = "binomial"))
        
        DT::datatable(round(fit_ols$coefficients,3))
    })
    
    #d1 = iris
    
    #output$x1 = render_dt(d1, 'cell', FALSE)
    
    #observe(str(input$x1_cell_edit))
    
    output$downloadData <- downloadHandler(
        filename = function() { "binary.csv" },
        content = function(file) {
            write.csv(read.csv("data/binary.csv"), file, row.names=F, col.names=F)
        }
    )
})
