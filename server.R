

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
    
    pred.readdata <- reactive({
        if (is.null(input$filep)) { return(NULL) }
        else{
            readdata <- as.data.frame(read.csv(input$filep$datapath ,header=TRUE, sep = ","))
            return(readdata)
        }
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
        #print("Summary for Selected X variable(s).")
        
        DT::datatable(t(do.call(cbind, lapply(df[, input$xAttr], summary))))
        
    })    
    
    
    output$CrossValidnTable <- renderDataTable(
        if (is.null(input$file)) {return(NULL)}
        else{
            df <- myData()
            #f <- as.formula(~input$yAttr + input$fxAttr)
            DT::datatable(xtabs(as.formula(paste0("~",input$fxAttr,"+",input$yAttr)), data = df))
        })
    
    testsample =  reactive({
        set.seed(12345)
        sample(1:nrow(myData()), round(nrow(myData())*((input$sample)/100)))
    })
    
    train_data = reactive({
        myData()[-testsample(),]
    })
    
    test_data = reactive({
        myData()[testsample(),]
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
        
        f <- as.formula(paste(paste(y, collapse = "+"),'~', paste(x, collapse = "+")))
        
        fit_ols <- summary(multinom(f, data = as.data.frame(train_data())))
        
        a <- t(fit_ols$coefficients)
        colnames(a) <- paste("Coeff", colnames(a), sep = "_")
        
        b <- pnorm(t((fit_ols$coefficients)/(fit_ols$standard.errors)))
        
        colnames(b) <- paste("PVal", colnames(b), sep = "_")
        
        res_table <- cbind(a,b)
        
        Coeff_0 <- matrix(0, dim(res_table)[1])
        
        res_table <- cbind(Coeff_0, res_table)
        colnames(res_table)[1] <- "Coeff_1"
        DT::datatable(round(res_table,3))
    })
    
    
    
    output$VarImp <- renderPlot({
        x <-input$xAttr
        y <- input$yAttr
        fx <- input$fxAttr
        
        
        for (i0 in (which(x %in% fx == TRUE))){x[i0] <- paste('as.factor(',x[i0],')')}
        f <- as.formula(paste(paste(y, collapse = "+"),'~', paste(x, collapse = "+")))
        
        fit_ols <- rpart(f, data = as.data.frame(train_data()))
        
        df <- as.data.frame(fit_ols$variable.importance)
        
        barplot(fit_ols$variable.importance, las = 2)
        
    })
    
    
    
    output$ConfMatrx <- renderPrint({
        x <-input$xAttr
        
        y <- input$yAttr
        
        fx <- input$fxAttr
        
        set.seed(12345)
        
        for (i0 in (which(x %in% fx == TRUE))){x[i0] <- paste('as.factor(',x[i0],')')}
        f <- as.formula(paste(paste(y, collapse = "+"),'~', paste(x, collapse = "+")))
        
        fit <- multinom(f, data = as.data.frame(train_data()), trace = FALSE)
        
        predicted_Y <- predict(fit, test_data(), type = "class")
        Actual_Y <- test_data()[,y]
        
        xtab <- table(predicted_Y, Actual_Y)
        
        xtab
        
    })
    
    output$accuracy <-renderPrint({
        x <-input$xAttr
        
        y <- input$yAttr
        
        fx <- input$fxAttr
        
        set.seed(12345)
        
        for (i0 in (which(x %in% fx == TRUE))){x[i0] <- paste('as.factor(',x[i0],')')}
        f <- as.formula(paste(paste(y, collapse = "+"),'~', paste(x, collapse = "+")))
        
        fit <- multinom(f, data = as.data.frame(train_data()), trace = FALSE)
        
        training_pred <- predict(fit, test_data(), type = "class")
        truth <- test_data()[,y]
        
        xtab <- table(training_pred, truth)
        
        accuracy = (sum(diag(xtab))/sum(xtab))*100
        
        accuracy
    })
    
    output$Prob <- DT::renderDataTable({
        x <-input$xAttr
        y <- input$yAttr
        fx <- input$fxAttr
        
        for (i0 in (which(x %in% fx == TRUE))){x[i0] <- paste('as.factor(',x[i0],')')}
        f <- as.formula(paste(paste(y, collapse = "+"),'~', paste(x, collapse = "+")))
        
        fit_ols <- summary(multinom(f, data = as.data.frame(train_data())))
        
        predicted_Y <- max.col(fitted(fit_ols))
        
        result <- round(fitted(fit_ols),3)
        colnames(result) <- paste("Y ", colnames(result), sep = "=")
        t0 <- cbind(result, predicted_Y)
        
        DT::datatable(t0)
    })
    
    output$Prob2 <- DT::renderDataTable({
        x <-input$xAttr
        y <- input$yAttr
        fx <- input$fxAttr
        
        for (i0 in (which(x %in% fx == TRUE))){x[i0] <- paste('as.factor(',x[i0],')')}
        f <- as.formula(paste(paste(y, collapse = "+"),'~', paste(x, collapse = "+")))
        
        fit_ols <- summary(multinom(f, data = as.data.frame(test_data())))
        
        predicted_Y <- max.col(fitted(fit_ols))
        
        result <- round(fitted(fit_ols),3)
        colnames(result) <- paste("Y ", colnames(result), sep = "=")
        t0 <- cbind(result, predicted_Y)
        
        DT::datatable(t0)
    })
    

    output$prediction =  renderDataTable({
        if (is.null(input$filep)) {return(NULL)}
        x <-input$xAttr
        
        y <- input$yAttr
        
        fx <- input$fxAttr
        
        set.seed(12345)
        
        for (i0 in (which(x %in% fx == TRUE))){x[i0] <- paste('as.factor(',x[i0],')')}
        f <- as.formula(paste(paste(y, collapse = "+"),'~', paste(x, collapse = "+")))
        
        fit <- multinom(f, data = as.data.frame(train_data()), trace = FALSE)
        
        training_pred <- predict(fit, as.data.frame(pred.readdata()), type = "probs")
        
        colnames(training_pred) <- paste("Probability that Y ", colnames(training_pred), sep = "=")
        
        result <- round(training_pred,4)
        
        predicted_Y <- max.col(training_pred)
        
        
        t0 <- cbind(result, predicted_Y)
        
        t0
    })
    
    
        
    output$downloadData4 <- downloadHandler(
        filename = function() { "logit_output_test.csv" },
        content = function(file) {
            
            x <-input$xAttr
            y <- input$yAttr
            fx <- input$fxAttr
            
            
            for (i0 in (which(x %in% fx == TRUE))){x[i0] <- paste('as.factor(',x[i0],')')}
            f <- as.formula(paste(paste(y, collapse = "+"),'~', paste(x, collapse = "+")))
            
            fit_ols <- summary(multinom(f, data = as.data.frame(test_data())))
            Predicted_Y <- max.col(fitted(fit_ols))
            
            result <- round(fitted(fit_ols),3)
            
            t0 <- cbind(result, Predicted_Y)
            
            write.csv(t0, file, row.names=F)
        }
    )
    
    output$downloadData5 <- downloadHandler(
        filename = function() { "logit_output_train.csv" },
        content = function(file) {
            
            x <-input$xAttr
            y <- input$yAttr
            fx <- input$fxAttr
            
            
            for (i0 in (which(x %in% fx == TRUE))){x[i0] <- paste('as.factor(',x[i0],')')}
            f <- as.formula(paste(paste(y, collapse = "+"),'~', paste(x, collapse = "+")))
            
            fit_ols <- summary(multinom(f, data = as.data.frame(train_data())))
            Predicted_Y <- max.col(fitted(fit_ols))
            
            result <- round(fitted(fit_ols),3)
            
            t0 <- cbind(result, Predicted_Y)
            
            write.csv(t0, file, row.names=F)
        }
    )
    
    
    
    
})
