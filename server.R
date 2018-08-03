function(input, output, session) {
    
    # load parameters
    params = read.csv("parameters.csv")
    for(i in 1:nrow(params)) {
        row = params[i,]
        updateTextInput(session, row$name, value = row$value)
    }

    # load threshold limits
    TH.Limits = read.csv("indices.csv")
    for(i in 1:nrow(TH.Limits)) {
        if (grepl("-", TH.Limits$StartFrom[i])) {
            TH.Limits$StartFrom = as.Date(TH.Limits$StartFrom, "%Y-%m-%d")
            break
        }
        if (grepl("/", TH.Limits$StartFrom[i])) {
            TH.Limits$StartFrom = as.Date(TH.Limits$StartFrom, "%m/%d/%Y")
            break
        }
    }

    # load periods
    UpDownMkts = read.csv("periods.csv")
    for(i in 1:nrow(UpDownMkts)) {
        if (grepl("-", UpDownMkts$Start[i])) {
            UpDownMkts$Start = as.Date(UpDownMkts$Start, "%Y-%m-%d")
            UpDownMkts$End = as.Date(UpDownMkts$End, "%Y-%m-%d")
        }
        if (grepl("/", UpDownMkts$Start[i])) {
            UpDownMkts$Start = as.Date(UpDownMkts$Start, "%m/%d/%Y")
            UpDownMkts$End = as.Date(UpDownMkts$End, "%m/%d/%Y")
        }
    }
    
    # load variables for grid search
    vars = read.csv("variables.csv")
    
    rv <- reactiveValues(
        status = "",
        CH_status = "",
        DC_status = "",
        OPT_status = "",
        ### delete 3 lines below ###
        norm = rnorm(500), 
        unif = runif(500),
        chisq = rchisq(500, 2))

    #************* main backtest ***********#
    observeEvent(input$go, {
        disable("go")
        disable("save")
        session$sendCustomMessage(type = 'print', message = list(selector = 'status', html = "processing..."))
        result = processData(input$RootFolder, input$CS_UseFile, input$TH_UseFile, input$ConstructModelPort, input$ConstructModelPortMethod, FALSE, NULL, session) 
        session$sendCustomMessage(type = 'print', message = list(selector = 'status', html = result))
        enable("save")
        enable("go")
    })
    
    observeEvent(input$save, {
        df = AllInputs()
        params$value = df[match(params$name, df$name),2]
        write.table(params, file = "parameters.csv", sep = ",", col.names = TRUE, row.names = FALSE)
        write.table(indices_data(), file = "indices.csv", sep = ",", col.names = TRUE, row.names = FALSE)
        write.table(periods_data(), file = "periods.csv", sep = ",", col.names = TRUE, row.names = FALSE)
        session$sendCustomMessage(type = 'print', message = list(selector = 'status', html = "saved!"))
    })

    #************* current holdings ***********#
    observeEvent(input$CH_go, {
        disable("CH_go")
        session$sendCustomMessage(type = 'print', message = list(selector = 'CH_status', html = "processing..."))
        result = processData(input$RootFolder, input$CH_CS_UseFile, input$CH_TH_UseFile, input$CH_ConstructModelPort, input$CH_ConstructModelPortMethod, TRUE, input$CH_OpinionDate, session) 
        session$sendCustomMessage(type = 'print', message = list(selector = 'CH_status', html = result))
        enable("CH_go")
    })
    
    #************* data check ***********#
    observeEvent(input$DC_go, {
        disable("DC_go")
        session$sendCustomMessage(type = 'print', message = list(selector = 'DC_status', html = "processing..."))
        result = checkData(input$RootFolder, session) 
        session$sendCustomMessage(type = 'print', message = list(selector = 'DC_status', html = result))
        enable("DC_go")
    })
    
    #************* grid search ***********#
    observeEvent(input$OPT_go, {
        disable("OPT_go")
        session$sendCustomMessage(type = 'print', message = list(selector = 'OPT_status', html = "processing..."))
        result = gridSearch(input$RootFolder, input$ConstructModelPortMethod, session) 
        session$sendCustomMessage(type = 'print', message = list(selector = 'OPT_status', html = result))
        enable("OPT_go")
    })

    observeEvent(input$OPT_save, {
        write.table(variables_data(), file = "variables.csv", sep = ",", col.names = TRUE, row.names = FALSE)
        session$sendCustomMessage(type = 'print', message = list(selector = 'OPT_status', html = "saved!"))
    })
    
    # update status
    output$status <- renderText({rv$status})
    output$CH_status <- renderText({rv$CH_status})
    output$DC_status <- renderText({rv$DC_status})
    output$OPT_status <- renderText({rv$OPT_status})
    
    #************* indices editor ***********#
    indices_values = reactiveValues()
    
    indices_data = reactive({
        if (!is.null(input$indices)) {
            DF = hot_to_r(input$indices)
        } else {
            if (is.null(indices_values[["DF"]]))
                DF = TH.Limits
            else
                DF = indices_values[["DF"]]
        }
        
        indices_values[["DF"]] = DF
        DF
    })
    
    output$indices <- renderRHandsontable({
        DF = indices_data()
        if (!is.null(DF))
            rhandsontable(DF, stretchH = "all") %>%
            hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    })
    
    #************* periods editor ***********#
    periods_values = reactiveValues()
    
    periods_data = reactive({
        if (!is.null(input$periods)) {
            DF = hot_to_r(input$periods)
        } else {
            if (is.null(periods_values[["DF"]]))
                DF = UpDownMkts
            else
                DF = periods_values[["DF"]]
        }
        
        periods_values[["DF"]] = DF
        DF
    })
    
    output$periods <- renderRHandsontable({
        DF = periods_data()
        DF$Name = as.character(DF$Name)
        if (!is.null(DF))
            rhandsontable(DF, stretchH = "all") %>%
            hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    })
    
    #************* variables editor ***********#
    variables_values = reactiveValues()
    
    variables_data = reactive({
        if (!is.null(input$variables)) {
            DF = hot_to_r(input$variables)
        } else {
            if (is.null(variables_values[["DF"]]))
                DF = vars
            else
                DF = variables_values[["DF"]]
        }
        
        variables_values[["DF"]] = DF
        DF
    })
    
    output$variables <- renderRHandsontable({
        DF = variables_data()
        if (!is.null(DF))
            rhandsontable(DF, stretchH = "all") %>%
            hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    })
    
    
    
        
    #************* gather all inputs besides tables ***********#
    AllInputs <- reactive({
        x <- reactiveValuesToList(input)
        x <- x[names(x)!="indices" & names(x)!="periods" & names(x)!="variables"]
        data.frame(
            name = names(x),
            value = unlist(x, use.names = FALSE)
        )
    })
    

    #************* to be deleted after this ***********#
    
    observeEvent(input$renorm, { rv$norm <- rnorm(500) })
    observeEvent(input$reunif, { rv$unif <- runif(500) })
    observeEvent(input$rechisq, { rv$chisq <- rchisq(500, 2) })
    
    
    output$norm <- renderPlot({
        hist(rv$norm, breaks = 30, col = "grey", border = "white",
             main = "500 random draws from a standard normal distribution")
    })
    output$unif <- renderPlot({
        hist(rv$unif, breaks = 30, col = "grey", border = "white",
             main = "500 random draws from a standard uniform distribution")
    })
    output$chisq <- renderPlot({
        hist(rv$chisq, breaks = 30, col = "grey", border = "white",
             main = "500 random draws from a Chi Square distribution with two degree of freedom")
    })
    
}
