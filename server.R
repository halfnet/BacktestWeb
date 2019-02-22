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
    
    # load DtS buckets for addtional tests
    dtsbuckets = read.csv("dtsbuckets.csv")

    # load DtS buckets 2 for addtional tests
    dtsbuckets2 = read.csv("dtsbuckets2.csv")
    
    # load DtS buckets 3 for addtional tests
    dtsbuckets3 = read.csv("dtsbuckets3.csv")

    # load rules for portfolio construction
    portrules = read.csv("portrules.csv")

    # load dts weights for portfolio construction
    portweights_dts = read.csv("portweights_dts.csv")

    # load val weights for portfolio construction
    portweights_val = read.csv("portweights_val.csv")

    # load vnd weights for portfolio construction
    portweights_vnd = read.csv("portweights_vnd.csv")

    final = NULL
    metrics = NULL
    monthlystats = NULL
    bondgraph_periods = NULL
    marketcycles = NULL
    
    rv <- reactiveValues(
        status = "",
        CH_status = "",
        DC_status = "",
        OPT_status = "",
        TEST_status = "",
        dtshist_status = "",
        dtshist = NULL, 
        dtsbreak = NULL,
        dtsxlim = NULL,
        trkerr_status = "",
        trkerr = NULL,
        trkerr_years = NULL,
        trkerr_ds = NULL,
        sumstats_ds = NULL,
        sumstats_status = "",
        bondgraph = NULL,
        bondgraph_status = "",
        marketcyclegraph = NULL,
        marketcycle_status = "")

    #************* main backtest ***********#
    observeEvent(input$go, {
        disable("go")
        disable("save")
        disable("equity_opinion")
        disable("ai_export")
        session$sendCustomMessage(type = 'print', message = list(selector = 'status', html = "processing..."))
        result = processData(input$RootFolder, input$CS_UseFile, input$TH_UseFile, input$TH_NoLimits, input$TH_UsePrevMonth, input$ConstructModelPort, input$ConstructModelPortMethod, input$PortWeightMethod, input$TEST_VAL_METRIC, FALSE, NULL, session) 
        session$sendCustomMessage(type = 'print', message = list(selector = 'status', html = result))
        enable("save")
        enable("go")
        enable("equity_opinion")
        enable("ai_export")
    })
    
    observeEvent(input$save, {
        df = AllInputs()
        params$value = df[match(params$name, df$name),2]
        write.table(params, file = "parameters.csv", sep = ",", col.names = TRUE, row.names = FALSE)
        write.table(indices_data(), file = "indices.csv", sep = ",", col.names = TRUE, row.names = FALSE)
        write.table(portrules_data(), file = "portrules.csv", sep = ",", col.names = TRUE, row.names = FALSE)
        write.table(portweights_dts_data(), file = "portweights_dts.csv", sep = ",", col.names = TRUE, row.names = FALSE)
        write.table(portweights_val_data(), file = "portweights_val.csv", sep = ",", col.names = TRUE, row.names = FALSE)
        write.table(portweights_vnd_data(), file = "portweights_vnd.csv", sep = ",", col.names = TRUE, row.names = FALSE)
        write.table(periods_data(), file = "periods.csv", sep = ",", col.names = TRUE, row.names = FALSE)
        session$sendCustomMessage(type = 'print', message = list(selector = 'status', html = "saved!"))
    })
    
    observeEvent(input$equity_opinion, {
        disable("go")
        disable("save")
        disable("equity_opinion")
        disable("ai_export")
        session$sendCustomMessage(type = 'print', message = list(selector = 'status', html = "processing..."))
        result = generateEquityOpinion(input$RootFolder, session) 
        session$sendCustomMessage(type = 'print', message = list(selector = 'status', html = result))
        enable("save")
        enable("go")
        enable("equity_opinion")
        enable("ai_export")
    })
    
    observeEvent(input$ai_export, {
        disable("go")
        disable("save")
        disable("equity_opinion")
        disable("ai_export")
        session$sendCustomMessage(type = 'print', message = list(selector = 'status', html = "processing..."))
        result = generateOutput4AI(input$RootFolder, session) 
        session$sendCustomMessage(type = 'print', message = list(selector = 'status', html = result))
        enable("save")
        enable("go")
        enable("equity_opinion")
        enable("ai_export")
    })

    observeEvent(input$mkt_cycle, {
        disable("go")
        disable("save")
        disable("equity_opinion")
        disable("ai_export")
        session$sendCustomMessage(type = 'print', message = list(selector = 'status', html = "processing..."))
        result = generateBullBearMkt(input$RootFolder, input$mkt_cycle_metric, session) 
        session$sendCustomMessage(type = 'print', message = list(selector = 'status', html = result))
        enable("save")
        enable("go")
        enable("equity_opinion")
        enable("ai_export")
    })
    
    #************* current holdings ***********#
    observeEvent(input$CH_go, {
        disable("CH_go")
        session$sendCustomMessage(type = 'print', message = list(selector = 'CH_status', html = "processing..."))
        result = processData(input$RootFolder, FALSE, FALSE, input$TH_NoLimits, input$TH_UsePrevMonth, input$ConstructModelPort, input$ConstructModelPortMethod, input$PortWeightMethod, input$TEST_VAL_METRIC, TRUE, input$CH_OpinionDate, session) 
        session$sendCustomMessage(type = 'print', message = list(selector = 'CH_status', html = result))
        enable("CH_go")
    })
    
    #************* data check ***********#
    observeEvent(input$DC_go, {
        disable("DC_go")
        disable("DC_recycled")
        session$sendCustomMessage(type = 'print', message = list(selector = 'DC_status', html = "processing..."))
        result = checkData(input$RootFolder, session) 
        session$sendCustomMessage(type = 'print', message = list(selector = 'DC_status', html = result))
        enable("DC_go")
        enable("DC_recycled")
    })

    observeEvent(input$DC_recycled, {
        disable("DC_go")
        disable("DC_recycled")
        session$sendCustomMessage(type = 'print', message = list(selector = 'DC_status', html = "processing..."))
        result = findRecycledTickers(input$RootFolder, session) 
        session$sendCustomMessage(type = 'print', message = list(selector = 'DC_status', html = result))
        enable("DC_go")
        enable("DC_recycled")
    })
    
    #************* grid search ***********#
    observeEvent(input$OPT_go, {
        disable("OPT_go")
        session$sendCustomMessage(type = 'print', message = list(selector = 'OPT_status', html = "processing..."))
        result = gridSearch(input$RootFolder, input$TH_NoLimits, input$TH_UsePrevMonth, input$ConstructModelPortMethod, input$PortWeightMethod, input$TEST_VAL_METRIC, session) 
        session$sendCustomMessage(type = 'print', message = list(selector = 'OPT_status', html = result))
        enable("OPT_go")
    })

    observeEvent(input$OPT_save, {
        write.table(variables_data(), file = "variables.csv", sep = ",", col.names = TRUE, row.names = FALSE)
        session$sendCustomMessage(type = 'print', message = list(selector = 'OPT_status', html = "saved!"))
    })
    
    #************* additional tests ***********#
    observeEvent(input$TEST_run, {
        disable("TEST_run")
        session$sendCustomMessage(type = 'print', message = list(selector = 'TEST_status', html = "processing..."))
        result = addlTests(input$RootFolder, session, input$TEST_VAL_METRIC) 
        session$sendCustomMessage(type = 'print', message = list(selector = 'TEST_status', html = result))
        enable("TEST_run")
    })
    
    observeEvent(input$TEST_save, {
        df = AllInputs()
        params$value = df[match(params$name, df$name),2]
        write.table(params, file = "parameters.csv", sep = ",", col.names = TRUE, row.names = FALSE)
        write.table(dtsbuckets_data(), file = "dtsbuckets.csv", sep = ",", col.names = TRUE, row.names = FALSE)
        write.table(dtsbuckets2_data(), file = "dtsbuckets2.csv", sep = ",", col.names = TRUE, row.names = FALSE)
        write.table(dtsbuckets3_data(), file = "dtsbuckets3.csv", sep = ",", col.names = TRUE, row.names = FALSE)
        session$sendCustomMessage(type = 'print', message = list(selector = 'TEST_status', html = "saved!"))
    })
    
    #************* update status ***********#
    output$status <- renderText({rv$status})
    output$CH_status <- renderText({rv$CH_status})
    output$DC_status <- renderText({rv$DC_status})
    output$OPT_status <- renderText({rv$OPT_status})
    output$TEST_status <- renderText({rv$TEST_status})
    output$dtshist_status <- renderText({rv$dtshist_status})
    output$trkerr_status <- renderText({rv$trkerr_status})
    output$sumstats_status <- renderText({rv$sumstats_status})
    output$bondgraph_status <- renderText({rv$bondgraph_status})
    
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
    
    
    #************* dtsbuckets editor ***********#
    dtsbuckets_values = reactiveValues()
    
    dtsbuckets_data = reactive({
        if (!is.null(input$dtsbuckets)) {
            DF = hot_to_r(input$dtsbuckets)
        } else {
            if (is.null(dtsbuckets_values[["DF"]]))
                DF = dtsbuckets
            else
                DF = dtsbuckets_values[["DF"]]
        }
        
        dtsbuckets_values[["DF"]] = DF
        DF
    })
    
    output$dtsbuckets <- renderRHandsontable({
        DF = dtsbuckets_data()
        DF$Label = as.character(DF$Label)
        if (!is.null(DF))
            rhandsontable(DF, stretchH = "all") %>%
            hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    })
    
    #************* dtsbuckets2 editor ***********#
    dtsbuckets2_values = reactiveValues()
    
    dtsbuckets2_data = reactive({
        if (!is.null(input$dtsbuckets2)) {
            DF = hot_to_r(input$dtsbuckets2)
        } else {
            if (is.null(dtsbuckets2_values[["DF"]]))
                DF = dtsbuckets2
            else
                DF = dtsbuckets2_values[["DF"]]
        }
        
        dtsbuckets2_values[["DF"]] = DF
        DF
    })
    
    output$dtsbuckets2 <- renderRHandsontable({
        DF = dtsbuckets2_data()
        DF$Label = as.character(DF$Label)
        if (!is.null(DF))
            rhandsontable(DF, stretchH = "all") %>%
            hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    })
    
    #************* dtsbuckets3 editor ***********#
    dtsbuckets3_values = reactiveValues()
    
    dtsbuckets3_data = reactive({
        if (!is.null(input$dtsbuckets3)) {
            DF = hot_to_r(input$dtsbuckets3)
        } else {
            if (is.null(dtsbuckets3_values[["DF"]]))
                DF = dtsbuckets3
            else
                DF = dtsbuckets3_values[["DF"]]
        }
        
        dtsbuckets3_values[["DF"]] = DF
        DF
    })
    
    output$dtsbuckets3 <- renderRHandsontable({
        DF = dtsbuckets3_data()
        DF$Label = as.character(DF$Label)
        if (!is.null(DF))
            rhandsontable(DF, stretchH = "all") %>%
            hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    })
    
    
    #************* portrules editor ***********#
    portrules_values = reactiveValues()
    
    portrules_data = reactive({
        if (!is.null(input$portrules)) {
            DF = hot_to_r(input$portrules)
        } else {
            if (is.null(portrules_values[["DF"]]))
                DF = portrules
            else
                DF = portrules_values[["DF"]]
        }
        
        portrules_values[["DF"]] = DF
        DF
    })
    
    output$portrules <- renderRHandsontable({
        DF = portrules_data()
        if (!is.null(DF))
            rhandsontable(DF, stretchH = "all") %>%
            hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    })

    #************* portweights_dts editor ***********#
    portweights_dts_values = reactiveValues()
    
    portweights_dts_data = reactive({
        if (!is.null(input$portweights_dts)) {
            DF = hot_to_r(input$portweights_dts)
        } else {
            if (is.null(portweights_dts_values[["DF"]]))
                DF = portweights_dts
            else
                DF = portweights_dts_values[["DF"]]
        }
        
        portweights_dts_values[["DF"]] = DF
        DF
    })
    
    output$portweights_dts <- renderRHandsontable({
        DF = portweights_dts_data()
        if (!is.null(DF))
            rhandsontable(DF, stretchH = "all") %>%
            hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    })

    #************* portweights_val editor ***********#
    portweights_val_values = reactiveValues()
    
    portweights_val_data = reactive({
        if (!is.null(input$portweights_val)) {
            DF = hot_to_r(input$portweights_val)
        } else {
            if (is.null(portweights_val_values[["DF"]]))
                DF = portweights_val
            else
                DF = portweights_val_values[["DF"]]
        }
        
        portweights_val_values[["DF"]] = DF
        DF
    })
    
    output$portweights_val <- renderRHandsontable({
        DF = portweights_val_data()
        if (!is.null(DF))
            rhandsontable(DF, stretchH = "all") %>%
            hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    })

    #************* portweights_vnd editor ***********#
    portweights_vnd_values = reactiveValues()
    
    portweights_vnd_data = reactive({
        if (!is.null(input$portweights_vnd)) {
            DF = hot_to_r(input$portweights_vnd)
        } else {
            if (is.null(portweights_vnd_values[["DF"]]))
                DF = portweights_vnd
            else
                DF = portweights_vnd_values[["DF"]]
        }
        
        portweights_vnd_values[["DF"]] = DF
        DF
    })
    
    output$portweights_vnd <- renderRHandsontable({
        DF = portweights_vnd_data()
        if (!is.null(DF))
            rhandsontable(DF, stretchH = "all") %>%
            hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    })
    
    #************* gather all inputs besides tables ***********#
    AllInputs <- reactive({
        x <- reactiveValuesToList(input)
        x <- x[names(x)!="indices" & names(x)!="periods" & names(x)!="variables" 
               & names(x)!="dtsbuckets" & names(x)!="dtsbuckets2" & names(x)!="dtsbuckets3" & names(x)!="portrules" 
               & names(x)!="portweights_dts" & names(x)!="portweights_val" & names(x)!="portweights_vnd"]
        data.frame(
            name = names(x),
            value = unlist(x, use.names = FALSE)
        )
    })
    
    #************* show histogram by DtS buckets ***********#
    observeEvent(input$show_dtshist, { 
        if (is.null(final) & file.exists(paste(input$RootFolder, "BondMaster.csv", sep = ""))) {
            session$sendCustomMessage(type = 'print', message = list(selector = 'dtshist_status', html = "loading BondMaster..."))
            final <<- read.csv(paste(input$RootFolder, "BondMaster.csv", sep = ""))
            session$sendCustomMessage(type = 'print', message = list(selector = 'dtshist_status', html = "loading completed"))
        }
        if (!is.null(final)) {
            temp = subset(final, final$Index.Name==input$dtshist_indices)
            rv$dtshist <- temp$DtS
            session$sendCustomMessage(type = 'print', message = list(selector = 'dtshist_status', html = ""))
        }
        if (input$dtshist_indices=="EMNS" | input$dtshist_indices=="HCNF" | input$dtshist_indices=="HPID") {
            dtsbuckets <<- read.csv("dtsbuckets.csv")
            rv$dtsbreak <- dtsbuckets$Min
            rv$dtsxlim <- c(0,5000)
        } else if (input$dtshist_indices=="EJ00") {
            dtsbuckets2 <<- read.csv("dtsbuckets2.csv")
            rv$dtsbreak <- dtsbuckets2$Min
            rv$dtsxlim <- c(0,1000)
        } else if (input$dtshist_indices=="CI00") {
            dtsbuckets3 <<- read.csv("dtsbuckets3.csv")
            rv$dtsbreak <- dtsbuckets3$Min
            rv$dtsxlim <- c(0,3000)
            
        }
    })

    output$dtshist <- renderPlot({
        if (!is.null(rv$dtshist)) {
            #print(dtsbuckets$Min)
            #print(dtsbuckets2$Min)
            #print(dtsbuckets3$Min)
            suppressWarnings(
                hist(rv$dtshist, 
                     xlab="DtS", 
                     #breaks = c(-6000,600,1300,2000,3200,100000),
                     breaks = c(rv$dtsbreak,100000), 
                     col = "grey", border = "white",
                     xlim=rv$dtsxlim, freq = TRUE,
                     main = "DtS Histogram")
            )
        }
    })
    
    #************* show tracking error ***********#
    observeEvent(input$show_trkerr, { 
        if (file.exists(paste(input$RootFolder, "metrics.csv", sep = ""))) {
            session$sendCustomMessage(type = 'print', message = list(selector = 'trkerr_status', html = "loading metrics..."))
            metrics <<- read.csv(paste(input$RootFolder, "metrics.csv", sep = ""))
            session$sendCustomMessage(type = 'print', message = list(selector = 'trkerr_status', html = "loading completed"))
        }
        if (!is.null(metrics)) {
            temp = subset(metrics, metrics$Index.Name==input$trkerr_indices & grepl("^Y-",metrics$period))
            temp = temp[order(temp$period),]
            temp$period = gsub("Y-", "", temp$period)
            rv$trkerr <- temp$var_trr
            rv$trkerr_years <- temp$period
            rv$trkerr_ds <- temp
            session$sendCustomMessage(type = 'print', message = list(selector = 'trkerr_status', html = ""))
        }

    })
    
    output$trkerr <- renderPlotly({
        if (!is.null(rv$trkerr)) {
            #print(rv$trkerr)
            suppressWarnings(
                #barplot(rv$trkerr, 
                #        main="Tracking Error", 
                #        names.arg=rv$trkerr_years)
                ggplot(rv$trkerr_ds, aes(x= period, y = var_trr)) + 
                         geom_bar(stat = 'identity', aes(fill = var_trr>0), position = 'dodge', col = 'transparent') + 
                         theme_bw() + scale_fill_discrete(guide = 'none') + 
                         labs(x = '', y = 'tracking error')
            )
        }
    })    
    
    #************* show summary stats ***********#
    observeEvent(input$show_sumstats, { 
        if (file.exists(paste(input$RootFolder, "metrics.csv", sep = ""))) {
            session$sendCustomMessage(type = 'print', message = list(selector = 'sumstats_status', html = "loading metrics..."))
            metrics <<- read.csv(paste(input$RootFolder, "metrics.csv", sep = ""))
            session$sendCustomMessage(type = 'print', message = list(selector = 'sumstats_status', html = "loading completed"))
        }
        #print(input$sumstats_indices)
        
        if (!is.null(metrics)) {
            temp = subset(metrics, metrics$Index.Name==input$sumstats_indices & !grepl("^Y-",metrics$period))
            if (nrow(temp)>0) {
                idx_names = c("idx_obs","idx_dur","idx_asw","idx_dxs","idx_stdev","idx_dndev",
                              "idx_gpr","idx_trr","idx_exr","idx_sharpe","idx_sortino","idx_sdrsr","idx_rrr","idx_tr","idx_turnover")
                p1_names = c("p1_obs","p1_dur","p1_asw","p1_dxs","p1nx_stdev","p1nx_dndev",
                             "p1nx_gpr","p1nx_trr","p1nx_exr","p1nx_sharpe","p1nx_sortino","p1nx_sdrsr","p1nx_rrr","p1nx_tr","p1_turnover")
                common_names = c("obs","dur","asw","dxs","stdev","dndev",
                                 "gpr","trr","exr","sharpe","sortino","sdrsr","rrr","tr","turnover")
                
                temp_idx = temp[,c("period","Index.Name","months",idx_names)]
                temp_idx$Index.Name = "Benchmark"
                temp_idx$trkerrSD = 0
                setnames(temp_idx, old = idx_names, new = common_names)
                temp_p1 = temp[,c("period","Index.Name","months",p1_names,"p1nx_trkerrSD")]
                setnames(temp_p1, old = p1_names, new = common_names)
                # rename columns
                names(temp_p1)[names(temp_p1)=="p1nx_trkerrSD"] = "trkerrSD"
                
                temp = rbind(temp_idx, temp_p1)
                temp = temp[order(temp$period, temp$Index.Name),]
                
                rv$sumstats_ds <- temp
                session$sendCustomMessage(type = 'print', message = list(selector = 'sumstats_status', html = ""))
            } else {
                rv$sumstats_ds <- NULL
                session$sendCustomMessage(type = 'print', message = list(selector = 'sumstats_status', html = "no records found"))
            }
        }
        
    })
    
    output$sumstats <- renderTable({rv$sumstats_ds}, striped = TRUE)    
    
    #************* show bond graph ***********#
    observeEvent(input$bondgraph_indices, { 
        
        Periods = read.csv("periods.csv")
        Periods$Start = as.Date(Periods$Start)
        Periods$End = as.Date(Periods$End)
        Periods = Periods[order(Periods$Indices, Periods$Name),]
        Periods = subset(Periods, Periods$Indices==input$bondgraph_indices)
        bondgraph_periods <<- Periods
        
        updateSelectInput(session, "bondgraph_periods",
                          label = "Select a Period:",
                          choices = Periods$Name
        )        
    })
    
    observeEvent(input$show_bondgraph, { 
        if (file.exists(paste(input$RootFolder, "monthlystats.csv", sep = ""))) {
            session$sendCustomMessage(type = 'print', message = list(selector = 'bondgraph_status', html = "loading monthly stats..."))
            monthlystats <<- read.csv(paste(input$RootFolder, "monthlystats.csv", sep = ""))
            session$sendCustomMessage(type = 'print', message = list(selector = 'bondgraph_status', html = "loading completed"))
        }
        if (!is.null(monthlystats)) {
            # now filter stats to selected index and selected period
            temp = subset(monthlystats, monthlystats$Index.Name==input$bondgraph_indices)
            temp$As.of.Date = as.Date(temp$As.of.Date)
            if (!grepl("FULL", input$bondgraph_periods)) {
                Period = subset(bondgraph_periods, bondgraph_periods$Name == input$bondgraph_periods)
                bondgraph_start = Period$Start
                bondgraph_end = Period$End
                temp = subset(temp, temp$As.of.Date>=bondgraph_start & temp$As.of.Date<=bondgraph_end)
                temp$idx_nav = ave(temp$idx_trr_tmp, temp$Index.Name, FUN=cumprod)
                temp$p1nx_nav = ave(temp$p1nx_trr_tmp, temp$Index.Name, FUN=cumprod)
                temp$op1_nav = ave(temp$op1_trr_tmp, temp$Index.Name, FUN=cumprod)
                temp$op0_nav = ave(temp$op0_trr_tmp, temp$Index.Name, FUN=cumprod)
                temp$op_1_nav = ave(temp$op_1_trr_tmp, temp$Index.Name, FUN=cumprod)
                temp$op_2_nav = ave(temp$op_2_trr_tmp, temp$Index.Name, FUN=cumprod)
            }
            if (nrow(temp)>0) {
                index=xts(x = temp$idx_nav*100, order.by = temp$As.of.Date)
                port=xts(x = temp$p1nx_nav*100, order.by = temp$As.of.Date)
                outperform=xts(x = temp$op1_nav*100, order.by = temp$As.of.Date)
                neutral=xts(x = temp$op0_nav*100, order.by = temp$As.of.Date)
                underperform=xts(x = temp$op_1_nav*100, order.by = temp$As.of.Date)
                worst=xts(x = temp$op_2_nav*100, order.by = temp$As.of.Date)
                rv$bondgraph <- cbind(index,port,outperform,neutral,underperform,worst)
                session$sendCustomMessage(type = 'print', message = list(selector = 'bondgraph_status', html = ""))
            } else {
                rv$bondgraph <- NULL
                session$sendCustomMessage(type = 'print', message = list(selector = 'bondgraph_status', html = "No records found"))
                
            }
        }
        
    })
    
    output$bondgraph <- renderDygraph({
        print(rv$bondgraph)
        if (!is.null(rv$bondgraph)) {
            dygraph(rv$bondgraph) %>% 
                dyRangeSelector() %>%
                dyLegend(width = 800)
        }
    })    
    
    #************* market cycle graph ***********#
    observeEvent(input$show_marketcycle, { 
        if (file.exists(paste(input$RootFolder, "marketcycles.csv", sep = ""))) {
            session$sendCustomMessage(type = 'print', message = list(selector = 'marketcycle_status', html = "loading market cycles..."))
            marketcycles <<- read.csv(paste(input$RootFolder, "marketcycles.csv", sep = ""))
            session$sendCustomMessage(type = 'print', message = list(selector = 'marketcycle_status', html = "loading completed"))
        }
        if (!is.null(marketcycles)) {
            # now construct the time series data for plotting
            temp = read.csv(paste(input$RootFolder, "Bond raw data/Index lvl data/Daily/trr_prr.csv", sep = ""))
            temp$Date = as.Date(temp$Date)
            temp = subset(temp, temp$Index.Name == input$marketcycle_indices)
            temp$IndexDate = paste(temp$Index.Name, temp$Date)
            marketcycles$StartDate = as.Date(marketcycles$StartDate)
            marketcycles$EndDate = as.Date(marketcycles$EndDate)
            marketcycles$IndexDate = paste(marketcycles$Index.Name, marketcycles$StartDate)
            temp=merge(temp, marketcycles[,c("IndexDate","MarketCycle","EndValue")], 
                       by.x = "IndexDate", by.y = "IndexDate", all.x = TRUE)
            temp=FillDown(temp,"MarketCycle")
            temp=FillDown(temp,"EndValue")
            temp$highlow = temp$EndValue
            
            if (input$mkt_cycle_metric=="PRR") {
                data=xts(x = temp$PRR, order.by = temp$Date)
            } else if (input$mkt_cycle_metric=="TRR") {
                data=xts(x = temp$TRR, order.by = temp$Date)
            } else {
            }
            
            highlow = xts(x = temp$highlow, order.by = temp$Date)
            rv$marketcyclegraph <- cbind(data, highlow)
            
            session$sendCustomMessage(type = 'print', message = list(selector = 'marketcycle_status', html = ""))
        } else {
            rv$marketcyclegraph <- NULL
            session$sendCustomMessage(type = 'print', message = list(selector = 'marketcycle_status', html = "No records found"))
                
        }
        
        
    })
    
    output$marketcycle <- renderDygraph({
        print(rv$marketcyclegraph)
        if (!is.null(rv$marketcyclegraph)) {
            dygraph(rv$marketcyclegraph) %>% 
                dyRangeSelector()   
        }
    })    
    
}
