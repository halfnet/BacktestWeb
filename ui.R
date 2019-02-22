tagList(
    tags$head(
        tags$script(
            HTML("
                 Shiny.addCustomMessageHandler ('print',function (message) {
                 $('#'+message.selector).html(message.html);
                 console.log(message);
                 });
                 ")
        )
    ),
    useShinyjs(),
    navbarPage(title = "Backtest",
               tabPanel(title = "Main",
                        fluidRow(
                            column(1, 
                                   actionButton(inputId = "go", label = "GO!")
                                   #bsTooltip("go", "start backtest", placement = "right", trigger = "hover",
                                    #         options = NULL)
                            ),
                            column(2, 
                                   actionButton(inputId = "save", 
                                                label = "SAVE")
                                   #bsTooltip("save", "save backtest parameters", placement = "right", trigger = "hover",
                                    #         options = NULL)
                            ),
                            column(2, 
                                   actionButton(inputId = "equity_opinion", 
                                                label = "Equities Op")
                            ),
                            column(2, 
                                   actionButton(inputId = "ai_export", 
                                                label = "AI Export")
                            ),
                            column(2, 
                                   actionButton(inputId = "mkt_cycle", 
                                                label = "Market Cycle")
                            ),
                            column(3, 
                                   selectInput(inputId = "mkt_cycle_metric", 
                                               label = "Metric for Market Cycle:",
                                               c("PRR" = "PRR",
                                                 "TRR" = "TRR"))                            
                                   )                            
                        ),
                        fluidRow(
                            textOutput("status")
                        ),                        
                        fluidRow(
                            tags$hr()
                        ),
                        fluidRow(
                            column(4,
                                   helpText("Company Score Parameters:"),
                                   checkboxInput(inputId = "CS_UseFile", 
                                                 label = "Use Saved File?",
                                                 value = FALSE),
                                   bsTooltip("CS_UseFile", "uncheck this to re-compute opinions", placement = "right", trigger = "hover",
                                             options = NULL),                                   
                                   textInput(inputId = "CompanyScoreFolder", 
                                             label = "Company Score Folder",
                                             value = "Company scores/"),
                                   bsTooltip("CompanyScoreFolder", "folder where company score files reside", placement = "right", trigger = "hover",
                                             options = NULL),                                   
                                   numericInput(inputId = "CS_MaxRange_Q", 
                                                label = "Max Range When RF = Quarter",
                                                value = -250),
                                   bsTooltip("CS_MaxRange_Q", "days look back for valid scores (negative value)", placement = "right", trigger = "hover",
                                             options = NULL),                                   
                                   numericInput(inputId = "CS_MaxRange_SA", 
                                                label = "Max Range When RF = Semi-Annual",
                                                value = -380),
                                   bsTooltip("CS_MaxRange_SA", "days look back for valid scores (negative value)", placement = "right", trigger = "hover",
                                             options = NULL),                                   
                                   numericInput(inputId = "CS_AdjEntryDate_123Q", 
                                                label = "Adjusted Entry Date for Quarters 1-3",
                                                value = 100),
                                   bsTooltip("CS_AdjEntryDate_123Q", "days to adjust from date of score (postive value)", placement = "right", trigger = "hover",
                                             options = NULL),                                   
                                   numericInput(inputId = "CS_AdjEntryDate_1S", 
                                                label = "Adjusted Entry Date for first half",
                                                value = 100),
                                   bsTooltip("CS_AdjEntryDate_1S", "days to adjust from date of score (postive value)", placement = "right", trigger = "hover",
                                             options = NULL),                                   
                                   numericInput(inputId = "CS_AdjEntryDate_4Q", 
                                                label = "Adjusted Entry Date for Quarter 4",
                                                value = 160),
                                   bsTooltip("CS_AdjEntryDate_4Q", "days to adjust from date of score (postive value)", placement = "right", trigger = "hover",
                                             options = NULL),                                   
                                   numericInput(inputId = "CS_AdjEntryDate_2S", 
                                                label = "Adjusted Entry Date for second half",
                                                value = 160),
                                   bsTooltip("CS_AdjEntryDate_2S", "days to adjust from date of score (postive value)", placement = "right", trigger = "hover",
                                             options = NULL),                                   
                                   numericInput(inputId = "CS_Opinion_NA", 
                                                label = "Maximum NA for inclusion",
                                                value = 2),
                                   numericInput(inputId = "CS_Opinion_NA_Factor", 
                                                label = "NA Conversion Factor",
                                                value = 0),
                                   numericInput(inputId = "CS_TableOption", 
                                                label = "Table Option",
                                                value = 1),
                                   bsTooltip("CS_TableOption", "1 - use MR if RF = auto, otherwise OR; 2 - always use OR; 3 - awalys use MR; 4 - use both", placement = "right", trigger = "hover",
                                             options = NULL)
                            ),
                            column(4, 
                                   helpText("Model Construction Parameters:"),
                                   checkboxInput(inputId = "ConstructModelPort", 
                                                 label = "Construct Model Portfolio?",
                                                 value = TRUE),
                                   bsTooltip("ConstructModelPort", "uncheck this to use original dynamic mode", placement = "right", trigger = "hover",
                                             options = NULL),                                   
                                   selectInput(inputId = "ConstructModelPortMethod", 
                                               label = "Construct Model Port Method:",
                                               c("MV%" = 1,
                                                 "MV%_A" = 2,
                                                 "MV%_B" = 3,
                                                 "DtS" = 4,
                                                 "DtS Cash" = 5)),
                                   selectInput(inputId = "PortWeightMethod", 
                                               label = "Port Weight Method:",
                                               c("Index Weight" = 1,
                                                 "Equal Weight" = 2,
                                                 "Index Weight DtS" = 3,
                                                 "Equal Weight DtS" = 4,
                                                 "Index Weight Val" = 5,
                                                 "Equal Weight Val" = 6,
                                                 "Index Weight VnD" = 7,
                                                 "Equal Weight VnD" = 8)),
                                   bsTooltip("PortWeightMethod", "this option only affect MV% methods", placement = "right", trigger = "hover",
                                             options = NULL),                                   
                                   textInput(inputId = "RootFolder", 
                                             label = "Root Folder",
                                             value = "C:/MyProjects/Guru/EMBackTesting/Backtest_000/"),
                                   bsTooltip("RootFolder", "root folder of all backtest files", placement = "right", trigger = "hover",
                                             options = NULL),                                   
                                   textInput(inputId = "IndexDataFolder", 
                                             label = "Index Data Folder",
                                             value = "Bond raw data/Index constituents/"),
                                   bsTooltip("IndexDataFolder", "sub folder where index constituents files reside", placement = "right", trigger = "hover",
                                             options = NULL),                                   
                                   textInput(inputId = "IndexLvlDataFolder", 
                                             label = "Index Level Data Folder",
                                             value = "Bond raw data/Index lvl data/"),
                                   bsTooltip("IndexLvlDataFolder", "sub folder where index level files reside", placement = "right", trigger = "hover",
                                             options = NULL),                                   
                                   numericInput(inputId = "OutPerformMultiple", 
                                                label = "Outperform Multiple",
                                                value = 3),
                                   numericInput(inputId = "MaxWeightPerName", 
                                                label = "Max Weight Per Name (%)",
                                                value = 5),
                                   numericInput(inputId = "ConstructModelPortDtSPHold", 
                                                label = "DtS Percentile Hold",
                                                value = 0.5),
                                   numericInput(inputId = "ConstructModelPortDtSPBuy", 
                                                label = "DtS Percentile Buy",
                                                value = 0.9),
                                   numericInput(inputId = "TAIL_RATIO_PERCENTILE", 
                                                label = "Tail Ratio Percentile",
                                                value = 0.1)
                            ),
                            column(4, 
                                   helpText("Threshold Parameters:"),
                                   checkboxInput(inputId = "TH_UseFile", 
                                                 label = "Use Saved File?",
                                                 value = FALSE),
                                   bsTooltip("TH_UseFile", "uncheck this to re-compute thresholds", placement = "right", trigger = "hover",
                                             options = NULL),                                   
                                   checkboxInput(inputId = "TH_NoLimits", 
                                                 label = "No Limits?",
                                                 value = FALSE),
                                   bsTooltip("TH_NoLimits", "check this disable thresholds", placement = "right", trigger = "hover",
                                             options = NULL),                                   
                                   checkboxInput(inputId = "TH_UsePrevMonth", 
                                                 label = "Use Prev Month?",
                                                 value = FALSE),
                                   bsTooltip("TH_UsePrevMonth", "check this to use prev month thresholds", placement = "right", trigger = "hover",
                                             options = NULL),                                   
                                   numericInput(inputId = "TH_MinWght", 
                                                label = "Minimum Portfolio Weight (%)",
                                                value = 50),
                                   numericInput(inputId = "TH_MA", 
                                                label = "Moving Average (Months)",
                                                value = 6)
                                   
                            )
                        ),
                        fluidRow(
                            tags$hr()
                        ),
                        helpText("Indices to include/exclude:"),
                        fluidRow(
                            rHandsontableOutput("indices")
                        ),
                        fluidRow(
                            tags$hr()
                        ),
                        helpText("Specify portfolio construction rules:"),
                        fluidRow(
                            rHandsontableOutput("portrules")
                        ),
                        fluidRow(
                            tags$hr()
                        ),
                        helpText("Specify portfolio construction weights (DtS):"),
                        fluidRow(
                            rHandsontableOutput("portweights_dts")
                        ),
                        fluidRow(
                            tags$hr()
                        ),
                        helpText("Specify portfolio construction weights (Val):"),
                        fluidRow(
                            rHandsontableOutput("portweights_val")
                        ),
                        fluidRow(
                            tags$hr()
                        ),
                        helpText("Specify portfolio construction weights (VnD):"),
                        fluidRow(
                            rHandsontableOutput("portweights_vnd")
                        ),
                        fluidRow(
                            tags$hr()
                        ),
                        helpText("Different periods of up/down markets:"),
                        fluidRow(
                            rHandsontableOutput("periods")
                        )
                        
               ),
               tabPanel(title = "Current Holdings",
                        fluidRow(
                            column(1, 
                                   actionButton(inputId = "CH_go", 
                                                label = "GO!")
                            ),
                            column(11, 
                                   textOutput("CH_status")
                            )
                        ),
                        fluidRow(
                            tags$hr()
                        ),
                        fluidRow(
                            column(12, 
                                   dateInput("CH_OpinionDate", "Opinion Date:")                          
                            )
                            
                        )
               ),
               tabPanel(title = "Data Check",
                        fluidRow(
                            column(1, 
                                   actionButton(inputId = "DC_go", 
                                                label = "GO!")
                            ),
                            column(2, 
                                   actionButton(inputId = "DC_recycled", 
                                                label = "Recycled Tickers")
                            ),
                            column(9, 
                                   textOutput("DC_status")
                            )
                        ),
                        fluidRow(
                            tags$hr()
                        )
               ),
               tabPanel(title = "Optimization",
                        fluidRow(
                            column(1, 
                                   actionButton(inputId = "OPT_go", 
                                                label = "GO!")
                            ),
                            column(1, 
                                   actionButton(inputId = "OPT_save", 
                                                label = "SAVE")
                            ),
                            column(10, 
                                   textOutput("OPT_status")
                            )
                        ),
                        fluidRow(
                            tags$hr()
                        ),
                        helpText("Variables for grid search:"),
                        fluidRow(
                            rHandsontableOutput("variables")
                        )
               ),
               tabPanel(title = "Additional Tests",
                        fluidRow(
                            column(2, 
                                   actionButton(inputId = "TEST_run", 
                                                label = "Run Test")
                            ),
                            column(2, 
                                   actionButton(inputId = "TEST_save", 
                                                label = "SAVE")
                            ),
                            column(8, 
                                   textOutput("TEST_status")
                            )
                        ),
                        fluidRow(
                            tags$hr()
                        ),
                        fluidRow(
                            column(2, 
                                   helpText("Creative Accounting:")
                            ),
                            column(5, 
                                   numericInput(inputId = "TEST_CA_SCORE", 
                                                label = "Score must be less than this to be negative:",
                                                value = 5)
                            ),
                            column(5, 
                                   numericInput(inputId = "TEST_CA_NA", 
                                                label = "NA must be less than this is to be negative:",
                                                value = 6)
                            )
                        ),
                        fluidRow(
                            column(2, 
                                   helpText("Liquidity:")
                            ),
                            column(5, 
                                   numericInput(inputId = "TEST_LD_SCORE", 
                                                label = "Score must be less than this to be negative:",
                                                value = 2)
                            ),
                            column(5, 
                                   numericInput(inputId = "TEST_LD_NA", 
                                                label = "NA must be less than this is to be negative:",
                                                value = 3)
                            )
                        ),
                        fluidRow(
                            column(2, 
                                   helpText("Valuation:")
                            ),
                            column(5, 
                            selectInput(inputId = "TEST_VAL_METRIC", 
                                        label = "Metric to use:",
                                        c("ASW/DUR" = 1,
                                          "ASW" = 2
                                          ))
                            ),
                            column(5, 
                                   numericInput(inputId = "TEST_VAL_TH", 
                                                label = "Valuation threshold (%):",
                                                value = 15)
                            )
                        ),
                        helpText("DtS Buckets (High Yield):"),
                        fluidRow(
                            rHandsontableOutput("dtsbuckets")
                        ),
                        helpText("DtS Buckets 2 (EJ00):"),
                        fluidRow(
                            rHandsontableOutput("dtsbuckets2")
                        ),                        
                        helpText("DtS Buckets 3 (CI00):"),
                        fluidRow(
                            rHandsontableOutput("dtsbuckets3")
                        ),                        
                        fluidRow(
                            column(6, 
                                   numericInput(inputId = "TEST_MIN_OBS", 
                                                label = "Minimum observations to be included:",
                                                value = 50)
                            )
                        )
               ),
               navbarMenu(title = "Analytics",
                          tabPanel(title = "DtS Histogram",
                                   selectInput(inputId = "dtshist_indices", 
                                               label = "Select an Index:",
                                               ui_index_list),
                                   plotOutput("dtshist"),
                                   actionButton("show_dtshist", "Show Histogram"),
                                   textOutput("dtshist_status")
                          ),
                          tabPanel(title = "Tracking Error",
                                   selectInput(inputId = "trkerr_indices", 
                                               label = "Select an Index:",
                                               ui_index_list),
                                   #plotOutput("trkerr"),
                                   plotlyOutput("trkerr"),
                                   actionButton("show_trkerr", "Show Chart"),
                                   textOutput("trkerr_status")
                          ),
                          tabPanel(title = "Summary Stats",
                                   selectInput(inputId = "sumstats_indices", 
                                               label = "Select an Index:",
                                               ui_index_list),
                                   tableOutput("sumstats"),
                                   actionButton("show_sumstats", "Show Table"),
                                   textOutput("sumstats_status")
                          ),
                          tabPanel(title = "Bond Graph",
                                   selectInput(inputId = "bondgraph_indices", 
                                               label = "Select an Index:",
                                               ui_index_list),
                                   selectInput(inputId = "bondgraph_periods", 
                                               label = "Select a Period:",
                                               ui_index_list),
                                   dygraphOutput("bondgraph"),
                                   actionButton("show_bondgraph", "Show Graph"),
                                   textOutput("bondgraph_status")
                          ),
                          tabPanel(title = "Market Cycle",
                                   selectInput(inputId = "marketcycle_indices", 
                                               label = "Select an Index:",
                                               ui_index_list),
                                   dygraphOutput("marketcycle"),
                                   actionButton("show_marketcycle", "Show Market Cycle"),
                                   textOutput("marketcycle_status")
                          )
               )
    )
)
