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
                                   actionButton(inputId = "go", label = "GO!"),
                                   bsTooltip("go", "start backtest", placement = "right", trigger = "hover",
                                             options = NULL)
                            ),
                            column(1, 
                                   actionButton(inputId = "save", 
                                                label = "SAVE"),
                                   bsTooltip("save", "save backtest parameters", placement = "right", trigger = "hover",
                                             options = NULL)
                            ),
                            column(8, 
                                   textOutput("status")
                            )
                        ),
                        fluidRow(
                            tags$hr()
                        ),
                        fluidRow(
                            column(4,
                                   helpText("Company Score Parameters:"),
                                   checkboxInput(inputId = "CS_UseFile", 
                                                 label = "Use Saved File?",
                                                 value = TRUE),
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
                                   numericInput(inputId = "CS_MaxRange_SA", 
                                                label = "Max Range When RF = Semi-Annual",
                                                value = -380),
                                   numericInput(inputId = "CS_AdjEntryDate_123Q", 
                                                label = "Adjusted Entry Date for Quarters 1-3",
                                                value = 100),
                                   numericInput(inputId = "CS_AdjEntryDate_1S", 
                                                label = "Adjusted Entry Date for first half",
                                                value = 100),
                                   numericInput(inputId = "CS_AdjEntryDate_4Q", 
                                                label = "Adjusted Entry Date for Quarter 4",
                                                value = 160),
                                   numericInput(inputId = "CS_AdjEntryDate_2S", 
                                                label = "Adjusted Entry Date for second half",
                                                value = 160),
                                   numericInput(inputId = "CS_Opinion_NA", 
                                                label = "Maximum NA for inclusion",
                                                value = 2),
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
                                                 "DtS" = 2,
                                                 "DtS Cash" = 3)),
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
                                   numericInput(inputId = "TransactionCost", 
                                                label = "Transaction Cost",
                                                value = 0.3),
                                   numericInput(inputId = "MaxWeightPerName", 
                                                label = "Max Weight Per Name (%)",
                                                value = 5),
                                   numericInput(inputId = "BuyM_RatioCutOff", 
                                                label = "Buy M-11 Ratio CutOff",
                                                value = 0),
                                   numericInput(inputId = "HoldM_RatioCutOff", 
                                                label = "Hold M-11 Ratio CutOff",
                                                value = 1),
                                   numericInput(inputId = "ConstructModelPortDtSPHold", 
                                                label = "DtS Percentile Hold",
                                                value = 0.5),
                                   numericInput(inputId = "ConstructModelPortDtSPBuy", 
                                                label = "DtS Percentile Buy",
                                                value = 0.9),
                                   numericInput(inputId = "TScoreHoldFilter", 
                                                label = "TScore Hold Filter (0=no filter)",
                                                value = 0),
                                   numericInput(inputId = "TScoreBuyFilter", 
                                                label = "TScore Buy Filter (0=no filter)",
                                                value = 0),
                                   numericInput(inputId = "TScoreCutOff", 
                                                label = "TScore CutOff",
                                                value = -1),
                                   numericInput(inputId = "TScoreNA", 
                                                label = "TScore NA",
                                                value = -1),
                                   textInput(inputId = "Ind_Lvl1", 
                                             label = "Industry Level 1 Filter",
                                             value = "ALL"),
                                   bsTooltip("Ind_Lvl1", "allow research of a given industry combination", placement = "right", trigger = "hover",
                                             options = NULL),                                   
                                   textInput(inputId = "Ind_Lvl2", 
                                             label = "Industry Level 2 Filter",
                                             value = "ALL"),
                                   textInput(inputId = "Ind_Lvl3", 
                                             label = "Industry Level 3 Filter",
                                             value = "ALL"),
                                   textInput(inputId = "Ind_Lvl4", 
                                             label = "Industry Level 4 Filter",
                                             value = "ALL")
                                   
                            ),
                            column(4, 
                                   helpText("Threshold Parameters:"),
                                   checkboxInput(inputId = "TH_UseFile", 
                                                 label = "Use Saved File?",
                                                 value = TRUE),
                                   bsTooltip("TH_UseFile", "uncheck this to re-compute thresholds", placement = "right", trigger = "hover",
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
                            column(4,
                                   helpText("Company Score Parameters:"),
                                   checkboxInput(inputId = "CH_CS_UseFile", 
                                                 label = "Use Saved File?",
                                                 value = FALSE)
                            ),
                            column(4, 
                                   helpText("Model Construction Parameters:"),
                                   checkboxInput(inputId = "CH_ConstructModelPort", 
                                                 label = "Construct Model Portfolio?",
                                                 value = TRUE),
                                   selectInput(inputId = "CH_ConstructModelPortMethod", 
                                               label = "Construct Model Port Method:",
                                               c("MV%" = 1,
                                                 "DtS" = 2,
                                                 "DtS Cash" = 3)),
                                   dateInput("CH_OpinionDate", "Opinion Date:")                          
                            ),
                            column(4, 
                                   helpText("Threshold Parameters:"),
                                   checkboxInput(inputId = "CH_TH_UseFile", 
                                                 label = "Use Saved File?",
                                                 value = FALSE)
                                   
                            )
                            
                            
                        )
               ),
               tabPanel(title = "Data Check",
                        fluidRow(
                            column(1, 
                                   actionButton(inputId = "DC_go", 
                                                label = "GO!")
                            ),
                            column(11, 
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
               navbarMenu(title = "Other",
                          tabPanel(title = "Sub Menu1",
                                   plotOutput("chisq"),
                                   actionButton("rechisq", "Resample")
                          ),
                          tabPanel(title = "Sub Menu2",
                                   plotOutput("norm"),
                                   actionButton("renorm", "Resample")
                          )
               )
    )
)
