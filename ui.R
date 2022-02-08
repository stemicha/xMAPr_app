#                                        .(((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((    
#                                      (((((                                                                                                               (((( 
#                                   ((((                                                                                                                     (((
#              ,,,,,,             ((((                                                                                                                        ((
#             ,,, ,,             (((                                                                                                                          ((
#             ,, ,,,           (((                            ,,,,,,     ,,,,,,              ,,,,,,,,          ,,,,,,,,,,,,,,,,                               ((
#            ,,,,,,           (((   ,                 ,,   ,,,,,,,,,,,,,,,,,,,,,,,         ,,,,,,,,,,,,        ,,,,,,,,,,,,,,,,,,                             ((
#       ,,,,.,, ,,          (((      ,               ,,   ,,,,      ,,,,,      ,,,,       ,,,,      ,,,,                      ,,,,                          ((( 
#    ,,,, .,,,,,,, ,,      (((        ,,            ,,   ,,,,                   ,,,      ,,,,        ,,,,                      ,,,    (((((((((((((((((((((((   
#      ,,,,,   ,,, ,,,    (((          ,,         .,     ,,,,                   ,,,     ,,,,          ,,,,                    ,,,,    ((             (((        
#               ,,, ,,,  (((            ,,       .,      ,,,,                   ,,,    .,,,            ,,,.    ,,,,,,,,,,,,,,,,,,     ((              *((       
#    ((((((((((( ,,,    ((               ,  ,,,  ,,      ,,,,                   ,,,    ,,,,    ,,,,    ,,,,    ,,,,,,,,,,,,,,,,       ((                ((      
# /(((,,,,,,,,/(((    /((               ,,        ,      ,,,,        ,,,        ,,,   ,,,,     ,,,,     ,,,,   ,,,                    ((                 ((     
#(((,,,,,,,,,,,,,((  (((               ,,          ,     ,,,,        ,,,        ,,,  ,,,,                ,,,,  ,,,                    ((                  ((    
#((,,,,,,,,,,,,,,/(((((               ,             ,,   ,,,,                   ,,,  ,,,,                ,,,,  ,,,                    ((                   ((   
#((,,,,,,,,,,,,,,,((((              ,,               ,,  ,,,,                   ,,,  ,,,,                ,,,,  ,,,                    ((                    ((. 
#((/,,,,,,,,,,,,,(((                ,                 ,  ,,,,                   ,,,                            ,,,                    ((                     ((/
# (((,,,,,,,,,,(((                                                                                                                                              
#   ((((((((((((                                                                                                                                                

# xMAPr - high dynamic range xMAP® data analysis
#
# ui.R
# 
# github: https://github.com/stemicha/
# author: Stephan Michalik#
#

#### activate for shinyapps IO deployment
 #load libraries
 # library(plotly)
 # library(ggsignif)
 # library(colourpicker)
 # library(factoextra)
 # library(FactoMineR)
 # library(RColorBrewer)
 # library(gridExtra)
 # library(grid)
 # library(ggrepel)
 # library(ggpubr)
 # library(heatmaply)
 # library(rlang)
 # library(data.table)
 # library(minpack.lm)
 # library(shinyFiles)
 # library(shinyWidgets)
 # library(tidyverse)
 # library(shiny)
 # library(shinyjs)
 # library(magrittr)
 # library(shinydashboard)
 # library(shinycssloaders)
 # library(scales)
 # library(shinyBS)
 # library(shinyalert)
 # library(DT)
 # library(utils)
 # library(readxl)
# 
# 
## start dashboard page ----
dashboardPage(title="xMAPr - analyzing serological assays with high precission",
      
  #header setup // setup logo -----
  dashboardHeader(title=img(src="logo.png", height = 40, align = "center"),
                    dropdownMenu(type = "notifications",
                                 icon=icon("font"),
                                 notificationItem(
                                   text = "adjust font size: strg & + OR strg & -",
                                   icon("windows")
                                 ),
                                 notificationItem(
                                   text = "adjust font size: cmd & + OR cmd & -",
                                   icon("apple")
                                 )
                    ),
                  tags$li(a(href = "xMAPr_manual/_book/index.html",
                            target="blank",
                            icon("question"),
                            title = "xMAPr manual"),
                          class = "dropdown"),
                  tags$li(a(href = "mailto:stephan.michalik@uni-greifswald.de",
                            icon("at"),
                            title = "feedback please write mail to stephan.michalik@uni-greifswald.de"),
                            class = "dropdown"),
                  tags$li(a(href = 'https://cfungene.uni-greifswald.de',
                            icon("home"),
                            target="blank",
                            title = "visit department homepage"),
                            class = "dropdown"),
                  # Joe Cheng suggestion
                  tags$li(class = "dropdown", actionButton(inputId = "session_clr",
                                                           label = "restart app",
                                                           icon("power-off"),
                                                           style="color: #FF6600; background-color: #1A1A1A; border-color: #FF6600; margin-top: 7px;",
                                                           onclick = "window.location.reload();"))
                  
                  
                  ),
  #sidebar setup // sidebar menu  -----
  dashboardSidebar(
    sidebarMenu(id="side.menu",
      uiOutput("projectNAME"),
      #home button sidebar ------
      menuItem("home",tabName = "home", icon = icon("home")),
      #raw data processing button sidebar -----
      menuItem("raw data processing",tabName = "rawProcess", icon = icon("magic")),
      #Data processing pipeline button sidebar -----
      menuItem("Data processing pipeline",tabName = "pipe", icon = icon("cogs")),
      #Result visualization selection button sidebar -----
      menuItem("Result visualization selection",tabName = "resvis",icon = icon("chart-area"),
                 #File assay data input
              fileInput("processed_data_file", "processed data",accept=c(".RDS")),
              bsPopover(id = "inputButton_data_visu", 
                        title = NULL, 
                        content ="before pressing the button please select the RDS-file generated from the Data processing pipeline module", 
                        placement = "right", 
                        trigger = "hover",
                        options = NULL),
              #action button for run data processing
              actionButton("inputButton_data_visu", "RUN data visualization...",width = "90%",icon=icon("youtube"),style="color: #000000; background-color: #B84B18; border-color: #722E00"),
              #select_meta_data_coloring
              uiOutput("meta.selection"), 
              menuSubItem("global result overview",tabName = "resvis_panel", icon = icon("globe")),
              menuSubItem("inspect analysis / reponse table",tabName = "sass",icon = icon("search")),
              menuSubItem("statistical analysis",tabName = "stats",icon = icon("compress"))                  
              
      ),
      #about button sidebar ------
      menuItem("about",tabName = "about", icon = icon("question-circle")),
      br(),
      downloadButton("downloadDemoData", label = "Download demo data",style="margin-left: 30px;"),
      h5("version 1.1",align="center"), # version ------
      h6("author: S. Michalik",align="center"),
      br(),
      h6("for optimal performance please",align="center",style="color: #FF6600;"),
      h6("use Firefox or Chrome browser",align="center",style="color: #FF6600;")

    )
  ),
  #page setup  -----
  dashboardBody(
    useShinyjs(), # # Include shinyjs
    #useShinyalert(),# Set up shinyalert
    
    #hide error messsages in shiny  -----
     tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
    # Also add some custom CSS to make the title background area the same
    # color as the rest of the header.
    # custom look  -----
    tags$head(tags$style(HTML('
                               /* body width */
                              .progress-bar{
                                background-color:#FF6600;
                              }
                               /* body width */
                              body {
                              width: 100%;
                              max-width: 100%;
                              }
                              /* mainpage background */
                              .content-wrapper,
                              .right-side {
                              background-color: #F5F5F5;
                              }

                              /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #080A0D;
                              }
                              
                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #080A0D;
                              }
                              
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #080A0D;
                              }  

                              
                              /* main sidebar */
                              .skin-blue .main-sidebar {
                              background-color: #080A0D;
                              }
                              
                              /* active selected tab in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #B84B18;
                              color: #000000;
                              }
                              
                              /* other links in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #080A0D;
                              color: #FFFFFF;
                              }

                              /* treeview sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview-menu {
                                  background-color: #171717;
                              }
                              
                              /* other links in the sidebarmenu when hovered */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #171717;
                              }
                              /* toggle button when hovered  */                    
                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #171717;
                              }
                              /* toggle button with left line when hovered  */ 
                              .skin-blue .sidebar-menu > li:hover > a {
                                border-left-color: #B84B18;
                              }
                              /* toggle button with left line when active  */ 
                              .skin-blue .sidebar-menu > li.active > a {
                                border-left-color: #B84B18;
                              }

                              .popover-title{
                              color: #B84B18;
                              font-size: 16px;
                              background-color: #171717;
                              }
                              
                              .popover-header{ 
                              background: #FFFFFF; 
                              } 
                              
                              .popover-content{ 
                              background: #FFFFFF;
                              color: #171717;
                              }
                              
                              .tooltip .tooltiptext {
                              visibility: hidden;
                              width: 120px;
                              background-color: black;
                              color: #000000;
                              text-align: center;
                              border-radius: 6px;
                              padding: 5px 0;
                              position: absolute;
                              z-index: 1;
                              bottom: 100%;
                              left: 50%;
                              margin-left: -60px;
                              }
                              
                              .tooltip:hover .tooltiptext {
                              visibility: visible;
                              opacity: 1;
                              }

                              }

                              '))),
    
    tabItems(
      
            
      ## START process pipeline UI -----
      #home mainpage -----
      tabItem(tabName = "home",
              fluidRow(#need to be wrapped in fluid row 
                
                  shiny::img(src="xMAPr_home_screen.png", align = "center",style="display: block; margin-left: auto; margin-right: auto;")
                    
                
              )
              #
      ),
      #about mainpage -----
      tabItem(tabName = "about",
              fluidRow(#need to be wrapped in fluid row 
              box(
                width = 12,
                includeHTML("www/xMAPr_introduction.html")
                # 1. knit to html
                  #2. remove line 17-25 from html --> scripts that jepardize the app
                  

                )
              )
              #
      ),
      
      
      ## raw file processing -------
        tabItem(tabName = "rawProcess",
              box(
                title = "xPonent raw file input", width = 3, solidHeader = TRUE, status = "info",
                uiOutput("downloadProcessedRaw.UI"),
                textInput(inputId = "RawProcessedName",label = "set name for processed output", value = "raw_processed_output"),
                bsPopover(id = "RawProcessedName", 
                          title = NULL, 
                          content ="the name will be used for file naming", 
                          placement = "bottom", 
                          trigger = "hover",
                          options = NULL),
                hr(),
                awesomeRadio(inputId = "RawProcessedType",
                             label = "Input type data:",
                             choices = c("xPonent", "BioPlex"),
                             selected = c("xPonent"),inline = T),
                bsPopover(id = "RawProcessedType", 
                          title = NULL, 
                          content ="xPonent: *.csv files; BioPlex: *.xlsx files", 
                          placement = "bottom", 
                          trigger = "hover",
                          options = NULL),
                awesomeRadio("sepRAW",
                             "Sample column separator for assay RAW xPonent input files ?",
                             c(pipe="|",underscore="_",dash="-"),
                             selected="_", 
                             inline = T),
                helpText("example: Sample1_1_10000 = Sample > Replicate > Xfold Dilution (Separator = _)"),
                hr(),
                h3("assay data"),
                fluidRow(
                  column(12,offset = 0,
                         fileInput("csvRAWassay",
                                   label="Upload assay raw data here",
                                   multiple = TRUE),
                         
                         actionButton(inputId = "csvRAWassayRESET", label = "reset", icon = icon("retweet"),style = "margin-top: 0px;",width = "90px")
                  ) 
                  
                ),
                helpText("multiple file selection is allowed !"),
                #column split character for assay sample column 
                hr(),
                h3("coupling control data"),
                fluidRow(
                  column(12,offset = 0,
                fileInput("csvRAWcontrol",
                          label="Upload coupling control raw data here",
                          multiple = FALSE),
                       actionButton(inputId = "csvRAWcontrolRESET", label = "reset", icon = icon("retweet"),style = "margin-top: 0px;",width = "90px")
                )
                ), 
                hr(),
                actionButton(inputId = "inputButton_raw_data_processing", label = "RUN data processing...",width = "100%",icon=icon("paper-plane"),style="color:#FFFFFF; background-color: #0885C8; border-color: #085EC8")
                
                ),
                box(title = "converted data frames", width = 9, solidHeader = TRUE, status = "info",
                        tabsetPanel(id="TabsetRawProcess",
                              tabPanel("RawAssayMFI",
                                       fluidPage(
                                         dataTableOutput("RawAssayMFI.table")
                                        )
                                       ),
                              tabPanel("RawAssayCount",
                                       fluidPage(
                                        dataTableOutput("RawAssayCount.table")
                                       )
                              ),
                              tabPanel("RawControlMFI",
                                       fluidPage(
                                        dataTableOutput("RawControlMFI.table")
                                       )
                              ),
                              tabPanel("RawControlCount",
                                       fluidPage(
                                        dataTableOutput("RawControlCount.table")
                                       )
                              )
                          )

              )
              ),
      ## Pipeline ------
      tabItem(tabName = "pipe",
              tabsetPanel(id="TabsetPipe",
                tabPanel("pipeline",
                         fluidPage(
                          br(),
                          #first row
                          fluidRow(
                            
                            box(
                              title = "1. control coupling MFI data file", width = 3,height = "220px", solidHeader = TRUE, status = "success",
                              #File control coupling data input ------
                              fluidRow(column(12,offset = 0, 
                              fileInput('controlCouplingDataFile', 'control coupling data',
                                        accept=c('text/csv', 
                                                 'text/comma-separated-values,text/plain', 
                                                 '.csv'))
                              )),
                              fluidRow(
                              column(6,offset = 0,
                                     actionButton(inputId="cc_example_popup", label="example",icon = icon("question-circle"),style = "margin-top: 25px;",width = "100%")
                              ),
                            
                              column(6,offset = 0, 
                                     actionButton(inputId = "controlCouplingDataFileRESET", label = "reset", icon = icon("retweet"),style = "margin-top: 25px;",width = "100%")
                              )
                              
                            )
                            ),
                            box(
                              title = "2. ctrl coupling bead count file", width = 3,height = "220px", solidHeader = TRUE, status = "success",
                              #File coupling control bead data input ------
                              fluidRow(column(12,offset = 0, 
                              fileInput('controlCouplingDataBeadCountFile', 'ctrl. coupl. count data',
                                        accept=c('text/csv', 
                                                 'text/comma-separated-values,text/plain', 
                                                 '.csv'))
                              )),
                              fluidRow(
                              column(6,offset = 0,
                                     actionButton(inputId="control_bead_count_example_popup", label="example",icon = icon("question-circle"),style = "margin-top: 25px;",width = "100%")
                              ),
                              
                              column(6,offset = 0, 
                                     actionButton(inputId = "controlCouplingDataBeadCountFileRESET", label = "reset", icon = icon("retweet"),style = "margin-top: 25px;",width = "100%")
                              )       
                            )),
                            box(
                              title = "3. assay MFI data file", width = 3,height = "220px", solidHeader = TRUE, status = "danger",
                              #File assay data input ------
                              fluidRow(column(12,offset = 0, 
                              fileInput('AssayDataFile', 'assay data',
                                        accept=c('text/csv', 
                                                 'text/comma-separated-values,text/plain', 
                                                 '.csv'))
                              )),
                              fluidRow(
                              column(6,offset = 0,
                                     actionButton(inputId="assay_example_popup", label="example",icon = icon("question-circle"),style = "margin-top: 25px;",width = "100%")
                              ),
                              column(6,offset = 0, 
                                     actionButton(inputId = "AssayDataFileRESET", label = "reset", icon = icon("retweet"),style = "margin-top: 25px;",width = "100%")
                              ) 
                              )
                            ),
                            box(
                              title = "4. assay data bead count file", width = 3,height = "220px", solidHeader = TRUE, status = "danger",
                              #File assay data bead count input  ------
                              fluidRow(column(12,offset = 0, 
                              fileInput('AssayDataBeadCountFile', 'assay bead count data',
                                        accept=c('text/csv', 
                                                 'text/comma-separated-values,text/plain', 
                                                 '.csv'))
                              )),
                              fluidRow(
                              column(6,offset = 0,
                                     actionButton(inputId="assay_bead_count_example_popup", label="example",icon = icon("question-circle"),style = "margin-top: 25px;",width = "100%")
                              ),
                              column(6,offset = 0, 
                                     actionButton(inputId = "AssayDataBeadCountFileRESET", 
                                                  label = "reset",
                                                  icon = icon("retweet"),
                                                  style = "margin-top: 25px;",
                                                  width = "100%")
                              )
                              ) 
                            )
                            
                          ),
                          #second row
                          fluidRow(
                            column(width=3,
                                   
                                   
                                   
                            box(
                              title = "5. meta data file", width = NULL,height = "240px", solidHeader = TRUE, status = "warning",
                              #File meta data input ------
                              fluidRow(column(12,offset = 0, 
                                              
                                              fileInput('SampleMetaDataFile', 'META DATA',
                                                        accept=c('text/csv', 
                                                                 'text/comma-separated-values,text/plain', 
                                                                 '.csv'))
                              )),
                              fluidRow(
                                column(6,offset = 0,
                                       actionButton(inputId="meta_file_example_popup", label="example",icon = icon("question-circle"),style = "margin-top: 25px;",width = "100%")
                                       
                                ),
                                column(6,offset = 0,
                                       actionButton(inputId = "SampleMetaDataFileRESET", label = "reset", icon = icon("retweet"),style = "margin-top: 25px;",width = "100%")
                                       
                                )
                              )
                              
                            ),
                            fluidRow(
                              #action button for run data processing ------
                              column(width=12,
                                     actionButton(inputId = "inputButton_data_processing",
                                                  label = "RUN data processing...",
                                                  width = "100%",
                                                  icon=icon("youtube"),
                                                  style="color:#fff; background-color: #000000; border-color: #212121; font-size: 24px; height: 80px")
                              )
                            )
                            ),
                            #additional parameter input , naming .... ------
                            
                            box(
                              title = "parameters", width = 9,height = "340px", solidHeader = TRUE, status = "info",
                              column(width=3,
                                     h4("select and type in parameters."),
                                     #h4("They will be checked before the analysis is starting !"),
                                     selectInput(inputId = "blank_corr_method",
                                                 label = "Blank correction method for assay data:",
                                                 choices = c("mean+3SD","median+IQR","mean","median","none"),
                                                 selected = "mean+3SD",
                                                 multiple = F),
                                     bsPopover(id = "blank_corr_method", 
                                               title = NULL, 
                                               content ="selection of different blank correction methods (default: mean + 3SD)", 
                                               placement = "bottom", 
                                               trigger = "hover",
                                               options = NULL),
                                     hr(),
                                     awesomeRadio("sep", "separator for input files ?",c(Tab="\t",Comma=",",Semicolon=";"),selected='\t') #input file separator
 
                              ),
                              column(width=3,
                                      textInput(inputId = "dilution_naming",label = "name: dilution column",value = "Dilution"),
                                      bsPopover(id = "dilution_naming", 
                                               title = NULL, 
                                               content ="type in the name used for the dilution column (pay attention to upper and lower case)", 
                                               placement = "bottom", 
                                               trigger = "hover",
                                               options = NULL),
                                      textInput(inputId = "replicate_naming",label = "name: replicate column",value = "Replicate"),
                                     bsPopover(id = "replicate_naming", 
                                               title = NULL, 
                                               content ="type in the name used for the replicate column (pay attention to upper and lower case)", 
                                               placement = "bottom", 
                                               trigger = "hover",
                                               options = NULL),
                                      textInput(inputId = "sample_naming",label = "name: sample column ",value = "Sample"),
                                     bsPopover(id = "sample_naming", 
                                               title = NULL, 
                                               content ="type in the name used for the sample column (pay attention to upper and lower case)", 
                                               placement = "bottom", 
                                               trigger = "hover",
                                               options = NULL)
                                      
                              ),
                              column(width=3,
                                      textInput(inputId = "antiTag_naming",label = "name: antiTag for coupling control",value = "His_AB"),
                                      bsPopover(id = "antiTag_naming", 
                                               title = NULL, 
                                               content ="type in the sample entry used in the coupling control data for the Anti-Tag reaction (pay attention to upper and lower case)", 
                                               placement = "bottom", 
                                               trigger = "hover",
                                               options = NULL),
                                      textInput(inputId = "controlBlank_naming",label = "name: Blank entry coupling ctrl.",value = "Blank"),
                                      bsPopover(id = "controlBlank_naming", 
                                               title = NULL, 
                                               content ="type in the sample entry used in the coupling control data for the blank reaction (pay attention to upper and lower case)", 
                                               placement = "bottom", 
                                               trigger = "hover",
                                               options = NULL),
                                      textInput(inputId = "Blank_naming",label = "name: Blank entry assay",value = "Blank"),
                                      bsPopover(id = "Blank_naming", 
                                               title = NULL, 
                                               content ="type in the sample entry used in the assay data for the Blank reaction (pay attention to upper and lower case)", 
                                               placement = "bottom", 
                                               trigger = "hover",
                                               options = NULL)
                                     ),
                              column(width=3,
                                     numericInput(inputId = "min_lin_r2_cutoff",label = "min. R-squared for lin. regression",value = 0.98),
                                     bsPopover(id = "min_lin_r2_cutoff", 
                                               title = NULL, 
                                               content ="type in the R-squared, which is used as a cutoff for linear regression selection, if a dilution series is linear or not, when the regression ended up in the linear decision tree arm (allowed: 0.5 - 1)", 
                                               placement = "bottom", 
                                               trigger = "hover",
                                               options = NULL),
                                     numericInput(inputId = "cv_var_cutoff",label = "CV cutoff for replicates",value = 0.5),
                                     bsPopover(id = "cv_var_cutoff", 
                                               title = NULL, 
                                               content ="type in the CV used for filtering measurements with replicates, if a CV is lower than the value the measurement of this dilution point is set to NA", 
                                               placement = "bottom", 
                                               trigger = "hover",
                                               options = NULL),
                                     textInput(inputId = "project_name", label="project name for output",value = "Project_data_analysis"), #project naming
                                     bsPopover(id = "project_name", 
                                               title = NULL, 
                                               content ="type in project name. The name is used as a prefix for file naming and will always appear in the visual inspection module of xMAPr", 
                                               placement = "bottom", 
                                               trigger = "hover",
                                               options = NULL)
                              )
                             )
                            )
                         )
                         ),
                    tabPanel(title = "pipeline overview",value = "pipeline.overview",
                  fluidPage(
                    fluidRow(
                      #raw data visualization // raw data tables  -----
                        tabsetPanel(
                             tabPanel("pipe parameters / progress",
                                      fluidRow(column(4,tableOutput("pipe_parameters"),
                                                      helpText("Download will be available once the processing is completed.")
                                                      ),
                                               column(width=2),
                                               column(6,
                                                      br(),
                                                      uiOutput("download_pipeline_analysis"),
                                                      hr(),
                                                      uiOutput("download_pipeline_analysis_plot_selection")
                                                      )
                                               ),
                                      fluidRow(column(width=1),
                                               column(width=6,
                                                      shinyWidgets::progressBar(id = "main", value = 0,status = "success",title = "Overall:",display_pct = TRUE),
                                                      shinyWidgets::progressBar(id = "antigen", value = 0,status = "warning",title = "Antigens:",display_pct = TRUE),
                                                      shinyWidgets::progressBar(id = "sample", value = 0,status = "info",title = "Samples:",display_pct = TRUE),
                                                      uiOutput("report_download_progressbar"),
                                                      uiOutput("report_plot_download_progressbar")
                                                )
                                               )
                                      ),
                             tabPanel("assay data raw",
                                      fluidPage(column(width=12,DT::dataTableOutput("assay.data.raw")))
                             ),
                             tabPanel("assay count data raw",
                                      fluidPage(column(width=12,DT::dataTableOutput("assay.count.data.raw")))
                             ),
                             tabPanel("control coupling data raw",
                                      fluidPage(column(width=12,DT::dataTableOutput("control.data.raw")))
                             ),
                             tabPanel("control count data raw",
                                      fluidPage(column(width=12,DT::dataTableOutput("control.count.data.raw")))
                             ),
                             tabPanel("sample meta data",
                                      fluidPage(column(width=12,DT::dataTableOutput("meta.data.raw")))
                             ),
                             tabPanel("input error messages",
                                      fluidPage(tableOutput("error.messages"))
                            )
                          )  
                      )
                    )
              
              )
              
              )
              ),
            ## END process pipeline UI -----
      tabItem(tabName = "pipe",
              h2("Data processing"),
              p("Hier dann die Auswahl der Files mit Kopplungskontrolle und Assay eingebaut werden,
               Selektion eines Output-Folders fuers RData-File, Run Button fuer die Prozessierung ueber das Skript,
                Progress bar/Ladeanimation bei Bearbeitung")
             
       
              ),
      tabItem(tabName = "expover",
              h2("Experiment overview"),
              fluidRow(
                column(10,
                       p("Hier Auswahl eines RData-Files fuer das zu betrachtende Experiment")
                )
              )
          
      ),
    
      
      #global result visualization -------
      tabItem(tabName = "resvis_panel",
              #h2("Result visualization"),
              tabBox(title = "Result visualization",
                     id = "resvis_panel_tab", height = "1200px",width=12,
                          tabPanel(title = "overview of responses",value="overview.of.responses",
                                   fluidPage(
                                           fluidRow(
                                        
                                             column(width = 2,offset = 8,
                                                    valueBoxOutput("AnalyteCountbox",width = 12)
                                             ),
                                             column(width = 2,
                                                    valueBoxOutput("SampleCountbox",width = 12)
                                             )
                                           ),
                                           fluidRow(
                                                    column(4,
                                                           addSpinner(plotlyOutput("proc.response.fitdist"), spin = "folding-cube", color = "#FF6600")
                                                                 
                                                           ),
                                                    column(4,
                                                           addSpinner(plotlyOutput("proc.response.overview1") , spin = "folding-cube", color = "#FF6600")
                                                           ),
                                                    column(4,
                                                           addSpinner(plotlyOutput("proc.response.overview2") , spin = "folding-cube", color = "#FF6600")
                                                           )
                                                    ),
                                           fluidRow(
                                             column(12,
                                                    actionButton(inputId="fit_types_help",
                                                                 label="explanation",
                                                                 icon = icon("question-circle"),
                                                                 style = "margin-top: 25px;",
                                                                 width = "100%"))
                                           )
                                           
                                   )
                                          
                        ),
                       tabPanel("dilution wise dynamic range plots",
                                fluidPage(
                                  column(12, align="center",
                                  addSpinner(plotOutput("proc.response.overview3",height = 850,width = 850) , spin = "folding-cube", color = "#FF6600")
                                  )
                                )
                                ),
                     tabPanel("quality measures",
                             tabsetPanel(
                               tabPanel("general overview",
                                        fluidPage(
                                           fluidRow(
                                             column(5,addSpinner(plotOutput("proc.gg.mfi.bead"), spin = "folding-cube", color = "#FF6600") ),
                                             column(5,addSpinner(plotOutput("proc.gg.mfi.cv"), spin = "folding-cube", color = "#FF6600") )
                                           )
                                        )
                               ),
                               tabPanel("coupling control MFI",
                                        column(12,
                                               addSpinner(plotlyOutput("proc.coupling.control"), spin = "folding-cube", color = "#FF6600") )
                                        ),
                               tabPanel("noise MFI",
                                        column(12,
                                               addSpinner(plotlyOutput("proc.noise"), spin = "folding-cube", color = "#FF6600") ),
                                        helpText("grey dots = mean blank (background) / color gradient dots = mean blank + 3xSD of blank (LOD)")
                                        ),
                               tabPanel("bead-count & LOD filtering",
                                        fluidRow(
                                          column(3,selectInput(inputId = "filter.order.stacked.barplot", label = "raw values sort according to...", 
                                                               choices=c("coupling_beadCount_low","assay_beadCount_low","LOD_filtering"),
                                                               selected = "assay_beadCount_low")
                                                 ),
                                        
                                          column(3,addSpinner(plotlyOutput("proc_gg_filtered"), spin = "folding-cube", color = "#FF6600") ),
                                          column(3,addSpinner(plotlyOutput("proc_gg_filtered.fit"), spin = "folding-cube", color = "#FF6600") )
                                          )
                                        ),
                              tabPanel("dilution-wise signal drop filtering",           
                                        fluidRow(
                                          column(5,addSpinner(plotlyOutput("proc_gg_filtered.signal.dropout"), spin = "folding-cube", color = "#FF6600") )
                                          )
                                        ),
                               tabPanel("heatmap of NA values (=1) and valid values (=0)",
                                          column(9,addSpinner(plotlyOutput("heatmap.response.na"), spin = "folding-cube", color = "#FF6600") ),
                                        helpText("grey = valid values / black = missing values")
                                        
                                        
                               ),
                               tabPanel("bead count heatmap",
                                        column(12, align="center",
                                        addSpinner(plotlyOutput("heatmap.beadcount"), spin = "folding-cube", color = "#FF6600") 
                                        )
                               )                
                     )
                     ),
                       tabPanel("2D PCA",
                                br(),
                                fluidRow(column(10, 
                                                align="center",
                                                addSpinner(plotOutput("facto.plot.2d",height = "700px") , spin = "folding-cube", color = "#FF6600")
                                                ),
                                         column(2,
                                                fluidRow(
                                                           shinyWidgets::dropdownButton( #add menu directly to plot (shinyWidgets)
                                                           tags$h3("2D PCA plot adjustments:"),
                                                           checkboxInput("shownam", "show labels ?", value = FALSE),
                                                           checkboxInput("ellipses", "add ellipses ?", value = TRUE),
                                                           checkboxInput("show.mean.points", "show center points ?", value = FALSE),
                                                           selectInput("ellipses.method","ellipses calculation method",c("norm","convex","t","euclid"),selected = "convex"),
                                                           numericInput("ellipses.level","size of the concentration ellipse in normal probability:",value=0.95),
                                                           numericInput("ellipses.alpha","transparency of ellipses:",value=0.1),
                                                           sliderInput("plot.point.size","size of the points:",min = 1,max = 10,value=4,step = 0.5),
                                                           circle = TRUE, status = "danger", icon = icon("wrench"), width = "200px",
                                                           tooltip = tooltipOptions(title = "Click to open plot adjustments !")
                                                            )
                                                        ),
                                                fluidRow(
                                                  br(),
                                                  uiOutput("PCA.knob1")
                                                ),
                                                fluidRow(
                                                  uiOutput("PCA.knob2")
                                                )
                                                
                                            )
                                         ),
                                fluidRow(column(5,addSpinner(plotOutput("facto.plot.1D.selection1"), spin = "folding-cube", color = "#FF6600")  ),
                                         column(5,addSpinner(plotOutput("facto.plot.1D.selection2"), spin = "folding-cube", color = "#FF6600")  ))
                       ),
                       tabPanel("3D PCA",
                                br(),
                                column(10,addSpinner(plotlyOutput("facto.plot.3d",height = "700px", width = "auto"), spin = "folding-cube", color = "#FF6600")  )
                                
                       ),
                       tabPanel("Scree plot of PCA",
                                br(),
                                column(12, 
                                       align="center",
                                       addSpinner(plotOutput("facto.plot.scree",width = "1000px",height = "500px"), spin = "folding-cube", color = "#FF6600") 
                                       )
                                
                       ),
                     tabPanel("heatmap (response data)",
                              tabsetPanel(
                                  tabPanel("heatmap",
                                           column(9,
                                                  align="center",
                                                  addSpinner(plotlyOutput("heatmap.response"), spin = "folding-cube", color = "#FF6600") 
                                                  )
                                           )
                                  
                                  ),
                                    
                              box(id = "heatmapSelect",title = "heatmap parameters",width = 3,
                                         fluidPage(
                                           numericInput(inputId="heatmap.response.ROW.cluster.count", label="number of cluster (row)", value=3, min = 1, max = 10),
                                           numericInput(inputId="heatmap.response.COLUMN.cluster.count", label="number of cluster (column)", value=3, min = 1, max = 10),
                                           numericInput(inputId="heatmap.response.ROW.text.size", label="text size (row)", value=5, min = 1, max = 20),
                                           numericInput(inputId="heatmap.response.COLUMN.text.size", label="text size (column)", value=5, min = 1, max = 20),
                                           selectInput(inputId="heatmap.response.dist.method", label="distance method", choices = c("euclidean", "maximum", "manhattan", "binary" , "minkowski"), selected = "euclidean"),
                                           selectInput(inputId="heatmap.response.hclust.method", label="cluster method", choices = c( "ward.D", "ward.D2", "single", "complete", "average", "mcquitty" ), selected = "complete"),
                                           selectInput(inputId="heatmap.response.Colv", label="cluster columns ?", choices = c( "YES","NO"), selected = "YES"),
                                           selectInput(inputId="heatmap.response.Rowv", label="cluster rows ?", choices = c( "YES","NO"), selected = "YES"),
                                           hr(),
                                           downloadButton(outputId="downloadResponseHeatmap",label = "download heatmap"),
                                           uiOutput("heatmap_to_delete_antigens"),
                                           uiOutput("heatmap_to_delete_samples")
                                           
                                         )
                                  )
                                
                                
                              
                              
                     )
                     
                     
                     
                     

              )
              
        
      )
      ,
      #individual antigen/sample fit ------

      tabItem(tabName = "sass",
              box(title = "response inspection",status = "primary",
                     id = "response_inspect_box", height = "560px",width=12,
                             
                        fluidRow(
                                  column(10,
                                         h1(textOutput("selectedAntigenSample")),
                                         addSpinner(plotOutput("plots.out.gg.AntigenSample.all") , spin = "folding-cube", color = "#FF6600") 
                                  ),
                                  column(2,
                                       valueBoxOutput("plots.out.gg.AntigenSample.fit.type",width="100%"),
                                       uiOutput("antigen.selection"),
                                       uiOutput("sample.selection"),
                                       
                                       checkboxGroupButtons(
                                         inputId = "axis.log.scale", label = "select axis for log scale :", 
                                         choices = c("x-axis", "y-axis"),
                                         width="200px",
                                         justified = TRUE, status = "primary",
                                         checkIcon = list(yes = icon("ok-sign", lib = "glyphicon"), no = icon("remove-sign", lib = "glyphicon"))
                                       ),
                                       hr(),
                                       uiOutput("UIdownloadSinglePlots"),
                                       hr(),
                                       uiOutput("highlight.antigen.sample.checkbox")
                                )
                              )
                              
                       
              ),
              box(title = "response table",status = "success",
                     id = "response_table_box", height = "800px",width=12,
                      fluidPage(
                        fluidRow(
                          dataTableOutput("resptable") 
                        )
                      )
                       

              )
      ),
     
      
      #statistical analysis  -----
      tabItem(tabName = "stats",
          tabsetPanel(id="TabsetStats",
            tabPanel("overview",
                                   
              fluidRow(
                box(title = "violinplot of antigen",status = "primary",
                    id = "violinplot", height = "480px",width=5,
                  column(12,
                         addSpinner(plotOutput("plot.antigen.violin.test",height = "400px"), spin = "folding-cube", color = "#FF6600")  )
                  ),
                box(title = "p-value adjustment",status = "primary",
                    id = "p.value.adjustment", height = "480px",width=4,
                    
                    addSpinner(plotlyOutput("plot.p.values.test",height = "350px",width = "350px"), spin = "folding-cube", color = "#FF6600")  
                    )
                ,
                box(title = "used selection for stat. testing and plot",status = "primary",
                    id = "violinplot", height = "480px",width=3,
                  column(12,
                           checkboxGroupButtons(
                           inputId = "axis.log.scale.test", label = "select axis for log scale :", 
                           choices = c("y-axis"),
                           width="200px",
                           justified = TRUE, status = "primary",
                           checkIcon = list(yes = icon("ok-sign", lib = "glyphicon"), no = icon("remove-sign", lib = "glyphicon"))
                         ),
                         hr(),
                         valueBoxOutput("stats.resptable.meta.selection",width="100%"),
                         br(),
                         uiOutput("antigen.selection.stats"),
                         hr(),
                         h5("download statistical analysis with the selected meta data:"),
                         downloadButton("download.stats.resptable", "Download Statistics", style="float:right"),
                         downloadButton("download_violin_plot", "Download violin-plot", style="float:left")
                         
                  )
                )
                         
                         
                         
                    
                     ),
              fluidRow(
              box(title = "statistic table",status = "success",
                  id = "stat_response_table_box",width=12,
                  fluidPage(
                    fluidRow(
                      addSpinner(dataTableOutput("stats.resptable",height = "900px") , spin = "dots", color = "#FF6600")
                      
                          )
                        )
                        
                    )
                    
               )
             ),
            tabPanel("Volcano plots",
                     fluidRow(
                       column(10,
                              align = "center",
                              uiOutput("volcanoPlotUI")
                       ),
                       column(2,
                              sliderInput(inputId="volcanoHeight", label = "height", min = 300, max = 2000, value = 1200, step = 50),
                              sliderInput(inputId="volcanoWidth", label = "width", min = 350, max = 2000, value = 1150, step = 50),
                              numericInput(inputId="volcano_FC_cut_off", label ="fold-change cut-off", value = 2),
                              numericInput(inputId="volcano_pvalue_cut_off", label = "p-value cut-off", value = 0.05),
                              radioButtons(inputId="volcano_p_value_type", label = "p-value raw or adjusted (BH)", choices = c("raw","adjusted"), selected = "adjusted"),
                              radioButtons(inputId="volcano_display_labels", label = "plot labels of sign. changed", choices = c("yes","no"), selected = "yes"),
                              sliderInput(inputId="volcano_theme_size", label = "base font size", min = 10, max = 40, value = 18, step = 2),
                              colourpicker::colourInput("volcano_up_col", "Select colour UP:", "orangered3",palette = "square", returnName = TRUE,showColour = c("background")),
                              colourpicker::colourInput("volcano_none_col", "Select colour NONE:", "gray",palette = "square", returnName = TRUE,showColour = c("background")),
                              colourpicker::colourInput("volcano_down_col", "Select colour DOWN:", "dodgerblue3",palette = "square", returnName = TRUE,showColour = c("background")),
                              downloadButton("download_volcano_plot", "Download Volcano plot", style="float:center")
                       )
                     )
                     
            
            )
          )
      )
    )
  )
)
#UI END