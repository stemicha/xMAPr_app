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
# dependencies.R
# 
# github: https://github.com/stemicha/
# author: Stephan Michalik#
#




# List the package and version as below
dependencies <- read.csv(textConnection("
                                        Package,     Min.Version,       source,     github_path
                                        plotly,       4.8.0,            CRAN, 
                                        ggsignif,     0.4.0,            CRAN,
                                        devtools,     2.0.1,            CRAN,
                                        colourpicker, 1.0,              CRAN,
                                        factoextra,   1.0.5,            CRAN,
                                        FactoMineR,   1.41,             CRAN,
                                        RColorBrewer, 1.1-2,            CRAN,
                                        gridExtra,    2.3,              CRAN,
                                        grid,         3.5.0,            CRAN,
                                        ggrepel,      0.8.0,            CRAN,
                                        ggpubr,       0.1.7,            CRAN,
                                        heatmaply,    0.15.2,           CRAN,
                                        tidyverse,    1.2.1,            CRAN,
                                        rlang,        0.2.1,            CRAN,
                                        data.table,   1.11.4,           CRAN,
                                        minpack.lm,   1.2-1,            CRAN,
                                        shinyFiles,   0.6.2,            CRAN,
                                        shinyWidgets, 0.4.3,            CRAN,
                                        shiny,        1.1.0,            CRAN,
                                        shinyjs,      1.0,              CRAN,
                                        magrittr,     1.5,              CRAN,
                                        shinydashboard, 0.7.0,          CRAN,
                                        scales,       1.0.0,            CRAN,
                                        shinyBS,      0.61,             CRAN,
                                        shinyalert,   1.0,              CRAN,
                                        readxl,       1.3.1,            CRAN,
                                        DT,           0.4,              CRAN,
                                        utils,        3.5.0,            CRAN
                                        "), stringsAsFactors = FALSE, strip.white = TRUE)



## No changes necessary below. ##

##Import installed package versions
 pkgs <- installed.packages()
 rownames(pkgs) <- c()
 pkgs <- data.frame(pkgs, stringsAsFactors = FALSE)

##Compare requirements to installed packages
 pkgs <- merge(dependencies, pkgs, by="Package", all.x=TRUE)

##Filter out packages meeting minimum version requirement
 pkgs <- pkgs[mapply(compareVersion, pkgs$Min.Version, pkgs$Version) > 0, ]

##Install missing and newer packages
 cran <- pkgs[pkgs$source=='CRAN', ]
 null <- lapply(cran$Package, install.packages)
 github <- pkgs[pkgs$source=='github', ]
 null <- lapply(github$github_path,devtools::install_github)

# Require dependencies [optional]
lapply(dependencies$Package, library, character.only=TRUE)

# install RTools windows
if(Sys.info()[['sysname']]=="Windows"){
  if(("installr" %in% rownames(installed.packages())) !=TRUE){
    devtools::install_github('talgalili/installr')
    installr::install.Rtools()
  }

}
