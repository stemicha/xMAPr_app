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
# server.R
# 
# github: https://github.com/stemicha/
# author: Stephan Michalik#
#



## increase upload size -----
options(shiny.usecairo=TRUE,shiny.maxRequestSize=2000*1024^2) 
#colors for fit types:
fit.type.colors<-c(nls="#0D47A1",nls_with_exclusion="#2196F3",`5P_log`="#BF360C",`5P_log_with_exclusion`="#FF5722",linear_fit="#FFD600",rejected="#795548",fit_failed="#607D8B")



shinyServer(function(input,output, session){
  

  #_________________________________________ ===================
  #download handler for demo data ------
  
  output$downloadDemoData <- downloadHandler(
    filename <- function() {
      paste("demo_data__xMAPr.zip", sep="")
    },
    
    content <- function(file) {
      file.copy("demo_data/demo_data__xMAPr.zip", file)
    },
    contentType = "application/zip"
  )
  
  
  
  
  
  
  #_________________________________________ ===================
  #for reset button of the app ------

  observeEvent(input$controlCouplingDataFileRESET, {
    shinyjs::reset("controlCouplingDataFile") # reset is a shinyjs function / reset controlCouplingDataFile input
  })
  observeEvent(input$controlCouplingDataBeadCountFileRESET, {
    shinyjs::reset("controlCouplingDataBeadCountFile")  # reset is a shinyjs function / reset controlCouplingDataBeadCountFile input
  })
  observeEvent(input$AssayDataFileRESET, {
    shinyjs::reset("AssayDataFile")  # reset is a shinyjs function / reset AssayDataFile input
  })
  observeEvent(input$AssayDataBeadCountFileRESET, {
    shinyjs::reset("AssayDataBeadCountFile")  # reset is a shinyjs function / reset AssayDataBeadCountFile input
  })
  observeEvent(input$SampleMetaDataFileRESET, {
    shinyjs::reset("SampleMetaDataFile")  # reset is a shinyjs function / reset SampleMetaDataFile input
  })
  observeEvent(input$csvRAWassayRESET, {
    shinyjs::reset("csvRAWassay") # reset is a shinyjs function / reset csvRAWassay input
  })
  observeEvent(input$csvRAWcontrolRESET, {
    shinyjs::reset("csvRAWcontrol")  # reset is a shinyjs function / reset csvRAWcontrol input
  })

  
  
  
  
  
  #_________________________________________ ===================
  #xPonent raw csv file conversion ------
  convertedRAWdata<-eventReactive(input$inputButton_raw_data_processing,{
    MFI.out.final<-c()
    count.out.final<-c()
    control.MFI.out.final<-c()
    control.count.out.final<-c()
    
                #shinyalert error pop ups
                if(is.null(input$csvRAWassay)){
                  shinyalert(
                    title = "no assay raw data selected !",
                    text = "need assay raw data input csv",
                    closeOnEsc = TRUE,
                    closeOnClickOutside = FALSE,
                    html = FALSE,
                    type = "error",
                    showConfirmButton = TRUE,
                    showCancelButton = FALSE,
                    confirmButtonText = "OK",
                    confirmButtonCol = "#AEDEF4",
                    timer = 0,
                    imageUrl = "logo_medium.png",
                    imageWidth = 100,
                    imageHeight = 100,
                    animation = TRUE
                  )
                }
                if(is.null(input$csvRAWcontrol)){
                shinyalert(
                  title = "no control coupling raw data selected !",
                  text = "need control coupling raw data input csv",
                  closeOnEsc = TRUE,
                  closeOnClickOutside = FALSE,
                  html = FALSE,
                  type = "error",
                  showConfirmButton = TRUE,
                  showCancelButton = FALSE,
                  confirmButtonText = "OK",
                  confirmButtonCol = "#AEDEF4",
                  timer = 0,
                  imageUrl = "logo_medium.png",
                  imageWidth = 200,
                  imageHeight = 200,
                  animation = TRUE
                    )
                }
                if(is.null(input$csvRAWcontrol) & is.null(input$csvRAWassay)){
                  shinyalert(
                    title = "no control coupling or assay raw data selected !",
                    text = "need control coupling and assay raw data input csv",
                    closeOnEsc = TRUE,
                    closeOnClickOutside = FALSE,
                    html = FALSE,
                    type = "error",
                    showConfirmButton = TRUE,
                    showCancelButton = FALSE,
                    confirmButtonText = "OK",
                    confirmButtonCol = "#AEDEF4",
                    timer = 0,
                    imageUrl = "logo_medium.png",
                    imageWidth = 200,
                    imageHeight = 200,
                    animation = TRUE
                  ) 
                }
                     
            
    if(!is.null(input$csvRAWcontrol) & !is.null(input$csvRAWassay)){
      
      shinyalert(
        title = "raw data processing ...",
        text = "this might take a while ...",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "success",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "logo_medium.png",
        imageWidth = 200,
        imageHeight = 200,
        animation = TRUE
      ) 
      
      for(i in 1:dim(input$csvRAWassay)[1]){
        t<-c();t<-read_lines(input$csvRAWassay$datapath[i])

          
        MFI.line<-c();MFI.line<-which(t=="\"DataType:\",\"Median\"") #line for MFI
        count.line<-c();count.line<-which(t=="\"DataType:\",\"Count\"")  #line for count
        space.line<-c();space.line<-which(t=="")                         #empty lines
        
        #setup endlines for count and MFI
        MFI.line.end<-c();MFI.line.end<-space.line[which(c(space.line-MFI.line)>0)[1]]
        count.line.end<-c();count.line.end<-space.line[which(c(space.line-count.line)>0)[1]]
        
        
        #MFI  
        #split lines into chunkes for MFI
        MFI.in<-c();MFI.in<-t[c(MFI.line+1):c(MFI.line.end-1)]
        
        #generate MFI out
        MFI.out<-c();MFI.out<-data.frame(Reduce(rbind, 
                                                lapply(MFI.in[-1],function(x){
                                                  tmp<-gsub('\"', "", x, fixed = TRUE)
                                                  unlist(strsplit(tmp,split = ","))
                                                })),
                                         row.names = NULL)
        #generate colnames for MFI out
        MFI.colnames<-gsub('\"', "", MFI.in[1], fixed = TRUE)
        MFI.colnames<-unlist(strsplit(MFI.colnames,split = ","))
        
        MFI.out<-MFI.out[,-c(1:2)] #remove location columns from dataframe
        colnames(MFI.out)<-MFI.colnames[-1] #remove location column name from vector and add column names
        MFI.out<-MFI.out[,-ncol(MFI.out)] #remove total events
        
        MFI.out<-separate(data = MFI.out,col = colnames(MFI.out)[1],sep = input$sepRAW,into = c("Sample","Replicate","Dilution"))
        
        #count
        #split lines into chunkes for count
        count.in<-c();count.in<-t[c(count.line+1):c(count.line.end-1)]
        
        #generate MFI out
        count.out<-c();count.out<-data.frame(Reduce(rbind, 
                                                    lapply(count.in[-1],function(x){
                                                      tmp<-gsub('\"', "", x, fixed = TRUE)
                                                      unlist(strsplit(tmp,split = ","))
                                                    })),
                                             row.names = NULL)
        #generate colnames for MFI out
        count.colnames<-gsub('\"', "", count.in[1], fixed = TRUE)
        count.colnames<-unlist(strsplit(count.colnames,split = ","))
        
        count.out<-count.out[,-c(1:2)] #remove location columns from dataframe
        colnames(count.out)<-count.colnames[-1] #remove location column name from vector and add column names
        count.out<-count.out[,-ncol(count.out)] #remove total events
        count.out<-separate(data = count.out,col = colnames(count.out)[1],sep = input$sepRAW,into = c("Sample","Replicate","Dilution"))
        
        #if multiple files are selected bind them together
        MFI.out.final<-rbind(MFI.out.final,MFI.out)
        count.out.final<-rbind(count.out.final,count.out)
        
        
      }
      
         
    }
    
    if(!is.null(input$csvRAWcontrol) & !is.null(input$csvRAWassay)){
      for(i in 1:dim(input$csvRAWcontrol)[1]){
        t<-c();t<-read_lines(input$csvRAWcontrol$datapath[i])
        
        
        MFI.line<-c();MFI.line<-which(t=="\"DataType:\",\"Median\"") #line for MFI
        count.line<-c();count.line<-which(t=="\"DataType:\",\"Count\"")  #line for count
        space.line<-c();space.line<-which(t=="")                         #empty lines
        
        #setup endlines for count and MFI
        MFI.line.end<-c();MFI.line.end<-space.line[which(c(space.line-MFI.line)>0)[1]]
        count.line.end<-c();count.line.end<-space.line[which(c(space.line-count.line)>0)[1]]
        
        
        #MFI  
        #split lines into chunkes for MFI
        MFI.in<-c();MFI.in<-t[c(MFI.line+1):c(MFI.line.end-1)]
        
        #generate MFI out
        MFI.out<-c();MFI.out<-data.frame(Reduce(rbind, 
                                                lapply(MFI.in[-1],function(x){
                                                  tmp<-gsub('\"', "", x, fixed = TRUE)
                                                  unlist(strsplit(tmp,split = ","))
                                                })),
                                         row.names = NULL)
        #generate colnames for MFI out
        MFI.colnames<-gsub('\"', "", MFI.in[1], fixed = TRUE)
        MFI.colnames<-unlist(strsplit(MFI.colnames,split = ","))
        
        MFI.out<-MFI.out[,-c(1:2)] #remove location columns from dataframe
        colnames(MFI.out)<-MFI.colnames[-1] #remove location column name from vector and add column names
        MFI.out<-MFI.out[,-ncol(MFI.out)] #remove total events
        

        #count
        #split lines into chunkes for count
        count.in<-c();count.in<-t[c(count.line+1):c(count.line.end-1)]
        
        #generate MFI out
        count.out<-c();count.out<-data.frame(Reduce(rbind, 
                                                    lapply(count.in[-1],function(x){
                                                      tmp<-gsub('\"', "", x, fixed = TRUE)
                                                      unlist(strsplit(tmp,split = ","))
                                                    })),
                                             row.names = NULL)
        #generate colnames for MFI out
        count.colnames<-gsub('\"', "", count.in[1], fixed = TRUE)
        count.colnames<-unlist(strsplit(count.colnames,split = ","))
        
        count.out<-count.out[,-c(1:2)] #remove location columns from dataframe
        colnames(count.out)<-count.colnames[-1] #remove location column name from vector and add column names
        count.out<-count.out[,-ncol(count.out)] #remove total events
        colnames(count.out)[1]<-"Sample"
        
        #if multiple files are selected bind them together
        control.MFI.out.final<-rbind(control.MFI.out.final,MFI.out)
        control.count.out.final<-rbind(control.count.out.final,count.out)
        
        
      }
      
      
    }
    
    
    #return output list for raw file conversion
    return(
      list(
        MFIassay=MFI.out.final,
        COUNTassay=count.out.final,
        MFIcontrol=control.MFI.out.final,
        COUNTcontrol=control.count.out.final
        
      )
    )
    
  })
  
  #render Output after raw data processing ------
  output$RawAssayMFI.table<-DT::renderDataTable({convertedRAWdata()$MFIassay},options = list(scrollX = TRUE, pageLength = 18))
  output$RawAssayCount.table<-DT::renderDataTable({convertedRAWdata()$COUNTassay},options = list(scrollX = TRUE, pageLength = 18))
  output$RawControlMFI.table<-DT::renderDataTable({convertedRAWdata()$MFIcontrol},options = list(scrollX = TRUE, pageLength = 18))
  output$RawControlCount.table<-DT::renderDataTable({convertedRAWdata()$COUNTcontrol},options = list(scrollX = TRUE, pageLength = 18))
  
  
  #generate download button for processed raw data------
  output$downloadProcessedRaw.UI<-renderUI({
    if(!is.null(convertedRAWdata()$MFIassay)){
      tagList(
        
        h4("download processed data:"),
        downloadButton(outputId = "downloadProcessedRaw",label = "download processed raw data",width = "100%",style="color:#FFFFFF; background-color: #34951E; border-color: #34631E"),
        helpText("zipped folder is ready for download"),
        hr()
      )
    }
    })
  
  #genrate download / zip folder for processed raw files --------
        output$downloadProcessedRaw <- downloadHandler(
          filename =paste(format(Sys.Date(),"%Y_%m_%d_"),input$RawProcessedName,".zip",sep=""),
          content = function(fname) {
            tmpdir <- gsub("//", "/", tempdir(), fixed = TRUE)
            tmpdir <- gsub("\\", "\\\\", tempdir(), fixed = TRUE)
            
            setwd(tmpdir)
            print(tmpdir)
            
            shinyalert(
              title = "generating download ...",
              text = "Please be patient this might take a while",
              closeOnEsc = TRUE,
              closeOnClickOutside = FALSE,
              html = FALSE,
              type = "info",
              showConfirmButton = TRUE,
              showCancelButton = FALSE,
              confirmButtonText = "OK",
              confirmButtonCol = "#AEDEF4",
              timer = 0,
              imageUrl = "logo_medium.png",
              imageWidth = 100,
              imageHeight = 100,
              animation = TRUE
            )
            #write data frames
            write_delim(x=convertedRAWdata()$MFIassay,file = paste(input$RawProcessedName,"_assayMFI.txt",sep=""),delim = "\t",col_names = T)
            write_delim(x=convertedRAWdata()$COUNTassay,file = paste(input$RawProcessedName,"_assayCOUNT.txt",sep=""),delim = "\t",col_names = T)
            write_delim(x=convertedRAWdata()$MFIcontrol,file = paste(input$RawProcessedName,"_controlMFI.txt",sep=""),delim = "\t",col_names = T)
            write_delim(x=convertedRAWdata()$COUNTcontrol,file = paste(input$RawProcessedName,"_controlCOUNT.txt",sep=""),delim = "\t",col_names = T)
            
            fs <- c(paste(input$RawProcessedName,"_assayMFI.txt",sep=""),
                    paste(input$RawProcessedName,"_assayCOUNT.txt",sep=""),
                    paste(input$RawProcessedName,"_controlMFI.txt",sep=""),
                    paste(input$RawProcessedName,"_controlCOUNT.txt",sep="")
            )
          #print
          print(fs)
          #zip files
          zip(zipfile=fname, files=fs)
          })
  
  #_________________________________________ ===================
  #pipeline shinyalert modals popups -----
                #coupling control data
                observeEvent(input$cc_example_popup, {
                  # Show a modal when the button is pressed
                  shinyalert(
                    title = "couplig control data table",
                    text = "This is an example of a data table input for the coupling control",
                    closeOnEsc = TRUE,
                    closeOnClickOutside = TRUE,
                    html = TRUE,
                    type = "info",
                    showConfirmButton = TRUE,
                    showCancelButton = FALSE,
                    confirmButtonText = "OK",
                    confirmButtonCol = "#000000",
                    timer = 0,
                    imageUrl = "table_coupling_control_data_example.png",
                    imageWidth = 450,
                    imageHeight = 200,
                    animation = TRUE
                  )
                })
              #coupling control count data
              observeEvent(input$control_bead_count_example_popup, {
                # Show a modal when the button is pressed
                shinyalert(
                  title = "couplig control bead count data table",
                  text = "This is an example of a data table input for the coupling control bead count",
                  closeOnEsc = TRUE,
                  closeOnClickOutside = TRUE,
                  html = TRUE,
                  type = "info",
                  showConfirmButton = TRUE,
                  showCancelButton = FALSE,
                  confirmButtonText = "OK",
                  confirmButtonCol = "#000000",
                  timer = 0,
                  imageUrl = "table_coupling_control_count_data_example.png",
                  imageWidth = 450,
                  imageHeight = 200,
                  animation = TRUE
                )
              })
                #assay data
                observeEvent(input$assay_example_popup, {
                  # Show a modal when the button is pressed
                  shinyalert(
                    title = "assay data table",
                    text = "This is an example of a data table input for the assay data",
                    closeOnEsc = TRUE,
                    closeOnClickOutside = TRUE,
                    html = TRUE,
                    type = "info",
                    showConfirmButton = TRUE,
                    showCancelButton = FALSE,
                    confirmButtonText = "OK",
                    confirmButtonCol = "#000000",
                    timer = 0,
                    imageUrl = "table_assay_data_example.png",
                    imageWidth = 450,
                    imageHeight = 300,
                    animation = TRUE
                  )
                })
                #assay bead count data
                observeEvent(input$assay_bead_count_example_popup, {
                  # Show a modal when the button is pressed
                  shinyalert(
                    title = "assay bead count data table",
                    text = "This is an example of a data table input for the assay bead count data",
                    closeOnEsc = TRUE,
                    closeOnClickOutside = TRUE,
                    html = TRUE,
                    type = "info",
                    showConfirmButton = TRUE,
                    showCancelButton = FALSE,
                    confirmButtonText = "OK",
                    confirmButtonCol = "#000000",
                    timer = 0,
                    imageUrl = "table_count_data_example.png",
                    imageWidth = 450,
                    imageHeight = 300,
                    animation = TRUE
                  )
                })
                #meta data
                observeEvent(input$meta_file_example_popup, {
                  # Show a modal when the button is pressed
                  shinyalert(
                    title = "meta data table",
                    text = "This is an example of a data table input for the meta data",
                    closeOnEsc = TRUE,
                    closeOnClickOutside = TRUE,
                    html = TRUE,
                    type = "info",
                    showConfirmButton = TRUE,
                    showCancelButton = FALSE,
                    confirmButtonText = "OK",
                    confirmButtonCol = "#000000",
                    timer = 0,
                    imageUrl = "table_meta_data_example.png",
                    imageWidth = 450,
                    imageHeight = 200,
                    animation = TRUE
                  )
                })
                #fit types explanation
                observeEvent(input$fit_types_help, {
                  # Show a modal when the button is pressed
                  shinyalert(
                    title = "fit types explanation",
                    text = "nls (nonlinear least squares);
                    nls_with_exclusion (nonlinear least squares + value exclusion [signal drop off]);
                    linear (linear fit, no saturation occurs over dilution stages);
                    rejected / fit_failed (no model applicable or model does not fit with choosen paramters",
                    closeOnEsc = TRUE,
                    closeOnClickOutside = TRUE,
                    html = TRUE,
                    type = "info",
                    showConfirmButton = TRUE,
                    showCancelButton = FALSE,
                    confirmButtonText = "OK",
                    confirmButtonCol = "#000000",
                    timer = 0,
                    imageUrl = "fit_types.png",
                    imageWidth = 450,
                    imageHeight = 500,
                    animation = TRUE
                  )
                })
                
                
  #_________________________________________ ===================
  #load data for pipeline data processing ------
        
                
            #Baltimore test ------- test data loading 
                #example.coupling.data<-fread(file.path("data/BALTIMORE_coupling_control.txt"),sep = "\t",header=T,data.table = F)
                #example.assay.data<-fread(file.path("data/BALITMORE_assay_data_artificial.txt"),sep = "\t",header=T,data.table = F)
                #example.count.data<-fread(file.path("data/BALITMORE_assay_data_bead_count_artificial.txt"),sep = "\t",header=T,data.table = F)
                #example.count.control.data<-fread(file.path("data/BALTIMORE_coupling_control beadcount artificial.txt"),sep = "\t",header=T,data.table = F)
                #example.meta.data<-fread(file.path("data/BALITMORE_meta_data_artificial.txt"),sep = "\t",header=T,data.table = F)
        
                example.coupling.data<-NULL
                example.assay.data<-NULL
                example.count.data<-NULL
                example.count.control.data<-NULL
                example.meta.data<-NULL
                
                        
                #coupling control data
                control_coupling_data <-  eventReactive(input$inputButton_data_processing,{
                  if(is.null(input$controlCouplingDataFile)){
                    dataframe <- example.coupling.data         
                  } else {
                  dataframe <- fread(
                    input$controlCouplingDataFile$datapath, 
                    sep=input$sep,
                    header = T,data.table = F,check.names = F)
                  }
                  dataframe[,1]<-as.character(dataframe[,1]) #convert first row to character, workaround if only numbers are provided as sampleID
                  return(dataframe)
                  
                }) 
                
                
                
                #assay data
                assay_data <- eventReactive(input$inputButton_data_processing,{
                  if(is.null(input$AssayDataFile)){
                    dataframe <- example.assay.data         
                  } else {
                    dataframe <- fread(
                      input$AssayDataFile$datapath, 
                      sep=input$sep,
                      header = T,data.table = F,check.names = F)
                    dataframe[,1]<-as.character(dataframe[,1]) #convert first row to character, workaround if only numbers are provided as sampleID
                    
                    
                  }
                  return(dataframe)
                })
                  
                #count data assay
                assay_count_data <-  eventReactive(input$inputButton_data_processing,{
                  if(is.null(input$AssayDataBeadCountFile)){
                    dataframe <- example.count.data       
                  } else {
                  dataframe <- fread(
                    input$AssayDataBeadCountFile$datapath, 
                    sep=input$sep,
                    header = T,data.table = F,check.names = F)
                  dataframe[,1]<-as.character(dataframe[,1]) #convert first row to character, workaround if only numbers are provided as sampleID
                  
                  }
                  return(dataframe)
                  
                }) 
                
                #count data coupling control
                control_count_data <-  eventReactive(input$inputButton_data_processing,{
                  if(is.null(input$controlCouplingDataBeadCountFile)){
                    dataframe <- example.count.control.data       
                  } else {
                    dataframe <- fread(
                      input$controlCouplingDataBeadCountFile$datapath, 
                      sep=input$sep,
                      header = T,data.table = F,check.names = F)
                    dataframe[,1]<-as.character(dataframe[,1]) #convert first row to character, workaround if only numbers are provided as sampleID
                    
                  }
                  return(dataframe)
                  
                }) 
                
                #meta data
                sample_meta_data <-  eventReactive(input$inputButton_data_processing,{
                  if(is.null(input$SampleMetaDataFile)){
                    dataframe <- example.meta.data        
                  } else {
                    dataframe <- fread(
                      input$SampleMetaDataFile$datapath, 
                      sep=input$sep,
                      header = T,data.table = F,check.names = F)
                    dataframe[,1]<-as.character(dataframe[,1]) #convert first row to character, workaround if only numbers are provided as sampleID
                    
                  }
                    return(dataframe)
                  
                  
                }) 
          
          
                
               
          # choose project name
          projectname <- eventReactive(input$inputButton_data_processing,{
            input$project_name
          })
          
         
#_________________________________________ ====================
          
          #switch on actionButtons ====
          
          #switch to input raw data viewer panel when process actionButton is clicked
          observeEvent(input$inputButton_data_processing, {
                updateTabsetPanel(session, "TabsetPipe",selected = "pipeline.overview")
                })
          #switch to input raw data viewer panel when process actionButton is clicked
          observeEvent(input$inputButton_data_visu, {
              updateTabItems(session, "side.menu", "resvis_panel")
            })
          
#_________________________________________ ====================       
          
        #pipeline: validate inputs=====
        validate.inputs<-eventReactive(input$inputButton_data_processing, {

                  am <- as_tibble(assay_data())        # assay file df input
                  am.count <- as_tibble(assay_count_data())  # assaycount file df input
                  ccm <- as_tibble(control_coupling_data())    # coupling control file df input
                  ccm.count <- as_tibble(control_count_data())    # coupling control file df input
                  meta <- as_tibble(sample_meta_data())   # meta file df input
                  
                  #locate columns
                  am.dil.col<-which(colnames(am)==input$dilution_naming)
                  am.rep.col<-which(colnames(am)==input$replicate_naming)
                  am.sample.col<-which(colnames(am)==input$sample_naming )
                  ccm.sample.col<-which(colnames(ccm)==input$sample_naming )
                  meta.sample.col<-which(colnames(meta)==input$sample_naming )
                  
                  #setup errors
                  error.out<-list(error.global.logical=FALSE,error.messages=NULL)
                  #do input error desting
                  if(sum(colnames(am)%in%input$dilution_naming)!=1){
                    error.out$error.messages<-c(error.out$error.messages,"used dilution name not found in assay data header")
                    error.out$error.global.logical=TRUE
                  }
                  if(sum(colnames(am.count)%in%input$dilution_naming)!=1){
                    error.out$error.messages<-c(error.out$error.messages,"used dilution name not found in assay bead count data header")
                    error.out$error.global.logical=TRUE
                  }
                  if(sum(colnames(am)%in%input$replicate_naming)!=1){
                    error.out$error.messages<-c(error.out$error.messages,"used dilution name not found in assay data header")
                    error.out$error.global.logical=TRUE
                  }
                  if(sum(colnames(am.count)%in%input$replicate_naming)!=1){
                    error.out$error.messages<-c(error.out$error.messages,"used replicate name not found in assay bead count data header")
                    error.out$error.global.logical=TRUE
                  }
                  if(sum(colnames(am)%in%input$sample_naming)!=1){
                    error.out$error.messages<-c(error.out$error.messages,"used sample name not found in assay data header")
                    error.out$error.global.logical=TRUE
                  }
                  if(sum(colnames(am.count)%in%input$sample_naming)!=1){
                    error.out$error.messages<-c(error.out$error.messages,"used sample name not found in assay bead count data header")
                    error.out$error.global.logical=TRUE
                  }
                  if(sum(colnames(meta)%in%input$sample_naming)!=1){
                    error.out$error.messages<-c(error.out$error.messages,"used sample name not found in meta data header")
                    error.out$error.global.logical=TRUE
                  }
                  if(length(which(am[,1]==input$Blank_naming))==0){
                    error.out$error.messages<-c(error.out$error.messages,"used blank name not found in assay data first column")
                    error.out$error.global.logical=TRUE
                  }
                  if(length(which(am.count[,1]==input$Blank_naming))==0){
                    error.out$error.messages<-c(error.out$error.messages,"used blank name not found in assay bead count data first column")
                    error.out$error.global.logical=TRUE
                  }
                  if(length(which(ccm[,1]==input$antiTag_naming))==0){
                    error.out$error.messages<-c(error.out$error.messages,"used antiTag name not found in coupling control data first column")
                    error.out$error.global.logical=TRUE
                  }
                  if(length(which(ccm.count[,1]==input$antiTag_naming))==0){
                    error.out$error.messages<-c(error.out$error.messages,"used antiTag name not found in coupling control bead count data first column")
                    error.out$error.global.logical=TRUE
                  }
                  if(length(which(ccm[,1]==input$controlBlank_naming))==0){
                    error.out$error.messages<-c(error.out$error.messages,"used blank name not found in coupling control data first column")
                    error.out$error.global.logical=TRUE
                  }
                  if(length(which(ccm.count[,1]==input$controlBlank_naming))==0){
                    error.out$error.messages<-c(error.out$error.messages,"used blank name not found in coupling control bead count data first column")
                    error.out$error.global.logical=TRUE
                  }
                  if(sum(colnames(am)[-c(am.dil.col,am.rep.col,am.sample.col)]==colnames(ccm)[-ccm.sample.col])!=ncol(ccm)-1){
                    error.out$error.messages<-c(error.out$error.messages,"assay data and control coupling data do not share the same number of antigens or antigens do not having the same order")
                    error.out$error.global.logical=TRUE
                  }
                  if(sum(colnames(am.count)[-c(am.dil.col,am.rep.col,am.sample.col)]==colnames(ccm.count)[-ccm.sample.col])!=ncol(ccm)-1){
                    error.out$error.messages<-c(error.out$error.messages,"assay bead count data and control coupling bead count data do not share the same number of antigens or antigens do not having the same order")
                    error.out$error.global.logical=TRUE
                  }
                  if(sum(colnames(am.count)[-c(am.dil.col,am.rep.col,am.sample.col)]==colnames(am.count)[-c(am.dil.col,am.rep.col,am.sample.col)])!=ncol(am)-3){
                    error.out$error.messages<-c(error.out$error.messages,"assay bead count data and assay data do not share the same number of antigens or antigens do not having the same order")
                    error.out$error.global.logical=TRUE
                  }
                  if(sum(colnames(ccm.count)[-ccm.sample.col]==colnames(ccm)[-ccm.sample.col])!=ncol(ccm)-1){
                    error.out$error.messages<-c(error.out$error.messages,"control coupling bead count data and control coupling data do not share the same number of antigens or antigens do not having the same order")
                    error.out$error.global.logical=TRUE
                  }
                  if(length(unique(am[,am.dil.col]))>=5){
                    error.out$error.messages<-c(error.out$error.messages,"number of dilutions to low! (min. 5 different dilutions required !)")
                    error.out$error.global.logical=TRUE
                  }
                 
                  
      
                  if(error.out$error.global.logical==FALSE){
                    shinyalert(
                      title = "Analyzing data...",
                      text = "all inputs are valid and the analysis has been executed ...",
                      closeOnEsc = TRUE,
                      closeOnClickOutside = TRUE,
                      html = TRUE,
                      type = "success",
                      showConfirmButton = TRUE,
                      showCancelButton = FALSE,
                      confirmButtonText = "OK",
                      confirmButtonCol = "#000000",
                      timer = 4000,
                      imageUrl = "logo_medium.png",
                      imageWidth = 450,
                      imageHeight = 200,
                      animation = TRUE
                    )
                  }else{
                    shinyalert(
                      title = "ERROR !",
                      text = paste(error.out$error.messages,collapse = "  /  "),
                      closeOnEsc = TRUE,
                      closeOnClickOutside = TRUE,
                      html = TRUE,
                      type = "error",
                      showConfirmButton = TRUE,
                      showCancelButton = FALSE,
                      confirmButtonText = "OK",
                      confirmButtonCol = "#000000",
                      timer = 4000,
                      imageUrl = "logo_medium.png",
                      imageWidth = 450,
                      imageHeight = 200,
                      animation = TRUE
                    )
      
                  }
                  
                  return(error.out)
            })
          
          #render error message table
          output$error.messages<-renderTable({validate.inputs()$error.messages},colnames = F,rownames = T)
          
          
      #INPUT: render output for raw input data =====================
          output$assay.data.raw<-DT::renderDataTable({assay_data()},options = list(scrollX = TRUE, pageLength = 25))
          output$control.data.raw<-DT::renderDataTable({control_coupling_data()},options = list(scrollX = TRUE, pageLength = 25))
          output$control.count.data.raw<-DT::renderDataTable({control_count_data()},options = list(scrollX = TRUE, pageLength = 25))
          output$assay.count.data.raw<-DT::renderDataTable({assay_count_data()},options = list(scrollX = TRUE, pageLength = 25))
          output$meta.data.raw<-DT::renderDataTable({sample_meta_data()},options = list(scrollX = TRUE, pageLength = 25))         

          
          
          
#_________________________________________ ====================
          
## analysis pipeline for response calculation -----                     
#                        
# pipeline: START of Response calculation pipeline -------------              
#                        
          # run all fit types
          # decide afterwards
                        

response.calculation <- eventReactive(input$inputButton_data_processing,{

  #setup  progress bar shinyWidget
   updateProgressBar(session = session,id = "main",value = 1, total = 7,title = paste("Step 1/7: prepare/write parameters..."))
   
            # pipeline: setup parameters ==================================
            am.dilution.column <- input$dilution_naming  #dilution column
            am.replicate.column <- input$replicate_naming #replicate column
            anti_tag.entry <- input$antiTag_naming       #antiTag entry coupling control 
            blank.entry <- input$Blank_naming            #Blank entry assay
            control.blank.entry <- input$controlBlank_naming  #Blank entry coupling control
            sample.column <- as.character(input$sample_naming)       #sample column over all inputs meta, assay data
            linear.r.squared.min <- input$min_lin_r2_cutoff    #r-squared cut-off for accepting linear fit
            cv.var <- input$cv_var_cutoff          # CV cut off
            n.fitted.points <- 20  #number of fitted points
      
            
            
            
            
        
  
 updateProgressBar(session = session,id = "main",value = 2, total = 7,title = paste("Step 2/7: setup data..."))
 
    #pipeline: do not execute if validation error == TRUE
    if(validate.inputs()$error.global.logical==FALSE){
              ## pipeline: setup of inputs ===============
              am <- as_tibble(assay_data())        # assay file df input
              am.count <- as_tibble(assay_count_data())  # assaycount file df input
              ccm <- as_tibble(control_coupling_data())    # coupling control file df input
              ccm.count <- as_tibble(control_count_data())    # coupling control file df input
              meta <- as_tibble(sample_meta_data())   # meta file df input
              
              #locate columns
              am.dil.col<-which(colnames(am)==am.dilution.column)
              am.rep.col<-which(colnames(am)==am.replicate.column)
              am.sample.col<-which(colnames(am)==sample.column)
              ccm.sample.col<-which(colnames(ccm)==sample.column)
              
              #CV cutoff usage
                  #remove Blank via subsetting
              use.CV.cutoff<-if(length(unique(na.omit(subset(assay_data(),assay_data()[,1]!=blank.entry)[,input$replicate_naming])))>1){TRUE}else{FALSE}
              
              ## pipeline: write parameter vector for output / no errors  ==================================
              param.out<-list(
                assay.input.file = if(is.null(input$AssayDataFile)){"missing/example file used"}else{input$AssayDataFile$name},
                assay.bead.count.input.file = if(is.null(input$AssayDataBeadCountFile)){"missing/example file used"}else{input$AssayDataBeadCountFile$name},
                control.input.file = if(is.null(input$controlCouplingDataFile)){"missing/example file used"}else{input$controlCouplingDataFile$name},
                control.bead.count.input.file = if(is.null(input$controlCouplingDataBeadCountFile)){"missing/example file used"}else{input$controlCouplingDataBeadCountFile$name},
                meta.input.file = if(is.null(input$SampleMetaDataFile)){"missing/example file used"}else{input$SampleMetaDataFile$name},
                sample.column.name = sample.column,
                dilution.column.name = am.dilution.column,
                replicate.column.name = am.replicate.column,
                anti_tag.entry = anti_tag.entry,
                blank.entry = blank.entry,
                control.blank.entry = control.blank.entry,
                cv.cut_off = cv.var,
                use.CV.cutoff = use.CV.cutoff,
                min.r.squared.for.linearFit=linear.r.squared.min,
                n.fitted.points = n.fitted.points,
                project.name = input$project_name
                
              )
              
            # pipeline: make data tidy =============
              
              am.tidy <- am %>% gather(colnames(am)[-c(1:3)],key = "antigen", value="MFI")
                    #am.tidy <- as_tibble(reshape2::melt(am,id.vars=colnames(am)[c(1:3)],variable.name="antigen",value.name = "MFI"))
                am.tidy$MFI_raw<-am.tidy$MFI #add MFI_raw
                am.count.tidy <- am.count %>% gather(colnames(am.count)[-c(1:3)],key = "antigen", value="BeadCount")
                  #am.count.tidy<-as_tibble(reshape2::melt(am.count,id.vars=colnames(am)[c(1:3)],variable.name="antigen",value.name = "BeadCount"))
                
                am.tidy<-left_join(am.tidy,am.count.tidy,by=c(sample.column,am.replicate.column,am.dilution.column,"antigen"))
                
                ccm.tidy<-ccm %>% gather(colnames(ccm)[-c(1)],key = "antigen", value="MFI")
                ccm.count.tidy<-ccm.count %>% gather(colnames(ccm.count)[-c(1)],key = "antigen", value="BeadCount")
                #ccm.tidy<-as_tibble(reshape2::melt(ccm,id.vars=colnames(ccm)[c(1)],variable.name="antigen",value.name = "MFI"))
                #ccm.count.tidy<-as_tibble(reshape2::melt(ccm.count,id.vars=colnames(ccm.count)[c(1)],variable.name="antigen",value.name = "BeadCount"))
  
                ccm.tidy<-left_join(ccm.tidy,ccm.count.tidy,by=c(sample.column,"antigen"))
                
                  # pipeline: do bead count filtering ==========
                      # convert MFI to NA which is below 35 beads // for assay data
                      am.tidy$assayMFI_BeadCount_filtered_logical<-FALSE
                          if(length(which(am.tidy$BeadCount<35))>0){
                            am.tidy$MFI[which(am.tidy$BeadCount<35)]<-NA
                            am.tidy$assayMFI_BeadCount_filtered_logical[which(am.tidy$BeadCount<35)]<-TRUE
                          }
                      # convert MFI to NA which is below 35 beads // for coupling control data
                      ccm.tidy$controlMFI_BeadCount_filtered_logical<-FALSE
                          if(length(which(ccm.tidy$BeadCount<35))>0){
                            ccm.tidy$MFI[which(ccm.tidy$BeadCount<35)]<-NA
                            ccm.tidy$controlMFI_BeadCount_filtered_logical[which(ccm.tidy$BeadCount<35)]<-TRUE
                            
                          }
    
 
 updateProgressBar(session = session,id = "main",value = 3, total = 7,title = paste("Step 3/7: blank substraction per antigen..."))
 
                # pipeline: do BLANK substraction LOD ==========
                  #generating mean blank substraction per antigen in // assay data file
                    mean_sd_blank <-  am.tidy%>%
                                      filter(UQ(as.name(sample.column))==blank.entry)%>%
                                      group_by(antigen)%>%
                                      summarise(mean_blank=mean(MFI,na.rm=T),sd_blank=sd(MFI,na.rm=T))%>%
                                      ungroup()
                    mean_sd_blank$sd_blank_3fold<-3*mean_sd_blank$sd_blank #3 fold SD
                    
                  #join blank and blank sd values to tidy df // assay data file
                    am.tidy<-left_join(am.tidy,mean_sd_blank,by="antigen")
                  
                  #do cutoff filtering of MFI with 3*SD of Blank + Blank as cutoff // assay data file
                      am.tidy$Blank_cutoff<-am.tidy$mean_blank+am.tidy$sd_blank_3fold
                      am.tidy$MFI_3foldSD_blank_filtered<-am.tidy$MFI
                      am.tidy$MFI_3foldSD_blank_filtered_logical<-FALSE
                          if(length(which(am.tidy$MFI_3foldSD_blank_filtered<am.tidy$Blank_cutoff))>0){
                            am.tidy$MFI_3foldSD_blank_filtered_logical[which(am.tidy$MFI_3foldSD_blank_filtered<am.tidy$Blank_cutoff)]<-TRUE
                            am.tidy$MFI_3foldSD_blank_filtered[which(am.tidy$MFI_3foldSD_blank_filtered<am.tidy$Blank_cutoff)]<-NA
                          }
                      am.tidy$MFI_3foldSD_blank_filtered_blank_substract<-am.tidy$MFI_3foldSD_blank_filtered-am.tidy$mean_blank
                
      
 
 updateProgressBar(session = session,id = "main",value = 4, total = 7,title = paste("Step 4/7: coupling control correction..."))
 
  
        #pipeline: do mean over replicates // coupling control data file  ==========
              ccm.tidy.mean <-  ccm.tidy%>%
                                group_by(UQ(as.name(sample.column)),antigen)%>%
                                summarise(mean_MFI=mean(MFI,na.rm=T))%>%
                                ungroup()%>%
                                spread(UQ(as.name(sample.column)),mean_MFI)
            #do mean over replicates // coupling control data file
              ccm.tidy.mean$coupling_control_MFI_blank_substracted<-unlist(ccm.tidy.mean[,anti_tag.entry])-unlist(ccm.tidy.mean[,control.blank.entry])
            #flag coupling control data which results in NA due to bead count
              ccm.tidy.mean$coupling_control_bead_count_to_low<-FALSE
                  if(sum(is.na(ccm.tidy.mean$coupling_control_MFI_blank_substracted))>0){
                    ccm.tidy.mean$coupling_control_bead_count_to_low[is.na(ccm.tidy.mean$coupling_control_MFI_blank_substracted)]<-TRUE
                  }
            #merge assay and coupling control data
              am.tidy<-left_join(am.tidy,ccm.tidy.mean,by="antigen")
            
            #normalize to coupling control data
              am.tidy$MFI_normalized<-am.tidy$MFI_3foldSD_blank_filtered_blank_substract/am.tidy$coupling_control_MFI_blank_substracted*100
  
            #remove blank entry from am.tidy
              am.tidy<-am.tidy%>%filter(UQ(as.name(sample.column))!=blank.entry)
 
 updateProgressBar(session = session,id = "main",value = 5, total = 7,title = paste("Step 5/7: calculate mean, sd and cv for assay data..."))
      
    
      #generate aggregated tibble over replicates
      
      am.tidy.aggregated <- filter(am.tidy,UQ(as.name(sample.column))!=blank.entry)%>%
                            group_by(UQ(as.name(sample.column)),UQ(as.name(am.dilution.column)),antigen)%>%
                            summarise(MFI_normalized_mean=mean(MFI_normalized,na.rm = T),
                                      MFI_normalized_sd=sd(MFI_normalized,na.rm = T),
                                      MFI_normalized_cv=sd(MFI_normalized,na.rm = T)/mean(MFI_normalized,na.rm = T))%>%
                            ungroup()
     
 
      #set MFI mean values to NA which to not meet CV criteria
      if(use.CV.cutoff){
        if(length(which(am.tidy.aggregated$MFI_normalized_cv>cv.var))){
          am.tidy.aggregated$MFI_normalized_mean[which(am.tidy.aggregated$MFI_normalized_cv>cv.var)]<-NA  
        }
      }
    
      #generate inverse xfold dilution
      am.tidy.aggregated$dilution.inverse<-1/unlist(am.tidy.aggregated[,input$dilution_naming])
      
      #signal drop estimation
      am.tidy.aggregated<-am.tidy.aggregated%>%group_by(UQ(as.name(sample.column)),antigen)%>%
        mutate(signal_drop=suppressWarnings(signaldrop(mfi = MFI_normalized_mean,absolute.dilution = dilution.inverse)$logical))%>%ungroup()

      
 updateProgressBar(session = session,id = "main",value = 6, total = 7,title = paste("Step 6/7: do regression over antigens and samples..."))
 
 
 
      #dilution vector
      dilu<-sort(unique(unlist(am.tidy.aggregated[,am.dilution.column])))

      
      #setup samples and antigen vectors
      samples<-unique(as.character(unlist(am.tidy.aggregated[,sample.column])))
      antigens<-unique(as.character(unlist(am.tidy.aggregated$antigen)))
      
      
      #on-fly filling data frames, full dataframe prebuilt
      sample.antigen.short<-c()
      sample.antigen.pred<-c()
      for(k in 1:length(samples)){
        sas.tmp<-rep(samples[k],length(antigens))
        sample.antigen.short<-c(sample.antigen.short,sas.tmp)
        sap.tmp<-rep(samples[k],length(antigens)*n.fitted.points)
        sample.antigen.pred<-c(sample.antigen.pred,sap.tmp)
      }
      
      
      antigen.sample.pred<-c()
      for(l in 1:length(antigens)){
        asp.tmp<-rep(antigens[l],n.fitted.points)
        antigen.sample.pred<-c(antigen.sample.pred,asp.tmp)
      }
      antigen.sample.pred<-rep(antigen.sample.pred,length(samples))
      
      out.selected.prediction.df<-as.data.frame(matrix(vector(),n.fitted.points*length(samples)*length(antigens),5))
          colnames(out.selected.prediction.df)<-c("dilution.input","MFI.predicted",sample.column,"antigen","fit.type")
          out.selected.prediction.df[,sample.column]<-sample.antigen.pred
          out.selected.prediction.df$antigen<-antigen.sample.pred
      
      out.predicted.df<-as.data.frame(matrix(vector(),n.fitted.points*length(samples)*length(antigens),6))
          colnames(out.predicted.df)<-c("dilution.input","nls" ,"nls_with_exclusion", "linear_fit",sample.column,"antigen")
          out.predicted.df[,sample.column]<-sample.antigen.pred
          out.predicted.df$antigen<-antigen.sample.pred
      
      out.response.df<-as.data.frame(matrix(vector(),length(samples)*length(antigens),4))
          colnames(out.response.df)<-c("response","antigen",sample.column,"fit.type")
          out.response.df[,sample.column]<-sample.antigen.short
          out.response.df$antigen<-rep(antigens,length(samples))
      
      out.res.comp<-as.data.frame(matrix(vector(),length(samples)*length(antigens),5))
          colnames(out.res.comp)<-c(sample.column,"antigen","nls" ,"nls_with_exclusion", "linear_fit" )
          out.res.comp[,sample.column]<-sample.antigen.short
          out.res.comp$antigen<-rep(antigens,length(samples))
      
      #loopwise filling data frames, only head prebuilt
      out.fit.summary.df<-as.data.frame(t(rep(NA,8)))
        colnames(out.fit.summary.df)<-c("Estimate","Std. Error","t value","Pr(>|t|)","coefficients",sample.column,"antigen","fit.type")
          out.fit.model.df<-c()
      
      out.qc<-as.data.frame(t(rep(NA,7)))
        colnames(out.qc)<-c("sigma","calc.dil","calc.MFI","response","fit.type",sample.column,"antigen")
     
        #residual comparison out
        res.comp.fit.out.final<-c()
        
        
        #calcuated value comp out
        calc.comp.out.final<-c()
                     #antigen loop
                     for(g in 1:length(antigens)){
                     
                       updateProgressBar(session = session,id = "antigen",value = g, total = length(antigens),title = paste("Antigens:",g,"of",length(antigens)))
                       
                       #sample loop
                       
                                      for(i in 1:length(samples)){
                                      
                                        updateProgressBar(session = session,id = "sample",value = i, total = length(samples),title = paste("Samples:",i,"of",length(samples)))
                                        
                                      
                                        #pipeline: select data subset for antigen >> g / sample >>i  =======
                                        tmp<-c()
                                        tmp<-subset(am.tidy.aggregated,am.tidy.aggregated$antigen==antigens[g] & am.tidy.aggregated[,sample.column]==samples[i])
                                        
                                        tmp$dilution.inverse<-1/unlist(tmp[,am.dilution.column])
                                        tmp$dilution.inverse.log10<-log10(tmp$dilution.inverse)
                                        tmp<-dplyr::arrange(tmp,desc(dilution.inverse)) #sort >> 1st dil. at top
                                        

                                        #setup dilution use (TRUE/FALSE)
                                        tmp$used.dilutions<-rep(TRUE,length(tmp$dilution.inverse))
                                        if(use.CV.cutoff){tmp$used.dilutions[tmp$MFI_normalized_cv>cv.var]<-FALSE}
                                        tmp$used.dilutions[is.na(tmp$MFI_normalized_mean)]<-FALSE
                                        
                                        #pipeline: prepare new data for fitted curve  =======
                                        new.data<-2^seq(log2(min(1/dilu)),log2(max(1/dilu)),length.out = n.fitted.points)
                                        new.log.data<-log10(new.data)
                                        pred.nls.noe<-c()
                                        pred.nls.exc<-c()
                                        pred.lin<-c()
                                        
                                        #reset output value collection
                                        fit.type<-NA
                                        
                                        #signal drop exclusion
                                        signaldrop.exclusion<-which(tmp$signal_drop!=TRUE)

                                 
                                        #pipeline: non-linear regression fitting including all data points  =======
                                        fit.mfi<-c()
                                        fit.mfi.dil.50<-NA;fit.mfi.MFImax.50<-NA;fit.mfi.resp<-NA
                                        if(sum(tmp$signal_drop,na.rm=T)==0 & !is.na(tmp$MFI_normalized_mean[which(tmp$dilution.inverse==max(tmp$dilution.inverse))])){
                                        fit.mfi <- try(nls(MFI_normalized_mean ~ SSmicmen_modified(dilution.inverse,MFImax, dil.50),data=tmp),silent = T)
                                        }
                                        


    
    
                                        if(length(grep("Error",fit.mfi))==0 &
                                           sum(!is.na(tmp$MFI_normalized_mean))>=4 & !is.null(fit.mfi)){
                                          fit.mfi.dil.50<-coef(fit.mfi)[2]
                                          fit.mfi.MFImax.50<-coef(fit.mfi)[1]/2
                                          fit.mfi.resp<-(1/fit.mfi.dil.50)*fit.mfi.MFImax.50
                                          out.res.comp[which(out.res.comp[,sample.column]==samples[i] & out.res.comp$antigen==antigens[g]),"nls"]<-fit.mfi.resp
                                        }
                                        
                                        #pipeline: non-linear regression fitting, signal drop dilution exclusion  =======
                                        fit.mfi.excl<-c()
                                        fit.mfi.excl.dil.50<-NA;fit.mfi.excl.MFImax.50<-NA;fit.mfi.excl.resp<-NA
                                        if(sum(tmp$signal_drop,na.rm=T)!=0 | is.na(tmp$MFI_normalized_mean[which(tmp$dilution.inverse==max(tmp$dilution.inverse))])){
                                          fit.mfi.excl <- try(nls(MFI_normalized_mean ~ SSmicmen_modified(dilution.inverse,MFImax, dil.50),data=tmp[signaldrop.exclusion,]),silent = T)
                                        }else{
                                          fit.mfi.excl="Error"
                                        }
                                        
                                        
                                        if(length(grep("Error",fit.mfi.excl))==0 & sum(!is.na(tmp$MFI_normalized_mean))>=4){
                                          fit.mfi.excl.dil.50<-coef(fit.mfi.excl)[2]
                                          fit.mfi.excl.MFImax.50<-coef(fit.mfi.excl)[1]/2
                                          fit.mfi.excl.resp<-(1/fit.mfi.excl.dil.50)*fit.mfi.excl.MFImax.50
                                          out.res.comp[which(out.res.comp[,sample.column]==samples[i] & out.res.comp$antigen==antigens[g]),"nls_with_exclusion"]<-fit.mfi.excl.resp
                                        }
                                        
                                        
                                        
                                        
                                        #pipeline: linear regression =======
                                        fit.lin.mfi<-c()
                                        fit.lin.dil.50<-NA;fit.lin.MFImax<-NA;fit.lin.resp<-NA;fit.lin.mfi.r.squared<-NA
                                        
                                        if(sum(!is.na(tmp$MFI_normalized_mean))>=4 & !is.na(tmp$MFI_normalized_mean[which(tmp$dilution.inverse==max(tmp$dilution.inverse))]) & sum(tmp$signal_drop,na.rm=T)==0){
                                          fit.lin.mfi<-lm(MFI_normalized_mean ~ dilution.inverse,data = tmp)
                                          fit.lin.mfi.r.squared<-summary(fit.lin.mfi)$r.squared
                                          if(!is.na(summary(fit.lin.mfi)$r.squared)){
                                            fit.lin.dil.50<-1/dilu[which(max(tmp$dilution.inverse)==tmp$dilution.inverse)]
                                            fit.lin.MFImax<-fitted(fit.lin.mfi)[which(max(tmp$dilution.inverse)==tmp$dilution.inverse)]
                                            fit.lin.resp<-(1/fit.lin.dil.50)*fit.lin.MFImax
                                            out.res.comp[which(out.res.comp[,sample.column]==samples[i] & out.res.comp$antigen==antigens[g]),"linear_fit"]<-fit.lin.resp
                                          }
                                        }
                                        
                                        #pipeline: predictions & correlations ========
                                        
                                        out.selected.prediction.df[which(out.selected.prediction.df[,sample.column]==samples[i] & out.selected.prediction.df$antigen==antigens[g]),"dilution.input"]<-new.data
                                        out.predicted.df[which(out.predicted.df[,sample.column]==samples[i] & out.predicted.df$antigen==antigens[g]),"dilution.input"]<-new.data
                                        
                                        pred.nls.noe<-c();pred.nls.exc<-c();pred.fpl<-c();pred.fpl.exc<-c();pred.lin<-c()
                                        pred.mat.nls.noe<-c();pred.mat.nls.exc<-c();pred.mat.fpl<-c();pred.mat.fpl.exc<-c();pred.mat.lin<-c()
                                        corr.nls.noe<-c();corr.nls.exc<-c();corr.fpl<-c();corr.fpl.exc<-c();corr.lin<-c()
                                        #pipeline: predictions & correlations / NLS ###########
                                        if(!is.na(fit.mfi.resp)){
                                          pred.nls.noe<-as.data.frame(cbind(dilution.input=new.data,
                                                                            MFI.predicted=coef(fit.mfi)[1] * new.data / (coef(fit.mfi)[2] + new.data)))
                                          
                                          out.predicted.df[which(out.predicted.df[,sample.column]==samples[i] & out.predicted.df$antigen==antigens[g]),"nls"]<-pred.nls.noe$MFI.predicted
                                          
                                          pred.mat.nls.noe<-as.data.frame(cbind(dilution.input=tmp$dilution.inverse,
                                                                                MFI.input=tmp$MFI_normalized_mean,
                                                                                MFI.predicted=coef(fit.mfi)[1]*tmp$dilution.inverse/(coef(fit.mfi)[2]+tmp$dilution.inverse)))
                                          
                                          corr.nls.noe<-cor(pred.mat.nls.noe$MFI.input,pred.mat.nls.noe$MFI.predicted,use="complete.obs")
                                        }
                                        
                                        #pipeline: predictions & correlations / NLS with excl. ###########
                                        if(!is.na(fit.mfi.excl.resp)){
                                          pred.nls.exc<-as.data.frame(cbind(dilution.input=new.data,
                                                                            MFI.predicted=coef(fit.mfi.excl)[1] * new.data / (coef(fit.mfi.excl)[2] + new.data)))
                                          
                                          out.predicted.df[which(out.predicted.df[,sample.column]==samples[i] & out.predicted.df$antigen==antigens[g]),"nls_with_exclusion"]<-pred.nls.exc$MFI.predicted
                                          
                                          pred.mat.nls.exc<-as.data.frame(cbind(dilution.input=tmp$dilution.inverse,
                                                                                MFI.input=tmp$MFI_normalized_mean,
                                                                                MFI.predicted=coef(fit.mfi.excl)[1]*tmp$dilution.inverse/(coef(fit.mfi.excl)[2]+tmp$dilution.inverse)))
                                          
                                          corr.nls.exc<-cor(pred.mat.nls.exc$MFI.input,pred.mat.nls.exc$MFI.predicted,use="complete.obs")
                                        }
                                        

                                        
                                        #pipeline: predictions & correlations / linear fit ###########
                                        if(!is.na(fit.lin.resp)){
                                          pred.lin<-as.data.frame(cbind(dilution.input=new.data,
                                                                        MFI.predicted= coef(fit.lin.mfi)[2]* new.data + coef(fit.lin.mfi)[1]))
                                          
                                          out.predicted.df[which(out.predicted.df[,sample.column]==samples[i] & out.predicted.df$antigen==antigens[g]),"linear_fit"]<-pred.lin$MFI.predicted
                                          
                                          pred.mat.lin<-as.data.frame(cbind(dilution.input=tmp$dilution.inverse,
                                                                            MFI.input=tmp$MFI_normalized_mean,
                                                                            MFI.predicted=coef(fit.lin.mfi)[2]* tmp$dilution.inverse + coef(fit.lin.mfi)[1]))
                                          
                                          corr.lin<-cor(pred.mat.lin$MFI.input,pred.mat.lin$MFI.predicted,use="complete.obs")
                                        }
                                        
                                        
                                        
                                        ##pipeline: fit decisions ==========

                                          #length of MFI exclusion is unequal to length of dilutions == missing values due to signal drop or MFI or bead count
                                          #// not all dilution can be used sec. antibody unsaturated or first dilution value = NA
                                           if(sum(tmp$signal_drop,na.rm=T)!=0 | 
                                              is.na(tmp$MFI_normalized_mean[which(tmp$dilution.inverse==max(tmp$dilution.inverse))])){
                                            
                                             #nls model failed , with signal drop
                                             if(is.na(fit.mfi.excl.resp)){
                                                       fit.type<-"fit_failed"
                                             }else{#End nls model failed, with signal drop
                                               if(fit.mfi.excl.dil.50<min(tmp$dilution.inverse)){
                                                 fit.type<-"rejected"
                                               }else{
                                                 fit.type<-"nls_with_exclusion"
                                                 
                                               }
                                               }

                                              
                                           }
                                          #only nls
                                          
                                          
                                          #length of ranked exclusion is equal to length of dilutions // all dilution can be used sec. antibody saturated
                                          if(sum(tmp$signal_drop,na.rm=T)==0 & !is.na(tmp$MFI_normalized_mean[which(tmp$dilution.inverse==max(tmp$dilution.inverse))])){
                                            
                                            #nls model failed
                                            if(is.na(fit.mfi.resp)){
                                        
                                                  if(!is.na(fit.lin.resp)){
                                                    if(fit.lin.mfi.r.squared>=input$min_lin_r2_cutoff){
                                                      fit.type<-"linear_fit"
                                                    }else{
                                                      fit.type<-"fit_failed"
                                                    } 
                                                
                                                  }else{
                                                    fit.type<-"fit_failed" #both na
                                              }
                                              
                                            }else{#END nls model failed
                                              if(fit.mfi.dil.50<min(tmp$dilution.inverse)){
                                                fit.type<-"rejected"
                                              }else{
                                              fit.type<-"nls"
                                              }
                                            }
                                          
                                          }
                                        
                                        
                                        out.response.df[which(out.response.df[,sample.column]==samples[i] & out.response.df$antigen==antigens[g]),"fit.type"]<-fit.type
                                        ##pipeline: END:fit decisions ==========

                                        
                                        #generate fit overview output / residuals and fitted values -------
                                            
                                        
                                            res.comp.fit.out<-rbind(
                                                        tibble(
                                                        sample=rep(samples[i],length(tmp$dilution.inverse)),
                                                        antigen=rep(antigens[g],length(tmp$dilution.inverse)),
                                                        dilution.inverse=tmp$dilution.inverse,
                                                        MFI_normalized_mean=tmp$MFI_normalized_mean,
                                                        fitted.values=if(!is.na(fit.mfi.resp)){
                                                              coef(fit.mfi)[1] * tmp$dilution.inverse / (coef(fit.mfi)[2] + tmp$dilution.inverse)
                                                              }else{
                                                                rep(NA,length(tmp$dilution.inverse))
                                                              },
                                                        fit.type=rep("nls",length(tmp$dilution.inverse))
                                                        ),
                                                        tibble(
                                                        sample=rep(samples[i],length(tmp$dilution.inverse)),
                                                        antigen=rep(antigens[g],length(tmp$dilution.inverse)),
                                                        dilution.inverse=tmp$dilution.inverse,
                                                        MFI_normalized_mean=tmp$MFI_normalized_mean,
                                                        fitted.values=if(!is.na(fit.mfi.excl.resp)){
                                                          c(coef(fit.mfi.excl)[1] * tmp$dilution.inverse / (coef(fit.mfi.excl)[2] + tmp$dilution.inverse))
                                                            }else{
                                                              rep(NA,length(tmp$dilution.inverse))
                                                            },
                                                        fit.type=rep("nls_with_exclusion",length(tmp$dilution.inverse))
                                                        ),

                                                      
                                                       tibble(
                                                         sample=rep(samples[i],length(tmp$dilution.inverse)),
                                                         antigen=rep(antigens[g],length(tmp$dilution.inverse)),
                                                         dilution.inverse=tmp$dilution.inverse,
                                                         MFI_normalized_mean=tmp$MFI_normalized_mean,
                                                         fitted.values=if(!is.na(fit.lin.dil.50)){
                                                           c(coef(fit.lin.mfi)[2]* tmp$dilution.inverse + coef(fit.lin.mfi)[1])
                                                         }else{
                                                           rep(NA,length(tmp$dilution.inverse))
                                                         },
                                                         fit.type=rep("linear_fit",length(tmp$dilution.inverse))
                                                       )
                                            )
                                           
                                            res.comp.fit.out.final<-rbind(res.comp.fit.out.final,res.comp.fit.out) 
                                          
                                        #calculated values comparison tibble
                                            calc.comp.out<-tibble(sample=rep(samples[i],3),
                                                                  antigen=rep(antigens[g],3),
                                                                  calculated.dilution = c(fit.mfi.dil.50,fit.mfi.excl.dil.50,fit.lin.dil.50),
                                                                  calculated.MFI = c(fit.mfi.MFImax.50,fit.mfi.excl.MFImax.50,fit.lin.MFImax),
                                                                  response = c(fit.mfi.resp,fit.mfi.excl.resp,fit.lin.resp),
                                                                  fit.type = c("nls","nls_with_exclusion","linear_fit")
                                                          )
                                            
                                            
                                            calc.comp.out.final<-rbind(calc.comp.out.final,calc.comp.out)
                                            
                                            
                                            
                                        ## extract data from fit =========
                                        fitted.values<-c()

                                        if(fit.type=="nls"){
                                          fitted.values<-c(coef(fit.mfi)[1] * tmp$dilution.inverse / (coef(fit.mfi)[2] + tmp$dilution.inverse))
                                          residual.values<-fitted.values-tmp$MFI_normalized_mean
                                          used.dilutions<-tmp$used.dilutions
                                          fit.summary<-summary(fit.mfi)
                                          qc.value<-fit.summary$sigma
                                          out.response.df[which(out.response.df[,sample.column]==samples[i] & out.response.df$antigen==antigens[g] & out.response.df$fit.type==fit.type),"response"]<-(1/fit.mfi.dil.50)*fit.mfi.MFImax.50
                                          out.selected.prediction.df[which(out.selected.prediction.df[,sample.column]==samples[i] & out.selected.prediction.df$antigen==antigens[g]),"MFI.predicted"]<-pred.nls.noe$MFI.predicted
                                          out.selected.prediction.df[which(out.selected.prediction.df[,sample.column]==samples[i] & out.selected.prediction.df$antigen==antigens[g]),"fit.type"]<-rep(fit.type,n.fitted.points)
                                        }
                                        
                                        if(fit.type=="nls_with_exclusion"){
                                          fitted.values<-c(coef(fit.mfi.excl)[1] * tmp$dilution.inverse / (coef(fit.mfi.excl)[2] + tmp$dilution.inverse))
                                          residual.values<-fitted.values-tmp$MFI_normalized_mean
                                          used.dilutions<-tmp$used.dilutions
                                          used.dilutions[which(max(tmp$dilution.inverse)==tmp$dilution.inverse)]<-FALSE
                                          fit.summary<-summary(fit.mfi.excl)
                                          qc.value<-fit.summary$sigma
                                          out.response.df[which(out.response.df[,sample.column]==samples[i] & out.response.df$antigen==antigens[g] & out.response.df$fit.type==fit.type),"response"]<-(1/fit.mfi.excl.dil.50)*fit.mfi.excl.MFImax.50
                                          out.selected.prediction.df[which(out.selected.prediction.df[,sample.column]==samples[i] & out.selected.prediction.df$antigen==antigens[g]),"MFI.predicted"]<-pred.nls.exc$MFI.predicted
                                          out.selected.prediction.df[which(out.selected.prediction.df[,sample.column]==samples[i] & out.selected.prediction.df$antigen==antigens[g]),"fit.type"]<-rep(fit.type,n.fitted.points)
                                        }
                                        
                                        if(fit.type=="linear_fit"){
                                          fitted.values<-c(coef(fit.lin.mfi)[2]* tmp$dilution.inverse + coef(fit.lin.mfi)[1])
                                          residual.values<-fitted.values-tmp$MFI_normalized_mean
                                          used.dilutions<-tmp$used.dilutions
                                          fit.summary<-summary(fit.lin.mfi)
                                          qc.value<-fit.summary$sigma
                                          out.response.df[which(out.response.df[,sample.column]==samples[i] & out.response.df$antigen==antigens[g] & out.response.df$fit.type==fit.type),"response"]<-(1/fit.lin.dil.50)*fit.lin.MFImax
                                          out.selected.prediction.df[which(out.selected.prediction.df[,sample.column]==samples[i] & out.selected.prediction.df$antigen==antigens[g]),"MFI.predicted"]<-pred.lin$MFI.predicted
                                          out.selected.prediction.df[which(out.selected.prediction.df[,sample.column]==samples[i] & out.selected.prediction.df$antigen==antigens[g]),"fit.type"]<-rep(fit.type,n.fitted.points)
                                        }
                                        
                                        tmp.fit.model<-c();tmp.summary<-c();tmp.qc<-c();mfi.na<-c()
                                        
                                        if(fit.type!="rejected" &
                                           fit.type!="fit_failed"){
                                          
                                          #summary and qc data frame
                                          
                                          if(fit.type=="nls" |
                                             fit.type=="nls_with_exclusion"){
                                            tmp.summary<-as.data.frame(fit.summary$parameters)
                                            tmp.summary$coefficients<-c("MFImax","dil.50")
                                            tmp.summary[,sample.column]<-rep(samples[i],2)
                                            tmp.summary$antigen<-rep(antigens[g],2)
                                            tmp.summary$fit.type<-rep(fit.type,2)
                                            if(fit.type=="nls"){
                                              tmp.qc<-c(qc.value,fit.mfi.dil.50,fit.mfi.MFImax.50,(1/fit.mfi.dil.50)*fit.mfi.MFImax.50,fit.type,samples[i],antigens[g])
                                            }
                                            if(fit.type=="nls_with_exclusion"){
                                              tmp.qc<-c(qc.value,fit.mfi.excl.dil.50,fit.mfi.excl.MFImax.50,(1/fit.mfi.excl.dil.50)*fit.mfi.excl.MFImax.50,fit.type,samples[i],antigens[g])
                                            }
                                          }
                                          
                                          if(fit.type=="linear_fit"){
                                            tmp.summary<-as.data.frame(fit.summary$coefficients)
                                            tmp.summary$coefficients<-c("interception","slope")
                                            tmp.summary[,sample.column]<-rep(samples[i],2)
                                            tmp.summary$antigen<-rep(antigens[g],2)
                                            tmp.summary$fit.type<-rep(fit.type,2)
                                            tmp.qc<-c(qc.value,fit.lin.dil.50,fit.lin.MFImax,(1/fit.lin.dil.50)*fit.lin.MFImax,fit.type,samples[i],antigens[g])
                                          }
                                          out.fit.summary.df<-rbind(out.fit.summary.df,tmp.summary)
                                          out.qc<-rbind(out.qc,tmp.qc)
                                          
                                          #single fit data frame
                                          tmp.fit.model<-tmp
                                          tmp.fit.model$fitted_values<-as.numeric(fitted.values)
                                          tmp.fit.model$residuals<-residual.values
                                          tmp.fit.model$fit_type<-rep(fit.type,nrow(tmp.fit.model))
                                          tmp.fit.model$used.dilutions<-tmp$used.dilutions
                                          out.fit.model.df<-rbind(out.fit.model.df,tmp.fit.model)
                                        }
                                        
                                        if(fit.type=="rejected" |
                                           fit.type=="fit_failed"){
                                          tmp.fit.model<-tmp
                                          tmp.fit.model$fitted_values<-rep(NA,length(tmp$dilution.inverse))
                                          tmp.fit.model$residuals<-rep(NA,length(tmp$dilution.inverse))
                                          tmp.fit.model$fit_type<-rep(fit.type,nrow(tmp.fit.model))
                                          tmp.fit.model$used.dilutions<-tmp$used.dilutions
                                          out.fit.model.df<-rbind(out.fit.model.df,tmp.fit.model)
                                          out.fit.summary.df<-rbind(out.fit.summary.df,c(rep(NA,5),samples[i],antigens[g],fit.type))
                                          out.qc<-rbind(out.qc,c(rep(NA,4),fit.type,samples[i],antigens[g]))
                                        }
                                        
                                        
                                        
                                      }#sample loop end

                     }# antigen loop end
              
 

 updateProgressBar(session = session,id = "main",value = 7, total = 7,title = paste("Step 7/7: save output report..."))
 
                    #final formatting of df
 
                    res.comp.fit.out.final$residual.values<-unlist(res.comp.fit.out.final$fitted.values)-unlist(res.comp.fit.out.final$MFI_normalized_mean) #add residuals

                    model.params.df<-out.fit.model.df
                      model.params.df$fit_type<-factor( model.params.df$fit_type,levels = c("nls","nls_with_exclusion","linear_fit","rejected","fit_failed"))
                    
                    predictions.df<-as_tibble(out.selected.prediction.df)
                      predictions.df$fit.type<-factor(predictions.df$fit.type,levels = c("nls","nls_with_exclusion","linear_fit","rejected","fit_failed"))
                    
                    pred.comp.df<-as_tibble(out.predicted.df)
                    
                    qc.df<-as_tibble(out.qc[2:nrow(out.qc),])
                        qc.df$fit.type<-factor(qc.df$fit.type,levels = c("nls","nls_with_exclusion","linear_fit","rejected","fit_failed"))
                        qc.df$sigma<-as.numeric(qc.df$sigma);qc.df$calc.dil<-as.numeric(qc.df$calc.dil);qc.df$calc.MFI<-as.numeric(qc.df$calc.MFI);qc.df$response<-as.numeric(qc.df$response)
                    
                    response.df<-as_tibble(out.response.df)
                      response.df$fit.type<-factor(response.df$fit.type,levels = c("nls","nls_with_exclusion","linear_fit","rejected","fit_failed"))
                    
                    resp.comp.df<-as_tibble(out.res.comp)
                    
                    rownames(out.fit.summary.df)<-seq(0,nrow(out.fit.summary.df)-1)
                    summary.df<-as_tibble(out.fit.summary.df[2:nrow(out.fit.summary.df),])
                        summary.df$fit.type<-factor(summary.df$fit.type,levels = c("nls","nls_with_exclusion","linear_fit","rejected","fit_failed"))
                        summary.df$Estimate<-as.numeric(summary.df$Estimate);summary.df$`Std. Error`<-as.numeric(summary.df$`Std. Error`);summary.df$`t value`<-as.numeric(summary.df$`t value`);summary.df$`Pr(>|t|)`<-as.numeric(summary.df$`Pr(>|t|)`)
                        
                    #add meta data to files
                        #force sample column as character
                        meta[,sample.column]<-as.character(unlist(meta[,sample.column]))
                        predictions.df[,sample.column]<-as.character(unlist(predictions.df[,sample.column]))
                        pred.comp.df[,sample.column]<-as.character(unlist(pred.comp.df[,sample.column]))
                        qc.df[,sample.column]<-as.character(unlist(qc.df[,sample.column]))
                        response.df[,sample.column]<-as.character(unlist(response.df[,sample.column]))
                        resp.comp.df[,sample.column]<-as.character(unlist(resp.comp.df[,sample.column]))
                        summary.df[,sample.column]<-as.character(unlist(summary.df[,sample.column]))
                        am.tidy[,sample.column]<-as.character(unlist(am.tidy[,sample.column]))
                        am.tidy.aggregated[,sample.column]<-as.character(unlist(am.tidy.aggregated[,sample.column]))
                        
                      #merge meta and generated dataframes
                    model.params.df<-left_join(model.params.df,meta,by=sample.column)
                    predictions.df<-left_join(predictions.df,meta,by=sample.column)
                    pred.comp.df<-left_join(pred.comp.df,meta,by=sample.column)
                    qc.df<-left_join(qc.df,meta,by=sample.column)
                    response.df<-left_join(response.df,meta,by=sample.column)
                    resp.comp.df<-left_join(resp.comp.df,meta,by=sample.column)
                    summary.df<-left_join(summary.df,meta,by=sample.column)
                    am.tidy<-left_join(am.tidy,meta,by=sample.column)
                    am.tidy.aggregated<-left_join(am.tidy.aggregated,meta,by=sample.column)
                    
                    ## pipeline: generate output ===============
                    pipeline_output<-list(parameter=param.out,
                                          am = am  ,      # assay file df input
                                          am.count = am.count,  # assaycount file df input
                                          ccm = ccm,    # coupling control file df input
                                          ccm.count = ccm.count,    # coupling control file df input
                                          meta = meta,   # meta file df input
                                          model.params = model.params.df,
                                          predictions = predictions.df,
                                          pred.comp = pred.comp.df,
                                          qc.sigma = qc.df,
                                          calculated.variables.comp = calc.comp.out.final,
                                          residual.comp = res.comp.fit.out.final,
                                          response = response.df,
                                          response_wide = response.df%>%select(response,antigen,UQ(as.name(input$sample_naming)))%>%spread(UQ(as.name(input$sample_naming)),response),
                                          resp.comp = resp.comp.df,
                                          summary = summary.df,
                                          am.tidy = am.tidy,
                                          am.tidy.aggregated = am.tidy.aggregated
                                          
                    )
          }else{  #end pipeline: do not execute if validation error == FALSE 
            #render pipe_line_output if error occurs
            ## pipeline: write parameter vector for output / no errors  ==================================
            param.out<-list(
              assay.input.file = if(is.null(input$AssayDataFile)){"missing/example file used"}else{input$AssayDataFile$name},
              assay.bead.count.input.file = if(is.null(input$AssayDataBeadCountFile)){"missing/example file used"}else{input$AssayDataBeadCountFile$name},
              control.input.file = if(is.null(input$controlCouplingDataFile)){"missing/example file used"}else{input$controlCouplingDataFile$name},
              control.bead.count.input.file = if(is.null(input$controlCouplingDataBeadCountFile)){"missing/example file used"}else{input$controlCouplingDataBeadCountFile$name},
              meta.input.file = if(is.null(input$SampleMetaDataFile)){"missing/example file used"}else{input$SampleMetaDataFile$name},
              sample.column.name = sample.column,
              dilution.column.name = am.dilution.column,
              replicate.column.name = am.replicate.column,
              anti_tag.entry = anti_tag.entry,
              blank.entry = blank.entry,
              control.blank.entry = control.blank.entry,
              cv.cut_off = cv.var,
              min.r.squared.for.linearFit=linear.r.squared.min,
              n.fitted.points = n.fitted.points,
              project.name = input$project_name
            )
            
            
            pipeline_output<-list(parameter=param.out)
          } 

      #return output list      
      return(pipeline_output)
            print(elapsed.time-Sys.time())#calculation time print
}) # end of Pipeline ====================
    #_________________________________________ ====================
          #response.calculation<-function()return(pipeline_output)
          
          
      #pipeline: renderUI for downloadButton only appears when Pipeline was successfully executed + Progress Indicators =========
          
          #render UI for selection if single plots should be included in report folder or not
          output$download_pipeline_analysis_plot_selection <- renderUI({
            if(length(which(names(response.calculation())=="response"))==1) {
              tagList(
                awesomeRadio(inputId = "regression.plots.pipeline",label =  "Do single regression plots output (PDF) ?",
                             choices =c(YES=TRUE,NO=FALSE),
                             selected=FALSE),
                h6("this option will increase the calculation time, due to plot rendering.")
              )
            }
          })
        
          #render UI for download button of report folder download
          output$download_pipeline_analysis <- renderUI({
            if(length(which(names(response.calculation())=="response"))==1) {
              if(!is.null(input$regression.plots.pipeline)){
                if(input$regression.plots.pipeline==FALSE){
                  tagList(
                    downloadButton(outputId = "download_pipeline_output",
                                   label = "Download Pipeline Output Report",
                                   icon=icon("download"),style="color:#fffff; background-color: #00A2FF; border-color: #0066FF"),
                    bsPopover(id = "download_pipeline_output", 
                              title = NULL, 
                              content ="standard pipeline output with overview plots, tables and project RDS file, which is needed for the data visualization module of xMAPr", 
                              placement = "right", 
                              trigger = "hover",
                              options = NULL),
                    helpText("Note : Use Ctrl+Click (Windows) / option+Click (Mac) to open the download in background")
                  )
                }else{
                  tagList(
                    downloadButton(outputId = "download_pipeline_output",
                                   label = "Download Pipeline Output Report (SLOW!!!)",
                                   icon=icon("download"),style="color:#fffff; background-color: #FB6821; border-color: #FB4321"),
                    bsPopover(id = "download_pipeline_output", 
                              title = NULL, 
                              content ="complex pipeline output with overview plots and sinple regression analysis plots (!!! ATTENTION - this might take very long since each antigen sample combination will be rendered !!!), tables and project RDS file, which is needed for the data visualization module of xMAPr", 
                              placement = "right", 
                              trigger = "hover",
                              options = NULL),
                    helpText("Note : Use Ctrl+Click (Windows) / option+Click (Mac) to open the download in background")
                  )
                }
              }
            }
          })
          
          #renderUI: progressbar single plot download
          output$report_plot_download_progressbar <- renderUI({
            if(length(which(names(response.calculation())=="response"))==1){
              if(!is.null(input$regression.plots.pipeline)){
                if(input$regression.plots.pipeline!=FALSE){
                  tagList(
                    shinyWidgets::progressBar(id = "report_download_progress_plots_antigens", value = 0,status = "danger",title = "Plot generation (antigens):",display_pct = TRUE),
                    shinyWidgets::progressBar(id = "report_download_progress_plots_samples", value = 0,status = "danger",title = "Plot generation (samples):",display_pct = TRUE)
                  )
                }
              }
            }
          })
          
          #renderUI: progressbar report downlaod
          output$report_download_progressbar <- renderUI({
            if(length(which(names(response.calculation())=="response"))==1) {
              shinyWidgets::progressBar(id = "report_download_progress", value = 0,status = "danger",title = "Report generation:",display_pct = TRUE)
            }
          })
       
          
          
          
          
          
          
        #pipeline: zip folder output from pipeline =========
          #response.calculation<-function()readRDS(file.path("data/2018_05_18__xMAPr_Analysis_output__Baltimore_signal_drop_exclusion/Baltimore_signal_drop_exclusion.RDS"))
          
          output$download_pipeline_output <- downloadHandler(
            filename =paste(format(Sys.Date(),"%Y_%m_%d"),"__xMAPr_Analysis_output__",input$project_name,".zip",sep=""),
            content = function(fname) {
                        #replace double backslash / slash
                        tmpdir <-  tempdir()
                        
                        setwd(tmpdir)
                        print(tmpdir)
                        #if  plotting is selected
                        
                        #workaround if only one replicate is used
                        cv.cut.use<-as.logical(as.data.frame(response.calculation()$parameter$use.CV.cutoff))
                        
                        if(input$regression.plots.pipeline==FALSE){
                          
                          shinyalert(
                            title = "generating download ...",
                            text = "Please be patient this might take a while",
                            closeOnEsc = TRUE,
                            closeOnClickOutside = FALSE,
                            html = FALSE,
                            type = "info",
                            showConfirmButton = TRUE,
                            showCancelButton = FALSE,
                            confirmButtonText = "OK",
                            confirmButtonCol = "#AEDEF4",
                            timer = 0,
                            imageUrl = "logo_medium.png",
                            imageWidth = 100,
                            imageHeight = 100,
                            animation = TRUE
                          )
                          
                          #
                        #generate files
                        #
                          updateProgressBar(session = session,id = "report_download_progress",value = 1, total = 6,title = "generate project file")
                        saveRDS(object = response.calculation(), file = paste(input$project_name,".RDS",sep=""))
                          
                          updateProgressBar(session = session,id = "report_download_progress",value = 2, total = 6,title = "response df file")
                        write_delim(x = response.calculation()$response, file = paste(input$project_name,"_calculated_response_data_frame_tidy",".txt",sep=""),delim = "\t")
                          
                          updateProgressBar(session = session,id = "report_download_progress",value = 3, total = 6,title = "response df wide format file")
                        write_delim(x = response.calculation()$response_wide, file = paste(input$project_name,"_calculated_response_data_frame_wideFormat",".txt",sep=""),delim = "\t")
                        
                        updateProgressBar(session = session,id = "report_download_progress",value = 4, total = 6,title = "generate project overview plots")

                        #general overview
                              #colors for fit types:
                              fit.type.colors<-c(nls="#0D47A1",nls_with_exclusion="#2196F3",`5P_log`="#BF360C",`5P_log_with_exclusion`="#FF5722",linear_fit="#FFD600",rejected="#795548",fit_failed="#607D8B")
                              
                        #bead count
                        
                        tmp.fit.type.table<-as.data.frame(table(response.calculation()$response$fit.type))
                            colnames(tmp.fit.type.table)<-c("fit.type","count")
                            tmp.fit.type.table$fit.type<-factor(tmp.fit.type.table$fit.type,levels=c("nls","nls_with_exclusion","linear_fit","rejected","fit_failed"))
                            #histogram fitting distribution over all samples
                        gg.fitdist<-ggplot(data = tmp.fit.type.table,aes(x=fit.type,y=count,fill=fit.type))+
                          geom_bar(stat = "identity")+
                          scale_fill_manual(values = fit.type.colors)+
                          labs(title="fit type count over dataset",x="",y="count")+
                          theme_bw(base_size = 10)+
                          theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5))
                        
                        #response table
                        tmp.response<-response.calculation()$response%>%select(response,antigen,UQ(as.name(input$sample_naming)),fit.type)
                            tmp.response<-dplyr::arrange(tmp.response,desc(response))
                            tmp.response$rank<-seq(1,nrow(tmp.response))
                            
                        #dynamic range plot
                        gg.dyn.range<-ggplot(data = tmp.response,aes(x=rank,y = log10(response),color=log10(response)))+
                          geom_point(stat = "identity")+
                          theme_bw(base_size = 10)+
                          scale_colour_distiller(type = "seq",palette = "Spectral")+
                          labs(title="dynamic range of response",x="ranked order",y=expression(log[10]~"response"),color=expression(log[10]~"response"))
                        
                        #dynamic range plot histogram count fit.types
                        gg.dyn.range.hist<-ggplot(filter(tmp.response,fit.type!="rejected" & fit.type!="fit_failed"), aes(x=log10(response), fill=fit.type, color=NA,group=fit.type)) +
                          theme_bw(base_size = 10)+
                          geom_histogram(bins=50,position = "stack")+
                          coord_flip()+
                          labs(title="fit types",x=expression(log[10]~"response"),y="count",fill="fit type")+
                          scale_fill_manual(values=fit.type.colors)+
                          scale_color_manual(values=fit.type.colors)
                        
                        #dynamic range plot per dilution
                            
                        tmp.am.tidy.agg<-response.calculation()$am.tidy.agg
                              tmp.am.tidy.agg<-dplyr::arrange(tmp.am.tidy.agg,desc(MFI_normalized_mean))
                              tmp.am.tidy.agg$rank<-seq(1,nrow(tmp.am.tidy.agg))
                              colnames(tmp.am.tidy.agg)[which(colnames(tmp.am.tidy.agg)==input$dilution_naming)]<-"dilution"
                              
                  
                            
                        gg.dyn.range.dilu<- ggplot(data = tmp.am.tidy.agg,aes(x=rank,y = log10(MFI_normalized_mean),color=log10(MFI_normalized_mean)))+
                          geom_point(stat = "identity")+
                          theme_bw(base_size = 10)+
                          facet_wrap(~dilution)+
                          scale_colour_distiller(type = "seq",palette = "Spectral")+
                          labs(title="dynamic range of response",subtitle="dilution-wise",x="ranked order",y=expression(log[10]~"MFI"),color=expression(log[10]~"MFI"))
                        
                        if(cv.cut.use){
                          gg.mfi.cv<-ggplot(tmp.am.tidy.agg,aes(MFI_normalized_cv))+geom_histogram(bins=100)+
                            scale_x_log10()+
                            theme_bw(base_size = 10)+
                            geom_vline(xintercept = input$cv_var_cutoff,linetype="dashed",color="tomato")+
                            labs(x="CV of MFI",title="assay data - coefficient of variation",
                                 subtitle=paste(sum(tmp.am.tidy.agg$MFI_normalized_cv>input$cv_var_cutoff,na.rm=T), "from", sum(!is.na(tmp.am.tidy.agg$MFI_normalized_mean)),"values removed"),
                                 caption=paste("red dashes line = CV cutoff of",input$cv_var_cutoff))
                          
                        }
                        
                        gg.mfi.bead<-ggplot(response.calculation()$am.tidy,aes(BeadCount))+geom_histogram(binwidth=1)+
                          theme_bw(base_size = 10)+
                          geom_vline(xintercept = 35,linetype="dashed",color="tomato")+
                          labs(x="bead count",title="assay data - bead count",
                               subtitle=paste(sum(response.calculation()$am.tidy$BeadCount<35,na.rm=T), "from", length(response.calculation()$am.tidy$BeadCount),"values removed"),
                               caption=paste("red dashes line = bead count cutoff of 35"))  
                        
                        
                        updateProgressBar(session = session,id = "report_download_progress",value = 5, total = 6,title = "save project overview plots")
                        
                        #save plots
                        ggsave(filename = paste(input$project_name,"_used_fit_histogram",".pdf",sep=""),plot = gg.dyn.range.hist,device = "pdf",width = 5,height = 5)
                        ggsave(filename = paste(input$project_name,"_bead_count_histogram",".pdf",sep=""),plot = gg.mfi.bead,device = "pdf",width = 5,height = 5)
                        if(cv.cut.use){ggsave(filename = paste(input$project_name,"_CV_histogram",".pdf",sep=""),plot = gg.mfi.cv,device = "pdf",width = 5,height = 5)}
                        ggsave(filename = paste(input$project_name,"_dynamic_range_over_dilutions",".pdf",sep=""),plot = gg.dyn.range,device = "pdf",width = 5,height = 5)
                        ggsave(filename = paste(input$project_name,"_dynamic_range_dilution_wise",".pdf",sep=""),plot = gg.dyn.range.dilu,device = "pdf",width = 10,height = 10)
                        ggsave(filename = paste(input$project_name,"_used_fit_count",".pdf",sep=""),plot = gg.fitdist,device = "pdf",width = 5,height = 5)
                        
                        #setup files which should be zipped
                        if(cv.cut.use){ 
                          fs <- c(paste(input$project_name,".RDS",sep=""),
                                paste(input$project_name,"_calculated_response_data_frame_tidy",".txt",sep=""),
                                paste(input$project_name,"_calculated_response_data_frame_wideFormat",".txt",sep=""),
                                paste(input$project_name,"_bead_count_histogram",".pdf",sep=""),
                                paste(input$project_name,"_CV_histogram",".pdf",sep=""),
                                paste(input$project_name,"_dynamic_range_over_dilutions",".pdf",sep=""),
                                paste(input$project_name,"_used_fit_histogram",".pdf",sep=""),
                                paste(input$project_name,"_dynamic_range_dilution_wise",".pdf",sep=""),
                                paste(input$project_name,"_used_fit_count",".pdf",sep="")
                                )
                        }else{
                          fs <- c(paste(input$project_name,".RDS",sep=""),
                                  paste(input$project_name,"_calculated_response_data_frame_tidy",".txt",sep=""),
                                  paste(input$project_name,"_calculated_response_data_frame_wideFormat",".txt",sep=""),
                                  paste(input$project_name,"_bead_count_histogram",".pdf",sep=""),
                                  paste(input$project_name,"_dynamic_range_over_dilutions",".pdf",sep=""),
                                  paste(input$project_name,"_used_fit_histogram",".pdf",sep=""),
                                  paste(input$project_name,"_dynamic_range_dilution_wise",".pdf",sep=""),
                                  paste(input$project_name,"_used_fit_count",".pdf",sep="")
                          )
                        }
                        
                        #print
                        print (fs)
                        
                        updateProgressBar(session = session,id = "report_download_progress",value = 6, total = 6,title = "zip project folder")
                        
                          #zip files
                          zip(zipfile=fname, files=fs)
                        
                        
                          }else{
                          
                          shinyalert(
                            title = "generating downloads with plots ...",
                            text = "Please be patient this might take longer due to plot rendering",
                            closeOnEsc = TRUE,
                            closeOnClickOutside = FALSE,
                            html = FALSE,
                            type = "info",
                            showConfirmButton = TRUE,
                            showCancelButton = FALSE,
                            confirmButtonText = "OK",
                            confirmButtonCol = "#AEDEF4",
                            timer = 0,
                            imageUrl = "logo_medium.png",
                            imageWidth = 100,
                            imageHeight = 100,
                            animation = TRUE
                          )
                          
                               #
                          #generate files
                          #
                          updateProgressBar(session = session,id = "report_download_progress",value = 1, total = 7,title = "generate project file")
                          saveRDS(object = response.calculation(), file = paste(input$project_name,".RDS",sep=""))
                          
                          updateProgressBar(session = session,id = "report_download_progress",value = 2, total = 7,title = "response df file")
                          write_delim(x = response.calculation()$response, file = paste(input$project_name,"_calculated_response_data_frame_tidy",".txt",sep=""),delim = "\t")
                          
                          updateProgressBar(session = session,id = "report_download_progress",value = 3, total = 7,title = "response df wide format file")
                          write_delim(x = response.calculation()$response_wide, file = paste(input$project_name,"_calculated_response_data_frame_wideFormat",".txt",sep=""),delim = "\t")
                          
                          updateProgressBar(session = session,id = "report_download_progress",value = 4, total = 7,title = "generate project overview plots")
                          
                          #general overview
                          #colors for fit types:
                          fit.type.colors<-c(nls="#0D47A1",nls_with_exclusion="#2196F3",linear_fit="#FFD600",rejected="#795548",fit_failed="#607D8B")
                          #bead count
                          
                          tmp.fit.type.table<-as.data.frame(table(response.calculation()$response$fit.type))
                          colnames(tmp.fit.type.table)<-c("fit.type","count")
                          tmp.fit.type.table$fit.type<-factor(tmp.fit.type.table$fit.type,levels=c("nls","nls_with_exclusion","linear_fit","rejected","fit_failed"))
                          #histogram fitting distribution over all samples
                          gg.fitdist<-ggplot(data = tmp.fit.type.table,aes(x=fit.type,y=count,fill=fit.type))+
                            geom_bar(stat = "identity")+
                            scale_fill_manual(values = fit.type.colors)+
                            labs(title="fit type count over dataset",x="",y="count")+
                            theme_bw(base_size = 10)+
                            theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5))
                          
                          #response table
                          tmp.response<-response.calculation()$response %>% select(response,antigen,UQ(as.name(input$sample_naming)),fit.type)
                          tmp.response<-dplyr::arrange(tmp.response,desc(response))
                          tmp.response$rank<-seq(1,nrow(tmp.response))
                          
                          #dynamic range plot
                          gg.dyn.range<-ggplot(data = tmp.response,aes(x=rank,y = log10(response),color=log10(response)))+
                            geom_point(stat = "identity")+
                            theme_bw(base_size = 10)+
                            scale_colour_distiller(type = "seq",palette = "Spectral")+
                            labs(title="dynamic range of response",x="ranked order",y=expression(log[10]~"response"),color=expression(log[10]~"response"))
                          
                          #dynamic range plot histogram count fit.types
                          gg.dyn.range.hist<-ggplot(filter(tmp.response,fit.type!="rejected" & fit.type!="fit_failed"), aes(x=log10(response), fill=fit.type, color=NA,group=fit.type)) +
                            theme_bw(base_size = 10)+
                            geom_histogram(bins=50,position = "stack")+
                            coord_flip()+
                            labs(title="fit types",x=expression(log[10]~"response"),y="count",fill="fit type")+
                            scale_fill_manual(values=fit.type.colors)+
                            scale_color_manual(values=fit.type.colors)
                          
                          #dynamic range plot per dilution
                          
                          tmp.am.tidy.agg<-response.calculation()$am.tidy.agg
                          tmp.am.tidy.agg<-dplyr::arrange(tmp.am.tidy.agg,desc(MFI_normalized_mean))
                          tmp.am.tidy.agg$rank<-seq(1,nrow(tmp.am.tidy.agg))
                          colnames(tmp.am.tidy.agg)[which(colnames(tmp.am.tidy.agg)==input$dilution_naming)]<-"dilution"
                          gg.dyn.range.dilu<- ggplot(data = tmp.am.tidy.agg,aes(x=rank,y = log10(MFI_normalized_mean),color=log10(MFI_normalized_mean)))+
                            geom_point(stat = "identity")+
                            theme_bw(base_size = 10)+
                            facet_wrap(~dilution)+
                            scale_colour_distiller(type = "seq",palette = "Spectral")+
                            labs(title="dynamic range of response",subtitle ="dilution-wise",x="ranked order",y=expression(log[10]~"MFI"),color=expression(log[10]~"MFI"))
                          
                          if(cv.cut.use){
                            gg.mfi.cv<-ggplot(tmp.am.tidy.agg,aes(MFI_normalized_cv))+geom_histogram(bins=100)+
                              scale_x_log10()+
                              theme_bw(base_size = 10)+
                              geom_vline(xintercept = input$cv_var_cutoff,linetype="dashed",color="tomato")+
                              labs(x="CV of MFI",title="assay data - coefficient of variation",
                                   subtitle=paste(sum(tmp.am.tidy.agg$MFI_normalized_cv>input$cv_var_cutoff,na.rm=T), "from", sum(!is.na(tmp.am.tidy.agg$MFI_normalized_mean)),"values removed"),
                                   caption=paste("red dashes line = CV cutoff of",input$cv_var_cutoff))
                            
                          }
                              
                          gg.mfi.bead<-ggplot(response.calculation()$am.tidy,aes(BeadCount))+geom_histogram(binwidth=1)+
                            theme_bw(base_size = 10)+
                            geom_vline(xintercept = 35,linetype="dashed",color="tomato")+
                            labs(x="bead count",title="assay data - bead count",
                                 subtitle=paste(sum(response.calculation()$am.tidy$BeadCount<35,na.rm=T), "from", length(tmp.am.tidy.agg$MFI_normalized_mean),"values removed"),
                                 caption=paste("red dashes line = bead count cutoff of 35"))  
                          
                          
                          updateProgressBar(session = session,id = "report_download_progress",value = 5, total = 7,title = "save project overview plots")
                          
                          #save project overviewplots
                          ggsave(filename = paste(input$project_name,"_used_fit_histogram",".pdf",sep=""),plot = gg.dyn.range.hist,device = "pdf",width = 5,height = 5)
                          ggsave(filename = paste(input$project_name,"_bead_count_histogram",".pdf",sep=""),plot = gg.mfi.bead,device = "pdf",width = 5,height = 5)
                          if(cv.cut.use){ggsave(filename = paste(input$project_name,"_CV_histogram",".pdf",sep=""),plot = gg.mfi.cv,device = "pdf",width = 5,height = 5)}
                          ggsave(filename = paste(input$project_name,"_dynamic_range_over_dilutions",".pdf",sep=""),plot = gg.dyn.range,device = "pdf",width = 5,height = 5)
                          ggsave(filename = paste(input$project_name,"_dynamic_range_dilution_wise",".pdf",sep=""),plot = gg.dyn.range.dilu,device = "pdf",width = 10,height = 10)
                          ggsave(filename = paste(input$project_name,"_used_fit_count",".pdf",sep=""),plot = gg.fitdist,device = "pdf",width = 5,height = 5)
                          
                          
                          #render plots for output =======
                          
                          updateProgressBar(session = session,id = "report_download_progress",value = 6, total = 7,title = "generate and save single plots")
                          
                          #pipeline: plot rendring for download ======
                          antigens<-unique(response.calculation()$am.tidy$antigen)
                          samples<-unique(unlist(response.calculation()$am.tidy[,response.calculation()$parameter$sample.column.name]))

                               
                          # Pipeline: export single Antigen|SamplePlots ===============
                          
                          pdf(paste(input$project_name,"_single_plots",".pdf",sep=""),width = 23,height = 7)
                          #
                          #loop over plots
                          #
                          #get column naming from parameter list
                          dil.column.name.param<-response.calculation()$parameter$dilution.column.name
                          sample.column.name.param<-response.calculation()$parameter$sample.column.name
                          antiTag.column.name.param<-response.calculation()$parameter$anti_tag.entry
                          cv.used.param<-response.calculation()$parameter$use.CV.cutoff
                          cv.cutoff<-response.calculation()$parameter$cv.cut_off
                          
                          
                          for(i in 1:length(antigens)){
                            updateProgressBar(session = session,id = "report_download_progress_plots_antigens",value = i, total = length(antigens),title = paste("antigens:",i,"of",length(antigens)))
                            for(g in 1:length(samples)){
                              updateProgressBar(session = session,id = "report_download_progress_plots_samples",value = g, total = length(samples),title = paste("samples:",g,"of",length(samples)))
                              
                              print(paste("Pipeline: Plot output |  sample:",g," // antigen:",i))
                           
                              
                              #get reponse comparison data
                              tmp.resp.comp<-c();tmp.resp.comp <- filter(response.calculation()$resp.comp[,1:6],antigen==antigens[i] &
                                                                           UQ(as.name(sample.column.name.param))==samples[g]) %>%
                                                                          gather(colnames(response.calculation()$resp.comp)[3:6],key = "fit.type",value = "response")
                              tmp.resp.comp$fit.type<-factor(tmp.resp.comp$fit.type,levels = c("nls","nls_with_exclusion","linear_fit"))
                              
                              #filter data
                              tmp.am.tidy <- c()
                              tmp.am.tidy <- filter(response.calculation()$am.tidy, antigen == antigens[i] &
                                                      UQ(as.name(sample.column.name.param)) == samples[g])
                              tmp.am.tidy$MFI_3foldSD_blank_filtered_logical <- factor(tmp.am.tidy$MFI_3foldSD_blank_filtered_logical, levels = c(FALSE, TRUE))
                              tmp.am.tidy$assayMFI_BeadCount_filtered_logical <- factor(tmp.am.tidy$assayMFI_BeadCount_filtered_logical, levels = c(FALSE, TRUE))

                              tmp.am.tidy.agg<-c(); tmp.am.tidy.agg <- filter(response.calculation()$am.tidy.aggregated,antigen==antigens[i] &
                                                                                UQ(as.name(sample.column.name.param))==samples[g])
                              
                              #calculate CV data
                              #use size as plot element
                              tmp.am.tidy.agg.raw<-c();tmp.am.tidy.agg.raw <- tmp.am.tidy %>% 
                                                                              group_by(UQ(as.name(sample.column.name.param)),
                                                                                       UQ(as.name(dil.column.name.param)),
                                                                                       antigen)%>%
                                                                              summarise(MFI_normalized_mean=mean(MFI_normalized,na.rm = T),
                                                                                        MFI_normalized_sd=sd(MFI_normalized,na.rm = T),
                                                                                        MFI_normalized_cv=sd(MFI_normalized,na.rm = T)/mean(MFI_normalized,na.rm = T)) %>%
                                                                              ungroup()
                              tmp.am.tidy.agg.raw$MFI_normalized_cv_filtered<-FALSE
                              tmp.am.tidy.agg.raw$MFI_normalized_cv_filtered[which(cv.cutoff<tmp.am.tidy.agg.raw$MFI_normalized_cv)]<-TRUE
                              
                              #rank exclusion
                              tmp.am.tidy.agg.raw$dilution.inverse<-1/unlist(tmp.am.tidy.agg.raw[,dil.column.name.param])
                              
                              #merge tibbles
                              tmp.am.tidy<-left_join(tmp.am.tidy,
                                                     tmp.am.tidy.agg[,c(sample.column.name.param,dil.column.name.param,"antigen","dilution.inverse","signal_drop")],
                                                     by = colnames(tmp.am.tidy)[colnames(tmp.am.tidy)%in%c(sample.column.name.param,dil.column.name.param,"antigen")])
                              
                                  
                              tmp.qc.sigma<-filter(response.calculation()$qc.sigma,antigen==antigens[i] & UQ(as.name(sample.column.name.param))==samples[g])
                              
                              tmp.residual.comp<-filter(response.calculation()$residual.comp,antigen==antigens[i] & sample==samples[g])
                              
                              tmp.calculated.variables.comp<-filter(response.calculation()$calculated.variables.comp,antigen==antigens[i] & sample==samples[g])
                              tmp.calculated.variables.comp$fit.type<-factor(tmp.calculated.variables.comp$fit.type,levels = c("nls","nls_with_exclusion","linear_fit"))
                              
                              tmp.fit.type<-c();tmp.fit.type<-as.character(tmp.qc.sigma$fit.type)
                              tmp.resp.comp <- tmp.resp.comp %>% mutate( used.fit = ifelse( fit.type == tmp.fit.type, TRUE, FALSE ) )
                              
                              
                              if(sum(!is.na(tmp.resp.comp$response))==0 | is.na(tmp.qc.sigma$response)){
                                response.plot<-ggplot(tmp.resp.comp)+
                                  geom_blank()+
                                  theme_bw(base_size = 12)+
                                  scale_fill_manual(values = fit.type.colors)+
                                  labs(title="response comparison",y=expression(log[2]~"response"),x="",fill="fit type")+
                                  theme(axis.text.x = element_text(angle=90,hjust = 1,vjust = 0.5),legend.position='none')
                                
                              }else{
                                response.plot<-ggplot(tmp.resp.comp)+
                                  geom_bar(aes(fit.type,log2(response),fill=fit.type),stat="identity",size=2)+
                                  theme_bw(base_size = 12)+
                                  geom_point(aes(fit.type,log2(response),color=used.fit),size=2)+
                                  scale_color_manual(values = c(NA,"black"))+
                                  scale_fill_manual(values = fit.type.colors)+
                                  labs(title="response comparison",y=expression(log[2]~"response"),x="",fill="fit type")+
                                  theme(axis.text.x = element_text(angle=90,hjust = 1,vjust = 0.5),legend.position='none')
                              }

                              #raw plot data plot  
                              if(sum(!is.na(tmp.am.tidy$MFI_raw))==0){
                                raw.plot<-ggplot(tmp.am.tidy,aes(x = 1/UQ(as.name(dil.column.name.param)),y = MFI_raw,color=MFI_3foldSD_blank_filtered_logical,shape=assayMFI_BeadCount_filtered_logical))+
                                  geom_blank()+
                                  theme_bw(base_size = 12)+
                                  theme(legend.position='bottom',legend.text=element_text(size=8))+
                                  labs(color="3xSD from BLANK filtered",
                                       shape="Bead count to low",
                                       x="dilution",
                                       title="raw data plot",
                                       subtitle="filtering is indicated",
                                       caption=paste("mean coupling control:",as.character(unlist(tmp.am.tidy[,antiTag.column.name.param])[1])))+
                                  guides(col = guide_legend(nrow = 2))
                                
                              }else{
                                raw.plot<-ggplot(tmp.am.tidy,aes(x = 1/UQ(as.name(dil.column.name.param)),
                                                                 y = MFI_raw,
                                                                 color=MFI_3foldSD_blank_filtered_logical,
                                                                 shape=assayMFI_BeadCount_filtered_logical)
                                )+
                                  geom_point(alpha=0.5,size=3)+
                                  scale_color_manual(values=c("seagreen","tomato"),drop=FALSE)+
                                  scale_shape_manual(values=c(19,17),drop=FALSE)+
                                  theme_bw(base_size = 12)+
                                  geom_hline(yintercept = tmp.am.tidy$Blank_cutoff[1],linetype="dashed",color="tomato")+
                                  theme(legend.position="bottom",legend.spacing.y = unit(0,"mm"),legend.margin = margin(1, 1, 1, 1),legend.box = "vertical",legend.direction = "horizontal",legend.text=element_text(size=9),legend.title=element_text(size=9))
                                
                                if(cv.used.param & sum(tmp.am.tidy.agg.raw$MFI_normalized_cv_filtered)>0 & sum(!is.na(tmp.am.tidy.agg$MFI_normalized_cv))>0){
                                  raw.plot<-raw.plot+labs(color="BLANK filtered",
                                                          shape="Bead count to low",
                                                          x="dilution",
                                                          size="CV filtered",
                                                          title="raw data plot",
                                                          y="raw MFI",
                                                          subtitle="filtering is indicated",
                                                          caption=paste("mean coupling control:",as.character(unlist(tmp.am.tidy[,antiTag.column.name.param])[1])))+
                                    geom_point(data = filter(tmp.am.tidy,MFI_normalized_cv_filtered==TRUE),
                                               mapping = aes(x=1/UQ(as.name(dil.column.name.param)),
                                                             y=MFI,size=MFI_normalized_cv_filtered),
                                               alpha=0.5,shape="X",color="tomato",inherit.aes = F)+
                                    scale_size_manual(values = c(8,9))
                                }else{
                                  raw.plot<-raw.plot+labs(color="BLANK filtered",
                                                          shape="Bead count to low (<35beads)",
                                                          x="dilution",
                                                          y="raw MFI",
                                                          title="raw data plot",
                                                          subtitle="filtering is indicated",
                                                          caption=paste("mean coupling control:",as.character(unlist(tmp.am.tidy[,antiTag.column.name.param])[1])))
                                }
                                
                                
                                ###add ranked exclusion
                                if(sum(tmp.am.tidy$signal_drop,na.rm=T)!=0){
                                  raw.plot<-raw.plot+
                                    geom_point(data = filter(tmp.am.tidy,signal_drop==TRUE),
                                               mapping = aes(x=1/UQ(as.name(dil.column.name.param)),
                                                             y=MFI,fill=signal_drop),
                                               alpha=0.5,shape="X",color="dodgerblue",size=10,inherit.aes = F)+
                                    scale_size_manual(values = c(8,9))
                                }
                                
                              }  
                              
                              
                              
                              #single sample/antigen fit plots

                              if(sum(!is.na(tmp.am.tidy.agg$MFI_normalized_mean))==0 | is.na(tmp.qc.sigma$response)){
                                fit.overview.plot<-ggplot(tmp.am.tidy.agg,aes(x = 1/UQ(as.name(dil.column.name.param)),y = MFI_normalized_mean))+
                                  theme_bw(base_size = 12)+
                                  geom_blank()+
                                  scale_color_manual(values = fit.type.colors)+
                                  theme(legend.position='none',legend.text=element_text(size=8))+
                                  labs(color="fit type",x="dilution",y="norm. MFI",title="curve fitting",subtitle=paste(tmp.qc.sigma$fit.type,"used"))
                              }else{
                                #nls is used
                                if(tmp.qc.sigma$fit.type=="nls" | tmp.qc.sigma$fit.type=="nls_with_exclusion"){
                                  
                                  if(tmp.qc.sigma$calc.dil>max(tmp.am.tidy.agg$dilution.inverse)){
                                    new.data<-2^seq(log2(min(tmp.am.tidy.agg$dilution.inverse)),log2(tmp.qc.sigma$calc.dil+tmp.qc.sigma$calc.dil*0.1),length.out = 50)
                                  }else{
                                    new.data<-2^seq(log2(min(tmp.am.tidy.agg$dilution.inverse)),log2(max(tmp.am.tidy.agg$dilution.inverse)),length.out = 50)
                                    
                                  }
                                  
                                  predicted.data<-data.frame(dilution.input=new.data,
                                                             MFI.predicted=tmp.qc.sigma$calc.MFI*2 * new.data / (tmp.qc.sigma$calc.dil + new.data),
                                                             fit.type=rep(tmp.qc.sigma$fit.type,length(new.data)))
                                  
                                  fit.overview.plot<-ggplot(tmp.am.tidy.agg,aes(x = dilution.inverse,y = MFI_normalized_mean))+
                                    theme_bw(base_size = 12)+
                                    geom_vline(xintercept = max(tmp.am.tidy.agg$dilution.inverse),linetype="dashed",color="hotpink",alpha=0.5)+
                                    geom_vline(xintercept = min(tmp.am.tidy.agg$dilution.inverse),linetype="dashed",color="hotpink",alpha=0.5)+
                                    geom_point(shape=16,alpha=0.5,size=3)+ #add raw data points
                                    geom_line(data = predicted.data,aes(dilution.input,MFI.predicted,color=fit.type),size=2,alpha=0.5)+
                                    scale_color_manual(values = fit.type.colors)+
                                    geom_errorbar(aes(ymin=MFI_normalized_mean-MFI_normalized_sd, ymax=MFI_normalized_mean+MFI_normalized_sd), colour="black",width=0.0001)+
                                    geom_point(data = tmp.qc.sigma,aes(calc.dil,calc.MFI,color=fit.type),shape="X",size=14)+ #add raw data points
                                    geom_label_repel(data = tmp.qc.sigma,aes(x = calc.dil,
                                                                             y = calc.MFI,
                                                                             label=paste("MFImax/2:",round(calc.MFI,digits = 2),"\nxfold-Dil.:",round(1/calc.dil,digits = 2))),
                                                     label.size = NA, 
                                                     alpha = 0.75,
                                                     max.iter=10000,
                                                     size=4,
                                                     fontface = 'plain', color = 'black',
                                                     nudge_y = 0.05,
                                                     direction = "x",
                                                     na.rm=TRUE)+
                                    theme(legend.position='none',legend.text=element_text(size=8))+
                                    labs(color="fit type",x="dilution",y="norm. MFI",title="curve fitting",subtitle=paste(tmp.qc.sigma$fit.type,"used"))
                                  
                                }
                                #linear is used
                                if(tmp.qc.sigma$fit.type=="linear_fit"){
                                  fit.overview.plot<-ggplot(tmp.am.tidy.agg,aes(x = dilution.inverse,y = MFI_normalized_mean))+
                                    theme_bw(base_size = 12)+
                                    geom_smooth(method = "lm",formula=y~x-1,se = F,color="#FFD600",size=2,alpha=0.5)+
                                    geom_point(shape=16,alpha=0.5,size=3)+ #add raw data points
                                    geom_errorbar(aes(ymin=MFI_normalized_mean-MFI_normalized_sd, ymax=MFI_normalized_mean+MFI_normalized_sd), colour="black",width=0.0001)+
                                    geom_point(data = tmp.qc.sigma,aes(calc.dil,calc.MFI), color= "#FFD600",shape="X",size=14)+ #add raw data points
                                    geom_label_repel(data = tmp.qc.sigma,aes(x = calc.dil,
                                                                             y = calc.MFI,
                                                                             label=paste("MFImax/2:",round(calc.MFI,digits = 2),"\nxfold-Dil.:",round(1/calc.dil,digits = 2))),
                                                     label.size = NA, 
                                                     alpha = 0.75,
                                                     max.iter=10000,
                                                     size=4,
                                                     fontface = 'plain', color = 'black',
                                                     nudge_y = 0.05,
                                                     direction = "x",
                                                     na.rm=TRUE)+
                                    theme(legend.position='none',legend.text=element_text(size=8))+
                                    labs(color="fit type",x="dilution",y="norm. MFI",title="curve fitting",subtitle=paste(tmp.qc.sigma$fit.type,"used"))
                                  
                                }
                                
                              }
                              
                              
                              #residual plot

                              if(sum(!is.na(tmp.residual.comp$residual.values))==0){
                                res.plot<-ggplot(tmp.residual.comp,aes(dilution.inverse,residual.values,color=fit.type))+
                                  geom_blank()+
                                  scale_color_manual(values = fit.type.colors)+
                                  theme_bw(base_size = 12)+
                                  theme(legend.position='bottom')+
                                  labs(x="dilution",color="fit type",title="residual plot of used fit",subtitle="all residuals = NA")+
                                  guides(color=FALSE)
                                #+
                                #guides(col = guide_legend(nrow = 3))
                                
                              }else{
                                res.plot<-tmp.residual.comp%>%filter(fit.type==unlist(tmp.qc.sigma$fit.type))%>%
                                  ggplot(aes(dilution.inverse,residual.values,color=fit.type))+
                                  geom_vline(xintercept = 1/min(tmp.am.tidy.agg[,dil.column.name.param],na.rm=T),color="hotpink",linetype="dashed",alpha=0.5)+
                                  geom_vline(xintercept = 1/max(tmp.am.tidy.agg[,dil.column.name.param],na.rm=T),color="hotpink",linetype="dashed",alpha=0.5)+        
                                  geom_hline(yintercept = 0)+
                                  geom_point(alpha=0.5,size=4)+
                                  scale_color_manual(values = fit.type.colors)+
                                  theme_bw(base_size = 12)+
                                  theme(legend.position='bottom')+
                                  labs(x="dilution",color="fit type",title="residual plot of used fit",subtitle=paste(tmp.qc.sigma$fit.type,"used"))+
                                  guides(color=FALSE)
                                #+
                                #guides(col = guide_legend(nrow = 1))
                                
                                
                              }
                              

                              
                              
                              
                              #response overview plot

                              #render plots for single antigen sample output
                              grid.arrange(
                                arrangeGrob(
                                  raw.plot,
                                  fit.overview.plot,
                                  res.plot,
                                  ncol = 3,
                                  top = textGrob(paste("Antigen:",antigens[i],"/","Sample:",samples[g],"/","fit:",tmp.fit.type,"\n"),
                                                 vjust = 1,
                                                 gp = gpar(fontface = "bold", cex = 2)
                                  ),
                                  layout_matrix = rbind(c(1,2,3))
                                )
                             )

                              }#end loop
                            
                            }#end loop
                          dev.off() #close pdf container
                          
                          
                          #setup files which should be zipped
                          if(cv.cut.use){ 
                            fs <- c(paste(input$project_name,".RDS",sep=""),
                                    paste(input$project_name,"_calculated_response_data_frame_tidy",".txt",sep=""),
                                    paste(input$project_name,"_calculated_response_data_frame_wideFormat",".txt",sep=""),
                                    paste(input$project_name,"_bead_count_histogram",".pdf",sep=""),
                                    paste(input$project_name,"_CV_histogram",".pdf",sep=""),
                                    paste(input$project_name,"_dynamic_range_over_dilutions",".pdf",sep=""),
                                    paste(input$project_name,"_used_fit_histogram",".pdf",sep=""),
                                    paste(input$project_name,"_dynamic_range_dilution_wise",".pdf",sep=""),
                                    paste(input$project_name,"_used_fit_count",".pdf",sep=""),
                                    paste(input$project_name,"_single_plots",".pdf",sep="")
                                    
                            )
                          }else{
                            fs <- c(paste(input$project_name,".RDS",sep=""),
                                    paste(input$project_name,"_calculated_response_data_frame_tidy",".txt",sep=""),
                                    paste(input$project_name,"_calculated_response_data_frame_wideFormat",".txt",sep=""),
                                    paste(input$project_name,"_bead_count_histogram",".pdf",sep=""),
                                    paste(input$project_name,"_dynamic_range_over_dilutions",".pdf",sep=""),
                                    paste(input$project_name,"_used_fit_histogram",".pdf",sep=""),
                                    paste(input$project_name,"_dynamic_range_dilution_wise",".pdf",sep=""),
                                    paste(input$project_name,"_used_fit_count",".pdf",sep=""),
                                    paste(input$project_name,"_single_plots",".pdf",sep="")

                            )
                          }
                          
                          #print
                          print (fs)
                          
                          updateProgressBar(session = session,id = "report_download_progress",value = 7, total = 7,title = "zip project folder")
                          
                          #zip files
                          zip(zipfile=fname, files=fs)
                          }
                        
                        
                      },
            contentType = "application/zip"
          )
          #end downloadHandler
          
          
          
          #pipeline: output of parameters to trigger reactive function and show paramters =====================
          output$pipe_parameters<-renderTable({t(as.data.frame(response.calculation()$parameter))},colnames = F,rownames = T)
                                                  

#_______________________________________ ------                                


#                        
# START of VISUALISATION  ------------              
#                        

       
          #load processed data for visualisation =============
                #processed data
                processed_data <-  eventReactive(input$inputButton_data_visu,{
                    if(is.null(input$processed_data_file)){
                      withProgress(message = 'loading file...',
                                   detail = 'This may take a while...',min = 1,max = 1,
                      proc.datacontainer <- readRDS(file = file.path("data/pool-measurements/16_04_2018__xMAPr_Analysis_output__Pool_data_processing/Pool_data_processing.RDS"))
                      )
                      #NULL #have to be modified for example dataset or error message         
                      } else {
                        withProgress(message = 'loading file...',
                                    detail = 'This may take a while...',min = 1,max = 1,
                        proc.datacontainer <- readRDS(file = input$processed_data_file$datapath)
                        )
                      }
                     return(proc.datacontainer)
                        
                        }) 
          
          #render project name UI output ------
          output$projectNAME<-renderUI({
            if(sum(names(processed_data())=="parameter")==1){
              tags$li(class = "dropdown",
                      tags$div(
                        hr(),
                        h6("loaded project:",align="left",style="margin-left: 10px;"),
                        if(nchar(processed_data()$parameter$project.name)<30){
                          HTML(
                            paste("<b><font color='#FB6720'>",processed_data()$parameter$project.name,"</font></b>",sep="")
                               )
                        }else{
                         HTML(
                            paste("<b><font color='#FB6720'>",
                          substring(processed_data()$parameter$project.name,1,30),
                          "<br/>",
                          substring(processed_data()$parameter$project.name,31,nchar(processed_data()$parameter$project.name)),
                                    "</font></b>",sep="")
                         )
                            
                        }
                        ,
                        
                        hr(),
                        align="center")
              )
            }
            
          })
         
          
          
          
          #render response DT -------     
          output$resptable<-DT::renderDataTable({processed_data()$response},options = list(scrollX = TRUE,selection = 'single',searchHighlight = TRUE))
                  outputOptions(output, "resptable", suspendWhenHidden = FALSE)
          

                        #DT: generate Visualiation interactive selectors -----------------
                            
          
          
                        output$antigen.selection<-renderUI({
                                                              selectInput(inputId = "antigen", label = "select antigen",
                                                                        choices =  unique(processed_data()$summary$antigen),
                                                                        selected=if(is.null(input$resptable_rows_selected)){
                                                                          unique(processed_data()$summary$antigen)[1]
                                                                        }else{
                                                                          processed_data()$response[input$resptable_rows_selected[length(input$resptable_rows_selected)],"antigen"]
                                                                          }
                                                                      )
                          
                                                            })

                  
                  
                        output$antigen.selection.stats<-renderUI({
                                                            selectInput(inputId = "antigen.stats", label = "select antigen",
                                                                        choices =  unique(processed_data()$summary$antigen),
                                                                        selected=if(is.null(input$stats.resptable_rows_selected)){
                                                                          unique(statistics.over.Meta.Sample()$response.stats$antigen)[1]
                                                                        }else{
                                                                          statistics.over.Meta.Sample()$response.stats[unlist(input$stats.resptable_rows_selected)[length(unlist(input$stats.resptable_rows_selected))],"antigen"]
                                                                        }
                                                            )
                                                            
                                                          })
                        
                        
          
                        output$sample.selection<-renderUI({
                                                              selectInput(inputId = "sample", label = "select sample",
                                                                       choices =  unique(unlist(processed_data()$summary[,processed_data()$parameter$sample.column.name])),
                                                                       selected=if(is.null(input$resptable_rows_selected)){
                                                                         unique(unlist(processed_data()$summary[,processed_data()$parameter$sample.column.name]))[1]
                                                                       }else{
                                                                         processed_data()$response[input$resptable_rows_selected[length(input$resptable_rows_selected)],processed_data()$parameter$sample.column.name]
                                                                       }
                                                                      )
                                                          })

                        
                        #selection ui input for coloring
                        output$meta.selection <- renderUI({
                          selectInput(inputId = "meta.sele", label = "Choose category:",
                                      choices = c(colnames(processed_data()$response[,-c(1:4)])),selected = colnames(processed_data()$response[,5])) 
                        })
                        
                          
                  
                        
                        
                        #color for meta data
                        #manual colors
                        general.colors.meta<-c("#0057e7","#d62d20","#008744","#ffa700","#03A9F4","#E91E63","#009688","#4CAF50","#B71C1C","#FF5722","#3F51B5","#795548","#607D8B","#673AB7","#FFC107")
                        #gradient colors
                        general.colors.gradient.meta<-colorRampPalette(c("black","#0057e7","#d62d20","#008744","#ffa700","grey"))
                        
                        
                        
                        
                        #selected coloring ============
                        meta.colors.selected<-reactive({
                          
                          table.in <- as.data.frame(processed_data()$response)
                          #parse meta data
                          meta.dat<-table.in[,-c(1,2,4)]
                          meta.dat<-meta.dat[!duplicated(meta.dat[,processed_data()$parameter$sample.column.name]),]
                          meta.dat[is.na(meta.dat)]<-"NA"
                          meta.dat[meta.dat==""]<-"NA"
                          #meta selection + colorimng
                          meta.select.number.of.elements<-length(unique(unlist(meta.dat[,input$meta.sele])))
                          if(meta.select.number.of.elements>15){
                            used.col<-general.colors.gradient.meta(meta.select.number.of.elements)
                          }else{
                            used.col<-general.colors.meta[1:meta.select.number.of.elements]
                          }
                          return(used.col)
                        })

        #summary of loaded data // response ===========

        output$summary <- renderPrint({
          summary(processed_data()$response)
        })


                        
                        
### general response data overview ==================
    #renderUI for selected antigen sample combination highlight
                    output$highlight.antigen.sample.checkbox<-renderUI({
                                          materialSwitch(inputId = "highlight.antigen.sample",
                                                      label = "highlight selected antigen/sample combination in global plots?",
                                                      value = FALSE
                                                     )
                                          })
                        outputOptions(output, "highlight.antigen.sample.checkbox", suspendWhenHidden = FALSE)
                        
    general.overview.response<-reactive({
                # Create a Progress object
                progress <- shiny::Progress$new()
                # Make sure it closes when we exit this reactive, even if there's an error
                on.exit(progress$close())
                
                progress$set(message = "generate general overview...", value = 0)
      
                progress$inc(1/5, detail = paste("process data"))
                
             table.in <- processed_data()$response
             colnames(table.in)[3]<-"sample"
             
             #log2 transformed data
             table.in$response.log10<-log10(table.in$response)
             #ad ranked order
             table.in<-table.in[order(table.in$response,decreasing = T),]
             table.in$order.log10<-seq(1,nrow(table.in))
             
             #table.in.raw<-data.frame(proc.datacontainer$model.params)
             table.in.raw<-processed_data()$model.params
             colnames(table.in.raw)[1:2]<-c("sample","dilution")
             table.in.raw$log10.MFI.input<-log10(table.in.raw$MFI_normalized_mean) #log10 of MFI
             #add ranked order
             table.in.raw<-table.in.raw[order(table.in.raw$log10.MFI.input,decreasing = T),]
             table.in.raw$order.log10<-seq(1,nrow(table.in.raw))
               
             #get am tidy aggregated
             table.in.agg<-processed_data()$am.tidy.aggregated
             
             #get am tidy 
             table.in.am<-processed_data()$am.tidy
             table.in.am.noise<-table.in.am%>%group_by(antigen)%>%summarise(Blank_cutoff=mean(Blank_cutoff),mean_blank=mean(mean_blank))
             table.in.am.noise$antigen<-factor(table.in.am.noise$antigen,levels = table.in.am.noise$antigen[order(table.in.am.noise$Blank_cutoff)])
             table.in.ccm<-table.in.am%>%distinct(antigen,coupling_control_MFI_blank_substracted)
             table.in.ccm$antigen<-factor(table.in.ccm$antigen,levels=table.in.ccm$antigen[order(table.in.ccm$coupling_control_MFI_blank_substracted)],ordered=F)
             
             progress$inc(1/5, detail = paste("noise plot"))
             
             gg.noise.plot<-ggplotly(ggplot(table.in.am.noise,aes(
                                                                   text = paste('antigen: ', antigen,'<br>Blank+3xSD: ', Blank_cutoff,'<br>Blank: ', mean_blank)))+
                                             geom_point(aes(x = antigen,
                                                            y = mean_blank),color="grey")+
                                             geom_point(aes(x = antigen,
                                                            y = Blank_cutoff,
                                                            color = Blank_cutoff))+
                                             theme_bw(base_size = 10)+
                                             scale_colour_distiller(type = "seq",palette = "Spectral")+
                                             theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5))+
                                             labs(title="Noise: Blank + 3xSD Blank",x="",y="MFI",color="MFI"),
                                           tooltip = "text", width = 1400, height = 400)
             
             
             
             progress$inc(1/5, detail = paste("coupling control plot"))
             #coupling control plot
             gg.coupling.control<-ggplotly(ggplot(table.in.ccm,aes(x = antigen,
                                     y = coupling_control_MFI_blank_substracted,
                                     color = coupling_control_MFI_blank_substracted,
                                     text = paste('antigen: ', antigen,'<br>coupling MFI: ', coupling_control_MFI_blank_substracted)))+
               geom_point()+
               theme_bw(base_size = 10)+
               scale_colour_distiller(type = "seq",palette = "Spectral")+
               theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5))+
               labs(title="coupling control MFI (Blank substracted)",x="",y="MFI",color="MFI"),
               tooltip = "text", width = 1400, height = 400)
                    
             
              
             #input<-list(sample="CF-IgG",antigen="BSAb")
             if(sum(input$highlight.antigen.sample)==1){
               table.in.raw.selected.antigen.sample<-filter(table.in.raw,antigen==input$antigen & sample==input$sample)
               table.in.selected.antigen.sample<-filter(table.in,antigen==input$antigen & sample==input$sample)
             }
            
             
             
             #bead count
             tmp.fit.type.table<-as.data.frame(table(processed_data()$response$fit.type))
                colnames(tmp.fit.type.table)<-c("fit.type","count")
                tmp.fit.type.table$fit.type<-factor(tmp.fit.type.table$fit.type,levels=c("nls","nls_with_exclusion","linear_fit","rejected","fit_failed"))
                
            #histogram fitting distribution over all samples
             progress$inc(1/5, detail = paste("generate distribution plot"))
             gg.fitdist<- ggplotly(
                                  ggplot(data = tmp.fit.type.table,aes(x=fit.type,y=count,fill=fit.type,
                                                                                text = paste('fit type: ', fit.type,'<br>count: ', count)))+
                                   geom_bar(stat = "identity")+
                                   scale_fill_manual(values = fit.type.colors)+
                                   labs(title="fit type count over dataset",x="",y="count",fill="")+
                                   theme_bw(base_size = 10)+
                                   theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5),
                                         plot.margin=margin(t = 0, r = 0, b = 13, l = 10, unit = "pt")),
                                   tooltip = "text", width = 480, height = 380)
             #dynamic range plot
             progress$inc(1/5, detail = paste("generate dynamic range plot"))
             gg.dyn.range.tmp<-ggplot(data = table.in,aes(x=order.log10,y = response.log10,color=response.log10,
                                                        text = paste("rank:",order.log10,"<br>fit type:",fit.type,"<br>sample:",sample,"<br>antigen:",antigen,"<br>response:",response,"<br>log10_reponse:",response.log10)))+
                                 geom_point(stat = "identity")+
                                 theme_bw(base_size = 10)+
                                 scale_colour_distiller(type = "seq",palette = "Spectral")+
                                 labs(title="dynamic range of response",x="ranked order",y="log10-response",color="log10-response")
             
             
           if(sum(input$highlight.antigen.sample)==1){
             gg.dyn.range<-ggplotly(gg.dyn.range.tmp +
                                      geom_point(data = table.in.selected.antigen.sample,aes(x=order.log10,y = response.log10),shape=1,color="black",size=3),
                                    tooltip = "text", width = 480, height = 380)
           }else{
             gg.dyn.range<-ggplotly(gg.dyn.range.tmp,tooltip = "text", width = 480, height = 380)
           }
                                 
                                   
                  
      
      
             #dynamic range plot histogram count fit.types
             progress$inc(1/5, detail = paste("generate fit distribution plot"))
             gg.dyn.range.hist<- ggplotly(
                                      ggplot(filter(table.in,fit.type!="rejected" & fit.type!="fit_failed"),
                                              aes(x=log10(response), fill=fit.type,text=paste("fit type:",fit.type))) +
                                       theme_bw(base_size = 10)+
                                       geom_histogram(bins=50,position = "stack",color=NA)+
                                       coord_flip()+
                                       labs(title="fit types",x="log10-response",y="count",fill="")+
                                       scale_fill_manual(values=fit.type.colors),
                                      tooltip = "text", width = 480, height = 380)
             

             progress$inc(1/5, detail = paste("generate dynamic range plot dilution-wise"))
             gg.dyn.range.dilu.tmp<- ggplot(data = table.in.raw,
                                             aes(x=order.log10,y = log10.MFI.input,color=log10.MFI.input,
                                                 text = paste("rank:",order.log10,"<br>fit type:",fit_type,"<br>sample:",sample,"<br>antigen:",antigen,"<br>norm.MFI:",MFI_normalized_mean,"<br>log10_norm.MFI:",log10.MFI.input))
                                             )+
                                         geom_point(stat = "identity")+
                                         theme_bw(base_size = 16)+
                                         facet_wrap(~dilution)+
                                         scale_colour_distiller(type = "seq",palette = "Spectral")+
                                         labs(title="dynamic range of response",subtitle="dilution-wise",x="ranked order",y="log10-MFI",color="log10-MFI")
                                      
            
             if(sum(input$highlight.antigen.sample)==1){
               gg.dyn.range.dilu<-#ggplotly(
                                            gg.dyn.range.dilu.tmp +
                                             geom_point(data = table.in.raw.selected.antigen.sample,aes(x=order.log10,y = log10.MFI.input),shape=1,color="black",size=3)
                                         #  ,
                                      #tooltip = "text", width = 900, height = 800)
             }else{
               gg.dyn.range.dilu<-#ggplotly(
                 gg.dyn.range.dilu.tmp
               #,tooltip = "text", width = 900, height = 800)
             }
             
             
             
             
             
             if(processed_data()$parameter$use.CV.cutoff){
               gg.mfi.cv<-ggplot(table.in.agg,aes(MFI_normalized_cv))+geom_histogram(bins=100)+
                 scale_x_log10()+
                 theme_bw(base_size = 18)+
                 geom_vline(xintercept = processed_data()$parameter$cv.cut_off,linetype="dashed",color="tomato")+
                 labs(x="CV of MFI",title="assay data - coefficient of variation",
                      subtitle=paste(sum(table.in.agg$MFI_normalized_cv>processed_data()$parameter$cv.cut_off,na.rm=T), "from", sum(!is.na(table.in.agg$MFI_normalized_cv)),"valid CV values removed"),
                      caption=paste("red dashes line = CV cutoff of",processed_data()$parameter$cv.cut_off,"/",nrow(table.in.agg%>%filter(!is.na(MFI_normalized_mean) & is.na(MFI_normalized_cv))),"values have only 1 replicate"))
             }else{
               gg.mfi.cv<-ggplot(table.in.agg,aes(MFI_normalized_cv))+
                 geom_blank()+
                 theme_bw(base_size = 18)+
                 labs(x="CV of MFI",title="replicates were not used !")
               
             }
             
             
             gg.mfi.bead<-ggplot(table.in.am,aes(BeadCount))+geom_histogram(binwidth=1)+
               theme_bw(base_size = 18)+
               geom_vline(xintercept = 35,linetype="dashed",color="tomato")+
               labs(x="bead count",title="assay data - bead count",
                    subtitle=paste(sum(table.in.am$BeadCount<35,na.rm=T), "from", length(table.in.am$BeadCount),"values removed"),
                    caption=paste("red dashes line = bead count cutoff of 35"))  
             
             #barplot of filtered values
             #filtered values on single antigen level
             qual.count.single<-table.in.am%>%group_by(antigen)%>%summarise(coupling_beadCount_low=sum(coupling_control_bead_count_to_low),
                                                                            assay_beadCount_low=sum(assayMFI_BeadCount_filtered_logical),
                                                                            LOD_filtering=sum(MFI_3foldSD_blank_filtered_logical))%>%
                                                                        gather("coupling_beadCount_low","assay_beadCount_low","LOD_filtering",
                                                                               key="filter.stage",value = "number_filtered")
             
             
             levels.stacked.barplot<-filter(qual.count.single,filter.stage==input$filter.order.stacked.barplot)%>%arrange(desc(number_filtered))
             
             qual.count.single$filter.stage<-factor(qual.count.single$filter.stage,levels=c("coupling_beadCount_low","assay_beadCount_low","LOD_filtering"))
             qual.count.single$antigen<-factor(qual.count.single$antigen,levels= levels.stacked.barplot$antigen)
             
             
             gg.filtered<-ggplotly(ggplot(qual.count.single,
                                          aes(x = filter.stage,
                                              y = number_filtered,
                                              fill=antigen,
                                              text=paste("antigen:",antigen,
                                                         "<br>count of filtered values:",number_filtered,
                                                         "<br> percentage:",round(number_filtered/dim(table.in.am)[1]*100,digits = 4),"%",
                                                         "<br> % of single antigen:",round(number_filtered/(dim(table.in.am)[1]/length(unique(table.in.am$antigen)))*100,digits = 4),"%")))+
                     geom_bar(stat="identity",position = "stack",color="black",size=0.2)+
                       scale_y_continuous(name = "percentage compared to all values",labels = function(x) paste0(round(x/dim(table.in.am)[1],digits = 2)*100, "%"))+ # Multiply by 100 & add %+
                       theme_bw(base_size = 16)+
                       scale_x_discrete(drop=FALSE)+
                       theme(plot.margin = margin(t = 20,r = 50,b = 50,l = 10),
                             axis.title.y = element_text(angle = 0,hjust = 0.5,vjust = 0.5,size = 10),
                             axis.text.y = element_text(angle = 0,hjust = 0.5,vjust = 0.5,size = 8),
                             axis.text.x = element_text(angle = 90,hjust = 0.5,vjust = 0.5,size = 8),
                             legend.position='none' #removes legend from ggplot
                             )+
                       labs(title="raw values filtering",x=""),
                     tooltip="text",height = 595,width = 300, autosize=TRUE) %>% layout(margin = list(l = 100))
             
             
             

             #filtered values on fit antigen level
             qual.count.response.single <- table.in%>%
                                           filter(fit.type=="rejected" | fit.type=="fit_failed")%>%
                                           group_by(antigen,fit.type) %>% 
                                           summarise(count=n()) %>% 
                                           arrange(desc(count)) %>% 
                                           ungroup()
             
             
        
             
             qual.count.response.single$antigen <- factor(qual.count.response.single$antigen,
                                                          levels= unique(qual.count.response.single$antigen))
             
    
             
             
             if(dim(qual.count.response.single)[1]==0){ #if no fit failed or rejected
               gg.filtered.fit<-ggplotly(ggplot()+
                                           geom_blank()+
                                           theme_bw(base_size = 16)+
                                           theme(plot.margin = margin(t = 20,r = 50,b = 80,l = 0),
                                                 axis.title.y = element_text(angle = 0,hjust = 0.5,vjust = 0.5,size = 10),
                                                 axis.text.y = element_text(angle = 0,hjust = 0.5,vjust = 0.5,size = 8),
                                                 axis.text.x = element_text(angle = 90,hjust = 0.5,vjust = 0.5,size = 8),
                                                 legend.position='none' #removes legend from ggplot
                                           )+
                                           labs(title="no fit failed/rejected",x="",y="count"),
                                         tooltip="text",height = 550,width = 300, autosize=TRUE) %>% 
                 layout(margin = list(l = 100))
             }else{
               gg.filtered.fit<-ggplotly(ggplot(qual.count.response.single,
                                                aes(x = fit.type,
                                                    y = count,
                                                    fill=antigen,
                                                    text=paste("antigen:",antigen,
                                                               "<br>count of filtered values:",count,
                                                               "<br> percentage:",round(count/length(unique(unlist(table.in[,3])))*100,digits = 4),"%")))+
                                           geom_bar(stat="identity",position = "stack",color="black",size=0.2)+
                                           theme_bw(base_size = 16)+
                                           theme(plot.margin = margin(t = 20,r = 50,b = 80,l = 0),
                                                 axis.title.y = element_text(angle = 0,hjust = 0.5,vjust = 0.5,size = 10),
                                                 axis.text.y = element_text(angle = 0,hjust = 0.5,vjust = 0.5,size = 8),
                                                 axis.text.x = element_text(angle = 90,hjust = 0.5,vjust = 0.5,size = 8),
                                                 legend.position='none' #removes legend from ggplot
                                           )+
                                           labs(title="fit failed or rejected",x="",y="count"),
                                         tooltip="text",height = 550,width = 300, autosize=TRUE) %>% 
                 layout(margin = list(l = 100))
             }
            
             
             
             #filtered signal drop
             qual.count.signaldrop.antigen<-table.in.raw%>%filter(signal_drop==TRUE)%>%filter(!is.na(MFI_normalized_mean))%>%group_by(antigen,dilution)%>%summarise(count=n())
             qual.count.signaldrop.antigen$antigen<-factor(qual.count.signaldrop.antigen$antigen,levels=unique(qual.count.signaldrop.antigen$antigen[order(qual.count.signaldrop.antigen$count,decreasing = T)]))
             qual.count.signaldrop.antigen$dilution<-factor(qual.count.signaldrop.antigen$dilution,levels = sort(unique(table.in.raw$dilution),decreasing = T))
             
             total.dil.count<-table.in.raw%>%group_by(dilution)%>%summarise(sum=n())
             total.dil.count<-unique(total.dil.count$sum)
             
             

             gg.filtered.signal.dropout<-ggplotly(ggplot(qual.count.signaldrop.antigen,
                                              aes(x = dilution,
                                                  y = count,
                                                  fill=antigen,
                                                  text=paste("antigen:",antigen,
                                                             "<br>count of filtered values:",count
                                                             )))+
                                                scale_x_discrete(drop=FALSE)+
                                         geom_bar(stat="identity",position = "stack",color="black",size=0.2)+
                                         theme_bw(base_size = 16)+
                                         scale_y_continuous(name = "percentage compared to all values of dilution",labels = function(y) paste0(round(y/total.dil.count,digits = 5)*100, "%"))+ # Multiply by 100 & add %+
                                         theme(plot.margin = margin(t = 20,r = 10,b = 50,l = 10),
                                               axis.title.y = element_text(angle = 0,hjust = 0.5,vjust = 0.5,size = 10),
                                               axis.text.y = element_text(angle = 0,hjust = 0.5,vjust = 0.5,size = 8),
                                               axis.text.x = element_text(angle = 90,hjust = 0.5,vjust = 0.5,size = 8),
                                               legend.position='none' #removes legend from ggplot
                                         )+
                                         labs(title="signal drop filtering",x="xfold dilution",y="count"),
                                       tooltip="text",height = 560,width = 500, autosize=TRUE) %>% 
               layout(margin = list(l = 100))
             
             
          
             #bead count plot
             gg.mfi.bead<-ggplot(table.in.am,aes(BeadCount))+
               geom_histogram(binwidth=1)+
               theme_bw(base_size = 18)+
               geom_vline(xintercept = 35,linetype="dashed",color="tomato")+
               labs(x="bead count",title="assay data - bead count",
                    subtitle=paste(sum(table.in.am$BeadCount<35,na.rm=T), "from", length(table.in.am$BeadCount),"values removed"),
                    caption=paste("red dashes line = bead count cutoff of 35"))  
             
             
             
             
           # return all object as a list
             return(list(
               gg.filtered=gg.filtered,
               gg.filtered.fit=gg.filtered.fit,
               gg.filtered.signal.dropout=gg.filtered.signal.dropout,
               gg.mfi.cv=gg.mfi.cv,
               gg.mfi.bead=gg.mfi.bead,
               gg.fitdist=gg.fitdist,
               gg.dyn.range=gg.dyn.range,
               gg.dyn.range.dilu=gg.dyn.range.dilu,
               gg.dyn.range.hist=gg.dyn.range.hist,
               gg.coupling.control=gg.coupling.control,
               gg.noise.plot=gg.noise.plot
               
             ))

           })   
                        
          #render Antigen and Sample input selection as text --------
          output$AntigenSampleSelectedText<-renderText({paste("selected sample:", input$sample, "| selected antigen:",input$antigen)})

    
    ### general response data overview heatmaps with heatmaply ==================
    

    general.overview.response.heatmaps<-reactive({
    
      
                    # Create a Progress object
                    progress <- shiny::Progress$new()
                    # Make sure it closes when we exit this reactive, even if there's an error
                    on.exit(progress$close())
                    
                    progress$set(message = "heatmap data processing...", value = 0)
                    
                    progress$inc(1/3, detail = paste("loading data"))
              #reponse data
              tmp.response<-processed_data()$response
              tmp.in<-as.data.frame(spread(data = tmp.response[,c(1:3)],key = antigen,value = response))
              rownames(tmp.in)<-tmp.in[,1]
              tmp.in<-tmp.in[,-1]
              tmp.in.meta<-as.data.frame(unlist(sapply(rownames(tmp.in),function(x) unlist(unique(tmp.response[which(x==tmp.response[,3]),input$meta.sele])))))
              colnames(tmp.in.meta)<-input$meta.sele
              
              #transform to binary na or no na
              tmp.in.na<-apply(tmp.in,2,function(x) as.numeric(is.na(x)))
              rownames(tmp.in.na)<-rownames(tmp.in)
              #order tmp.na
              tmp.in.na<-tmp.in.na[,order(apply(tmp.in.na,2,sum))]
              tmp.in.na<-tmp.in.na[order(apply(tmp.in.na,1,sum)),]
              
              #na values heatmap
              heat.resp.na<-heatmaply(tmp.in.na,
                           grid_gap = 0,
                           colors = c("grey80", "grey20"),
                           fontsize_row = input$heatmap.response.ROW.text.size,
                           fontsize_col = input$heatmap.response.COLUMN.text.size,
                           Rowv = FALSE,
                           Colv = FALSE,
                           column_text_angle = 270,
                           margins=c(100,100,5,0)
                           )%>% 
                layout(height=800,width=1000)
              
              na.values<-apply(tmp.in,2,function(x)sum(is.na(x)))
              to.delete.antigens<-names(na.values[which(na.values==nrow(tmp.in))])
           
              na.values1<-apply(tmp.in,1,function(x)sum(is.na(x)))
              to.delete.samples<-names(na.values[which(na.values==ncol(tmp.in))])
              
              if(length(to.delete.antigens)!=0){
                tmp.in<-tmp.in[,-unlist(sapply(to.delete.antigens,function(x) which(colnames(tmp.in)==x)))]
                
              }
              
              if(length(to.delete.samples)!=0){
                tmp.in<-tmp.in[-unlist(sapply(to.delete.samples,function(x) which(rownames(tmp.in)==x))),]
                
              }
              
      

              progress$inc(1/3, detail = paste("generating heatmaps"))
              
                      heat.resp<-heatmaply(log10(tmp.in),
                                col=colorRampPalette(rev(brewer.pal(11,"Spectral")))(256),
                                k_row = input$heatmap.response.ROW.cluster.count,
                                k_col = input$heatmap.response.COLUMN.cluster.count,
                                plot_method = "plotly",
                                dist_method = input$heatmap.response.dist.method,
                                hclust_method = input$heatmap.response.hclust.method,
                                Rowv = if(input$heatmap.response.Rowv=="YES"){TRUE}else{FALSE},
                                Colv = if(input$heatmap.response.Colv=="YES"){TRUE}else{FALSE},
                                key.title="log10-reponse",
                                row_side_colors = tmp.in.meta,
                                fontsize_row = input$heatmap.response.ROW.text.size,
                                fontsize_col = input$heatmap.response.COLUMN.text.size,
                                column_text_angle = 270,
                                margins=c(100,100,5,0)
                      )%>% 
                        layout(height=800,width=1000)
                      
                    
                      
                      progress$inc(1/3, detail = paste("return results"))
                      
                      return(list(
                        heat.resp.data=tmp.in,
                        heat.resp.meta=tmp.in.meta,
                        heat.resp=heat.resp,
                        to.delete.antigens=to.delete.antigens,
                        to.delete.samples=to.delete.samples,
                        heat.resp.na=heat.resp.na
                        
                      ))
    
    })
    
    general.overview.count.heatmaps<-reactive({
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "heatmap count data processing...", value = 0)
      
      progress$inc(1/3, detail = paste("loading data"))
      
      
      #beadcount data
      tmp.count<-as.data.frame(processed_data()$am.count)
      tmp.count<-tmp.count[-which(tmp.count[,1]==processed_data()$parameter$blank.entry),]
      
      rownames(tmp.count)<-paste(tmp.count[,1],tmp.count[,2],tmp.count[,3],sep = "_")
      tmp.count.row.col<-tmp.count[,c(1:3)]
      tmp.count<-tmp.count[,-c(1:3)]
      
      progress$inc(1/3, detail = paste("generating heatmaps"))
      
      if(dim(tmp.count)[1]>3500){#cap to 3500 rows at max. for displaying
        heat.beadcount<-"to_many_rows"
      }else{
        heat.beadcount<- heatmaply(tmp.count,
                                   col=colorRampPalette(brewer.pal(11,"Spectral"))(256),
                                   plot_method = "plotly",
                                   key.title="bead count",
                                   fontsize_row = 2,
                                   fontsize_col = 5,
                                   margins=c(50,50,NA,0)
        ) %>% 
          layout(height=800,width=1000)
      }
      
    
      
      progress$inc(1/3, detail = paste("return results"))
      
      return(list(
        heat.beadcount=heat.beadcount
      ))
      
    })
    
    
    
    #general.overview.count.heatmaps<-function()list(
    #  heat.beadcount=heat.beadcount
    #)
    
            ### RENDER: general response data overview heatmaps with heatmaply
    
                      output$heatmap_to_delete_antigens<-renderUI({
                        if(length(general.overview.response.heatmaps()$to.delete.antigens)==0){
                          helpText("all antigens are used for the heatmap")
                        }else{
                          helpText(paste(paste(general.overview.response.heatmaps()$to.delete.antigens,collapse = ", "),"was/were removed from data matrix (only NA values)"))
                          
                        }

                      })
                      
                      output$heatmap_to_delete_samples<-renderUI({
                      if(length(general.overview.response.heatmaps()$to.delete.samples)==0){
                        helpText("all samples are used for the heatmap")
                      }else{
                        helpText(paste(paste(general.overview.response.heatmaps()$to.delete.samples,collapse = ", "),"was/were removed from data matrix (only NA values)"))
                        
                      }
                      })
    
                       output$heatmap.response<-renderPlotly({
                           ###heatmap plot
                           general.overview.response.heatmaps()$heat.resp

                        })
                       
                       output$heatmap.response.na<-renderPlotly({
                         ###heatmap plot
                         general.overview.response.heatmaps()$heat.resp.na
                         
                       })
                       
                       
                         
                         output$heatmap.beadcount<-renderPlotly({
                           if(class(general.overview.count.heatmaps()$heat.beadcount)[1]=="character"){
                                     ggplotly(ggplot(data.frame(x=1,y=1),aes(x,y))+
                                              geom_blank()+
                                              geom_text(aes(x,y,label="no heatmap rendered to many rows"),size=12)+
                                              theme_void()
                                     )
                           }else{
                             ###heatmap plot
                             general.overview.count.heatmaps()$heat.beadcount
                           }
                         })
                         
                    
                       
    
                      
                       #download heatmap html
                       output$downloadResponseHeatmap <- downloadHandler(
                         filename =function(){paste(format(Sys.Date(),"%Y_%m_%d_"),input$RawProcessedName,"dist_method",
                                                    input$heatmap.response.dist.method,"hclust_method",
                                                    input$heatmap.response.hclust.method,"response_heatmap.html",sep="")},
                         content = function(file) {
                           tmpdir <- gsub("//", "/", tempdir(), fixed = TRUE)
                           tmpdir <- gsub("\\", "\\\\", tempdir(), fixed = TRUE)
                           
                           setwd(tmpdir)
                           print(tmpdir)
                           
                           
                           heatmaply(log10(general.overview.response.heatmaps()$heat.resp.data),
                                     col=colorRampPalette(rev(brewer.pal(11,"Spectral")))(256),
                                     k_row = input$heatmap.response.ROW.cluster.count,
                                     k_col = input$heatmap.response.COLUMN.cluster.count,
                                     plot_method = "plotly",
                                     dist_method = input$heatmap.response.dist.method,
                                     hclust_method = input$heatmap.response.hclust.method,
                                     Rowv = if(input$heatmap.response.Rowv=="YES"){TRUE}else{FALSE},
                                     Colv = if(input$heatmap.response.Colv=="YES"){TRUE}else{FALSE},
                                     key.title="log10-reponse",
                                     row_side_colors = general.overview.response.heatmaps()$heat.resp.meta,
                                     fontsize_row = input$heatmap.response.ROW.text.size*2,
                                     fontsize_col = input$heatmap.response.COLUMN.text.size*2,
                                     column_text_angle = 270,
                                     margins=c(100,100,5,0),
                                     file = file
                                    )%>% 
                             layout(height=800,width=1000)
                          
                           file
                           
                         })
                       

    
    
    
### general response data overview PCA ==================
general.overview.response.pca<-reactive({
                   
                    # Create a Progress object
                    progress <- shiny::Progress$new()
                    # Make sure it closes when we exit this reactive, even if there's an error
                    on.exit(progress$close())
                    
                    progress$set(message = "performing PCA...", value = 0)
                    
                    progress$inc(1/4, detail = paste("process data"))
                          
                     table.in <- processed_data()$response
                     colnames(table.in)[3]<-"sample"
                    
                     #log2 transformed data
                     table.in$response.log10<-log10(table.in$response)
                     #ad ranked order
                     table.in<-table.in[order(table.in$response,decreasing = T),]
                     table.in$order.log10<-seq(1,nrow(table.in))
                          
                          progress$inc(1/4, detail = paste("PCA"))
                          #do pca analysis of data
                          pca.dat<-table.in %>% select(sample,antigen,response.log10) %>% spread(key = antigen,value = response.log10)
                          pca.dat <- as.data.frame(pca.dat)
                          rownames(pca.dat)<-pca.dat[,1]
                          pca.dat<-pca.dat[,-1]
                          raw.dim = dim(pca.dat)
                          #replace with min. value // missing value imputation!
                          pca.dat.na.omit<-pca.dat
                          pca.dat.na.omit[is.na(pca.dat.na.omit)]<-min(pca.dat.na.omit,na.rm=T)
                          pca.dim = dim(pca.dat.na.omit)
                          pca.facto <- FactoMineR::PCA(pca.dat.na.omit, graph = FALSE,ncp = 10,scale.unit = T)
                          
                          #parse meta data
                          meta.dat<-table.in[,-c(1,2,4)]
                          meta.dat<-meta.dat[!duplicated(meta.dat$sample),]
                          meta.dat[is.na(meta.dat)]<-"NA"
                          meta.dat[meta.dat==""]<-"NA"
                          meta.dat<-meta.dat[unlist(sapply(rownames(pca.dat),function(x) which(x==meta.dat$sample))),-c(ncol(meta.dat)-1,ncol(meta.dat))]
                          #meta.dat$sample==rownames(pca.dat)
                          
                          
                          
  
                          progress$inc(1/4, detail = paste("generating scree plot"))
                          
                          #pca.facto.scree.var,pca.facto.plot
                          pca.facto.scree.var<-#screeplot # eigenvalue
                            fviz_eig(pca.facto, choice = c("variance"),addlabels=TRUE, hjust = -0.3,ncp = 10) +
                            labs(title="Scree plot", subtitle="explained variances",caption=paste(sum(is.na(pca.dat.na.omit)),"values replaced with min. value in dataset (mis. value imputation)"))+
                            theme_bw(base_size = 22)
                          
                          
                          
                          #contribution plot
                          facto.contr.ID.biplot<-fviz_pca_var(pca.facto, select.var = list(contrib = 10),col.var = "contrib",col.circle = "black")+
                            labs(title=paste("TOP", 10), subtitle="contributions to PCA")+
                            theme_bw(base_size = 10)
                          
                          progress$inc(1/4, detail = paste("generating 3D PCA"))
                          
                          #generate 3D plotly version of PCA plot FactomineR :: Eigenvalues = coordinates                   
                          plotly3d.facto<-as.data.frame(pca.facto$ind$coord)

                          plotly3d.facto.plot <- plot_ly(plotly3d.facto, x = ~Dim.1, y = ~Dim.2, z = ~Dim.3,color = ~factor(unlist(meta.dat[,input$meta.sele])),  colors = meta.colors.selected()) %>%
                            add_markers() %>%
                            add_text(text = meta.dat$sample, textposition = "top right",visible = "legendonly") %>%
                            layout(title = "PCA (individuals coordinates) / imputed missing val.",
                                   scene = list(xaxis = list(title = paste("Dim. 1 (", round(pca.facto$eig[1,2]), "%)", sep="")),
                                                yaxis = list(title = paste("Dim. 2 (", round(pca.facto$eig[2,2]), "%)", sep="")),
                                                zaxis = list(title = paste("Dim. 3 (", round(pca.facto$eig[3,2]), "%)", sep=""))))
                          
                          
                         
                          
                          
                          # return all object as a list
                          return(
                            list(
                                plotly3d.facto.plot = plotly3d.facto.plot,
                                facto.contr.ID.biplot = facto.contr.ID.biplot,
                                pca.facto = pca.facto,
                                meta.dat = meta.dat,
                                pca.facto.scree.var = pca.facto.scree.var,
                                raw.dim = raw.dim,
                                pca.dim = pca.dim
                            )
                          )
                          
                        
                  })
                        
    
                        
     
                        #general.overview.response: render plots  ===============

                                    #render UI for PCA dim selection
                                     
                                      output$PCA.knob1<-renderUI(
                                        if(length(which(names(general.overview.response.pca())=="pca.facto"))==1){
                                          knobInput(inputId = "pca.dim1",
                                                    label = "PCA dimension x-axis:",
                                                    value = 1,
                                                    min = 1,
                                                    max = dim(general.overview.response.pca()$pca.facto$ind$coord)[2],
                                                    thickness = 0.3,
                                                    cursor = TRUE,
                                                    width = 150,height = 150
                                          )
                                        }
                                              
                                        )
    
                                    output$PCA.knob2<-renderUI(
                                      if(length(which(names(general.overview.response.pca())=="pca.facto"))==1){
                                              knobInput(inputId = "pca.dim2",
                                                                     label = "PCA dimension y-axis:",
                                                                     value = 2,
                                                                     min = 1,
                                                                     max = dim(general.overview.response.pca()$pca.facto$ind$coord)[2],
                                                                     thickness = 0.3,
                                                                     cursor = TRUE,
                                                                     width = 150,height = 150
                                                )
                                      }
                                       )


                              
                                    
                        #render 3d PCA plotly            
                        output$facto.plot.3d<-renderPlotly({
                          if(length(which(names(general.overview.response.pca())=="pca.facto"))==1){
                            general.overview.response.pca()$plotly3d.facto.plot
                            }
                          })
                        
                        #render 2D PCA
                        output$facto.plot.2d<-renderPlot({ 
                                      if(!is.null(input$pca.dim1)){
                                                  # Create a Progress object
                                                  progress <- shiny::Progress$new()
                                                  # Make sure it closes when we exit this reactive, even if there's an error
                                                  on.exit(progress$close())
                                                  
                                                  progress$set(message = "render PCA plot...", value = 0)
                                                  
                                                  progress$inc(1/1, detail = paste(""))
                                                      #plot PCA eigenvalues // selected Dims (Knobs)
                                                            pca.facto.plot<-fviz_pca_ind(general.overview.response.pca()$pca.facto,
                                                                                         axes = c(as.numeric(input$pca.dim1), as.numeric(input$pca.dim2)),
                                                                                         mean.point=input$show.mean.points,
                                                                                         pointsize=input$plot.point.size,
                                                                                         pointshape=19,
                                                                                         label="none",
                                                                                         ellipse.type = input$ellipses.method,
                                                                                         ellipse.alpha=input$ellipses.alpha,
                                                                                         addEllipses = input$ellipses,
                                                                                         ellipse.level=input$ellipses.level,
                                                                                         habillage = as.factor(unlist(general.overview.response.pca()$meta.dat[,input$meta.sele])))+ 
                                                              scale_color_manual(values = c(meta.colors.selected()))+
                                                              theme_classic(base_size = 18)+
                                                              labs(title=paste("PCA: Dim",input$pca.dim1,"vs",input$pca.dim2),subtitle="individuals coordinates",color=input$meta.sele)+
                                                              guides(fill=F,shape = F)
                                                            
                                                            if(input$shownam) pca.facto.plot<-pca.facto.plot + geom_text_repel(aes(label=rownames(general.overview.response.pca()$pca.facto$ind$coord)), label = rownames(general.overview.response.pca()$pca.facto$ind$coord))
                                                          
                                                            pca.facto.plot
                                                                    }  
                                                            })
                        
                        #PCA 1D selection1
                        output$facto.plot.1D.selection1<-renderPlot({ 
                          if(!is.null(input$pca.dim1)){
                          tmp<-as.data.frame(general.overview.response.pca()$pca.facto$ind$coord)
                          tmp$sample<-rownames(tmp)
                          tmp<-left_join(tmp,general.overview.response.pca()$meta.dat,by="sample")
                          
                          ggplot(data = tmp,mapping = aes(UQ(as.name(colnames(tmp)[input$pca.dim1])),color=UQ(as.name(input$meta.sele)),fill=UQ(as.name(input$meta.sele))))+
                            geom_density(alpha=0.3)+
                            scale_color_manual(values = c(meta.colors.selected()),guide=F)+
                            scale_fill_manual(values = c(meta.colors.selected()))+
                            theme_classic(base_size = 12)+
                            labs(title=paste("PCA: Dim",input$pca.dim1),subtitle="individuals coordinates",color="group")
                          }
                        })
                        
                        
                        #PCA 1D selection2
                        output$facto.plot.1D.selection2<-renderPlot({ 
                          if(!is.null(input$pca.dim2)){
                          tmp<-as.data.frame(general.overview.response.pca()$pca.facto$ind$coord)
                          tmp$sample<-rownames(tmp)
                          tmp<-left_join(tmp,general.overview.response.pca()$meta.dat,by="sample")
                          
                          ggplot(data = tmp,mapping = aes(UQ(as.name(colnames(tmp)[input$pca.dim2])),color=UQ(as.name(input$meta.sele)),fill=UQ(as.name(input$meta.sele))))+
                            geom_density(alpha=0.3)+
                            scale_color_manual(values = c(meta.colors.selected()),guide=F)+
                            scale_fill_manual(values = c(meta.colors.selected()))+
                            theme_classic(base_size = 12)+
                            labs(title=paste("PCA: Dim",input$pca.dim2),subtitle="individuals coordinates",color="group")
                          }
                        })
                        
                      
                        output$facto.plot.scree<-renderPlot({general.overview.response.pca()$pca.facto.scree.var})
                        output$proc.gg.mfi.cv<-renderPlot({general.overview.response()$gg.mfi.cv})
                        output$proc.gg.mfi.bead<-renderPlot({general.overview.response()$gg.mfi.bead})
                        output$proc_gg_filtered<-renderPlotly({general.overview.response()$gg.filtered})
                        output$proc_gg_filtered.fit<-renderPlotly({general.overview.response()$gg.filtered.fit})
                        output$proc_gg_filtered.signal.dropout<-renderPlotly({general.overview.response()$gg.filtered.signal.dropout})
                        
                        output$proc.coupling.control<-renderPlotly({general.overview.response()$gg.coupling.control})
                        output$proc.noise<-renderPlotly({general.overview.response()$gg.noise.plot})
                        
                        output$proc.response.fitdist<-renderPlotly({general.overview.response()$gg.fitdist})
                        output$proc.response.overview1<-renderPlotly({general.overview.response()$gg.dyn.range})
                        output$proc.response.overview2<-renderPlotly({general.overview.response()$gg.dyn.range.hist})
                        output$proc.response.overview3<-renderPlot({
                                                                  # Create a Progress object
                                                                  progress <- shiny::Progress$new()
                                                                  # Make sure it closes when we exit this reactive, even if there's an error
                                                                  on.exit(progress$close())
                                                                  progress$set(message = "render dynamic range plot dilution-wise...", value = 0)
                                                                  
                                                                  progress$inc(1/1, detail = paste(""))
                                                                  
                                                                  general.overview.response()$gg.dyn.range.dilu
                                                                  
                                                                  
                                                    })
                  
                
                                
                        #general.overview.response: render ValueBoxes ===============
                        output$AnalyteCountbox <- renderValueBox({
                          valueBox(
                            color="teal",
                            paste(length(unique(unlist(processed_data()$summary$antigen)))),
                            "Analyte count",
                            icon = icon("atom"),
                            width = NULL
                          )
                        })
                        output$SampleCountbox <- renderValueBox({
                          valueBox(
                            color="light-blue",
                            paste(length(unique(unlist(processed_data()$summary[,processed_data()$parameter$sample.column.name])))),
                            "Sample count",
                            icon = icon("tint"),
                            width = NULL
                          )
                        })
                       
            
#_________________________________________ ====================

                        # single sample & single antigen output -------------
                    
                        #generate single plots interactive DT selection -----------------
                        #
                        
                plots.out.gg.AntigenSample<-reactive({
                          
                              # Create a Progress object
                              progress <- shiny::Progress$new()
                              # Make sure it closes when we exit this reactive, even if there's an error
                              on.exit(progress$close())
                              
                              progress$set(message = "generate single sample/antigen inspection...", value = 0)
                              
                              progress$inc(1/5, detail = paste("process data"))
                  
                          #get column naming from parameter list
                          dil.column.name.param<-processed_data()$parameter$dilution.column.name
                          sample.column.name.param<-processed_data()$parameter$sample.column.name
                          antiTag.column.name.param<-processed_data()$parameter$anti_tag.entry
                          cv.used.param<-processed_data()$parameter$use.CV.cutoff
                          cv.cutoff<-processed_data()$parameter$cv.cut_off
                          
                          
                          #get reponse comparison data
                          tmp.resp.comp<-filter(processed_data()$resp.comp[,1:5],antigen==input$antigen & UQ(as.name(sample.column.name.param))==input$sample)%>%gather(colnames(processed_data()$resp.comp)[3:5],key = "fit.type",value = "response")
                                  tmp.resp.comp$fit.type<-factor(tmp.resp.comp$fit.type,levels = c("nls","nls_with_exclusion","linear_fit"))
                                  
                          #filter data
                          tmp.am.tidy<-filter(processed_data()$am.tidy,antigen==input$antigen & UQ(as.name(sample.column.name.param))==input$sample)
                                    tmp.am.tidy$MFI_3foldSD_blank_filtered_logical<-factor(tmp.am.tidy$MFI_3foldSD_blank_filtered_logical,levels=c(FALSE,TRUE))
                                    tmp.am.tidy$assayMFI_BeadCount_filtered_logical<-factor(tmp.am.tidy$assayMFI_BeadCount_filtered_logical,levels=c(FALSE,TRUE))
                                    
                                    
                                    
                                    
                          tmp.am.tidy.agg<-filter(processed_data()$am.tidy.aggregated,antigen==input$antigen & UQ(as.name(sample.column.name.param))==input$sample)
                                    
                                #calculate CV data
                                  #use size as plot element
                                  tmp.am.tidy.agg.raw <- tmp.am.tidy%>%group_by(UQ(as.name(sample.column.name.param)),UQ(as.name(dil.column.name.param)),antigen)%>%
                                    summarise(MFI_normalized_mean=mean(MFI_normalized,na.rm = T),
                                              MFI_normalized_sd=sd(MFI_normalized,na.rm = T),
                                              MFI_normalized_cv=sd(MFI_normalized,na.rm = T)/mean(MFI_normalized,na.rm = T))%>%
                                    ungroup()
                                  tmp.am.tidy.agg.raw$MFI_normalized_cv_filtered<-FALSE
                                  tmp.am.tidy.agg.raw$MFI_normalized_cv_filtered[which(cv.cutoff<tmp.am.tidy.agg.raw$MFI_normalized_cv)]<-TRUE
                                  
                                  #rank exclusion
                                  tmp.am.tidy.agg.raw$dilution.inverse<-1/unlist(tmp.am.tidy.agg.raw[,dil.column.name.param])
                                  
                                  
                                  tmp.am.tidy<-left_join(tmp.am.tidy,tmp.am.tidy.agg.raw[,c(1,2,3,7)],by = colnames(tmp.am.tidy)[c(1,3,4)])
                                  tmp.am.tidy<-left_join(tmp.am.tidy,tmp.am.tidy.agg[,c(1,2,3,8)],by = colnames(tmp.am.tidy)[c(1,3,4)])
                                  
                                
                                    
                          tmp.qc.sigma<-filter(processed_data()$qc.sigma,antigen==input$antigen & UQ(as.name(sample.column.name.param))==input$sample)
                          
                          tmp.residual.comp<-filter(processed_data()$residual.comp,antigen==input$antigen & sample==input$sample)

                          tmp.calculated.variables.comp<-filter(processed_data()$calculated.variables.comp,antigen==input$antigen & sample==input$sample)
                                  tmp.calculated.variables.comp$fit.type<-factor(tmp.calculated.variables.comp$fit.type,levels = c("nls","nls_with_exclusion","linear_fit"))
                                    
                            
                          #fittype color for box
                          tmp.fit.type<-unique(as.character(unlist(tmp.qc.sigma$fit.type)))
                          #valid colors for box
                                #red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
                          fit.type.box.color<-c(nls="blue",nls_with_exclusion="light-blue",linear_fit="yellow",rejected="maroon",fit_failed="black")
                          tmp.fit.type.box.color<-fit.type.box.color[which(tmp.fit.type==names(fit.type.box.color))]

                          progress$inc(1/5, detail = paste("generate response plot plot"))
                          
                          if(sum(!is.na(tmp.resp.comp$response))==0 | is.na(tmp.qc.sigma$response)){
                            response.plot<-ggplot(tmp.resp.comp)+
                              geom_blank()+
                              theme_bw(base_size = 12)+
                              scale_fill_manual(values = fit.type.colors)+
                              labs(title="response comparison",y=expression(log[2]~"response"),x="",fill="fit type")+
                              theme(axis.text.x = element_text(angle=90,hjust = 1,vjust = 0.5),legend.position='none')
                            
                          }else{
                            response.plot<-ggplot(tmp.resp.comp)+
                              geom_bar(aes(fit.type,log2(response),fill=fit.type),stat="identity")+
                              theme_bw(base_size = 12)+
                              scale_fill_manual(values = fit.type.colors)+
                              labs(title="response comparison",y=expression(log[2]~"response"),x="",fill="fit type")+
                              theme(axis.text.x = element_text(angle=90,hjust = 1,vjust = 0.5),legend.position='none')
                          }
                          
                            
                          
                          
                          progress$inc(1/5, detail = paste("generate raw data plot"))
                          
                          #raw plot data plot  
                          if(sum(!is.na(tmp.am.tidy$MFI_raw))==0){
                            raw.plot<-ggplot(tmp.am.tidy,aes(x = 1/UQ(as.name(dil.column.name.param)),y = MFI_raw,color=MFI_3foldSD_blank_filtered_logical,shape=assayMFI_BeadCount_filtered_logical))+
                              geom_blank()+
                              theme_bw(base_size = 12)+
                              theme(legend.position='bottom',legend.text=element_text(size=8))+
                              labs(color="3xSD from BLANK filtered",
                                   shape="Bead count to low",
                                   x="dilution",
                                   title="raw data plot",
                                   subtitle="filtering is indicated",
                                   caption=paste("mean coupling control:",as.character(unlist(tmp.am.tidy[,antiTag.column.name.param])[1])))+
                              guides(col = guide_legend(nrow = 2))
                            
                          }else{
                            raw.plot<-ggplot(tmp.am.tidy,aes(x = 1/UQ(as.name(dil.column.name.param)),
                                                             y = MFI_raw,
                                                             color=MFI_3foldSD_blank_filtered_logical,
                                                             shape=assayMFI_BeadCount_filtered_logical)
                                             )+
                              geom_point(alpha=0.5,size=3)+
                              scale_color_manual(values=c("seagreen","tomato"),drop=FALSE)+
                              scale_shape_manual(values=c(19,17),drop=FALSE)+
                              theme_bw(base_size = 12)+
                              geom_hline(yintercept = tmp.am.tidy$Blank_cutoff[1],linetype="dashed",color="tomato")+
                              theme(legend.position="bottom",legend.spacing.y = unit(0,"mm"),legend.margin = margin(1, 1, 1, 1),legend.box = "vertical",legend.direction = "horizontal",legend.text=element_text(size=9),legend.title=element_text(size=9))
                            
                          if(cv.used.param & sum(tmp.am.tidy.agg.raw$MFI_normalized_cv_filtered)>0 & sum(!is.na(tmp.am.tidy.agg$MFI_normalized_cv))>0){
                              raw.plot<-raw.plot+labs(color="BLANK filtered",
                                   shape="Bead count to low",
                                   x="dilution",
                                   size="CV filtered",
                                   title="raw data plot",
                                   y="raw MFI",
                                   subtitle="filtering is indicated",
                                   caption=paste("mean coupling control:",as.character(unlist(tmp.am.tidy[,antiTag.column.name.param])[1])))+
                                geom_point(data = filter(tmp.am.tidy,MFI_normalized_cv_filtered==TRUE),
                                           mapping = aes(x=1/UQ(as.name(dil.column.name.param)),
                                                         y=MFI,size=MFI_normalized_cv_filtered),
                                           alpha=0.5,shape="X",color="tomato",inherit.aes = F)+
                                scale_size_manual(values = c(8,9))
                            }else{
                              raw.plot<-raw.plot+labs(color="BLANK filtered",
                                                      shape="Bead count to low (<35beads)",
                                                      x="dilution",
                                                      y="raw MFI",
                                                      title="raw data plot",
                                                      subtitle="filtering is indicated",
                                                      caption=paste("mean coupling control:",as.character(unlist(tmp.am.tidy[,antiTag.column.name.param])[1])))
                            }
                             
                            
                            ###add ranked exclusion
                            if(sum(tmp.am.tidy$signal_drop,na.rm=T)!=0){
                              raw.plot<-raw.plot+
                                geom_point(data = filter(tmp.am.tidy,signal_drop==TRUE),
                                           mapping = aes(x=1/UQ(as.name(dil.column.name.param)),
                                                         y=MFI,fill=signal_drop),
                                           alpha=0.5,shape="X",color="dodgerblue",size=10,inherit.aes = F)+
                                scale_size_manual(values = c(8,9))
                            }
                              
                          }  
                          
                         
                          
                          #single sample/antigen fit plots
                          progress$inc(1/5, detail = paste("generate single sample/antigen fit plots"))
                          
                          if(sum(!is.na(tmp.am.tidy.agg$MFI_normalized_mean))==0 | is.na(tmp.qc.sigma$response)){
                            fit.overview.plot<-ggplot(tmp.am.tidy.agg,aes(x = 1/UQ(as.name(dil.column.name.param)),y = MFI_normalized_mean))+
                              theme_bw(base_size = 12)+
                              geom_blank()+
                              scale_color_manual(values = fit.type.colors)+
                              theme(legend.position='none',legend.text=element_text(size=8))+
                              labs(color="fit type",x="dilution",y="norm. MFI",title="curve fitting",subtitle=paste(tmp.qc.sigma$fit.type,"used"))
                          }else{
                            #nls is used
                            if(tmp.qc.sigma$fit.type=="nls" | tmp.qc.sigma$fit.type=="nls_with_exclusion"){
                              
                              if(tmp.qc.sigma$calc.dil>max(tmp.am.tidy.agg$dilution.inverse)){
                                new.data<-2^seq(log2(min(tmp.am.tidy.agg$dilution.inverse)),log2(tmp.qc.sigma$calc.dil+tmp.qc.sigma$calc.dil*0.1),length.out = 50)
                              }else{
                                new.data<-2^seq(log2(min(tmp.am.tidy.agg$dilution.inverse)),log2(max(tmp.am.tidy.agg$dilution.inverse)),length.out = 50)
                                
                              }
                              
                              predicted.data<-data.frame(dilution.input=new.data,
                                                         MFI.predicted=tmp.qc.sigma$calc.MFI*2 * new.data / (tmp.qc.sigma$calc.dil + new.data),
                                                         fit.type=rep(tmp.qc.sigma$fit.type,length(new.data)))
                              
                              fit.overview.plot<-ggplot(tmp.am.tidy.agg,aes(x = dilution.inverse,y = MFI_normalized_mean))+
                                theme_bw(base_size = 12)+
                                geom_vline(xintercept = max(tmp.am.tidy.agg$dilution.inverse),linetype="dashed",color="hotpink",alpha=0.5)+
                                geom_vline(xintercept = min(tmp.am.tidy.agg$dilution.inverse),linetype="dashed",color="hotpink",alpha=0.5)+
                                geom_point(shape=16,alpha=0.5,size=3)+ #add raw data points
                                geom_line(data = predicted.data,aes(dilution.input,MFI.predicted,color=fit.type),size=2,alpha=0.5)+
                                scale_color_manual(values = fit.type.colors)+
                                geom_errorbar(aes(ymin=MFI_normalized_mean-MFI_normalized_sd, ymax=MFI_normalized_mean+MFI_normalized_sd), colour="black",width=0.0001)+
                                geom_point(data = tmp.qc.sigma,aes(calc.dil,calc.MFI,color=fit.type),shape="X",size=14)+ #add raw data points
                                geom_label_repel(data = tmp.qc.sigma,aes(x = calc.dil,
                                                                         y = calc.MFI,
                                                                         label=paste("MFImax/2:",round(calc.MFI,digits = 2),"\nxfold-Dil.:",round(1/calc.dil,digits = 2))),
                                                                         label.size = NA, 
                                                                         alpha = 0.75,
                                                                         max.iter=10000,
                                                                         size=4,
                                                                         fontface = 'plain', color = 'black',
                                                                         nudge_y = 0.05,
                                                                         direction = "x",
                                                                         na.rm=TRUE)+
                                theme(legend.position='none',legend.text=element_text(size=8))+
                                labs(color="fit type",x="dilution",y="norm. MFI",title="curve fitting",subtitle=paste(tmp.qc.sigma$fit.type,"used"))
 
                            }
                            #linear is used
                            if(tmp.qc.sigma$fit.type=="linear_fit"){
                              
                              #get linear fit for antigen selection
                              tmp.lin.pred<-filter(processed_data()$predictions,antigen==input$antigen & UQ(as.name(sample.column.name.param))==input$sample)
                              
                              fit.overview.plot<-ggplot(tmp.am.tidy.agg,aes(x = dilution.inverse,y = MFI_normalized_mean))+
                                theme_bw(base_size = 12)+
                                geom_line(data = tmp.lin.pred,aes(dilution.input,MFI.predicted),color="#FFD600",size=2,alpha=0.5)+
                                geom_point(shape=16,alpha=0.5,size=3)+ #add raw data points
                                geom_errorbar(aes(ymin=MFI_normalized_mean-MFI_normalized_sd, ymax=MFI_normalized_mean+MFI_normalized_sd), colour="black",width=0.0001)+
                                geom_point(data = tmp.qc.sigma,aes(calc.dil,calc.MFI), color= "#FFD600",shape="X",size=14)+ #add raw data points
                                geom_label_repel(data = tmp.qc.sigma,aes(x = calc.dil,
                                                                         y = calc.MFI,
                                                                         label=paste("MFImax/2:",round(calc.MFI,digits = 2),"\nxfold-Dil.:",round(1/calc.dil,digits = 2))),
                                                 label.size = NA, 
                                                 alpha = 0.75,
                                                 max.iter=10000,
                                                 size=4,
                                                 fontface = 'plain', color = 'black',
                                                 nudge_y = 0.05,
                                                 direction = "x",
                                                 na.rm=TRUE)+
                                theme(legend.position='none',legend.text=element_text(size=8))+
                                labs(color="fit type",x="dilution",y="norm. MFI",title="curve fitting",subtitle=paste(tmp.qc.sigma$fit.type,"used"))
                              
                            }
                            
                          }
                          
                          #residual plot
                          progress$inc(1/5, detail = paste("generate residual plot"))
                          
                          if(sum(!is.na(tmp.residual.comp$residual.values))==0){
                            res.plot<-ggplot(tmp.residual.comp,aes(dilution.inverse,residual.values,color=fit.type))+
                              geom_blank()+
                              scale_color_manual(values = fit.type.colors)+
                              theme_bw(base_size = 12)+
                              theme(legend.position='bottom')+
                              labs(x="dilution",color="fit type",title="residual plot of used fit",subtitle="all residuals = NA")+
                              guides(color=FALSE)
                              #+
                              #guides(col = guide_legend(nrow = 3))
                           
                          }else{
                            res.plot<-tmp.residual.comp%>%filter(fit.type==unlist(tmp.qc.sigma$fit.type))%>%
                                      ggplot(aes(dilution.inverse,residual.values,color=fit.type))+
                                      geom_vline(xintercept = 1/min(tmp.am.tidy.agg[,dil.column.name.param],na.rm=T),color="hotpink",linetype="dashed",alpha=0.5)+
                                      geom_vline(xintercept = 1/max(tmp.am.tidy.agg[,dil.column.name.param],na.rm=T),color="hotpink",linetype="dashed",alpha=0.5)+        
                                      geom_hline(yintercept = 0)+
                                      geom_point(alpha=0.5,size=4)+
                                      scale_color_manual(values = fit.type.colors)+
                                      theme_bw(base_size = 12)+
                                      theme(legend.position='bottom')+
                                      labs(x="dilution",color="fit type",title="residual plot of used fit",subtitle=paste(tmp.qc.sigma$fit.type,"used"))+
                                      guides(color=FALSE)
                                      #+
                                      #guides(col = guide_legend(nrow = 1))
                       

                          }
                          
                          #generate dilution gate plot
                          progress$inc(1/5, detail = paste("generate dilution gate plot"))
                          
                          if(sum(!is.na(tmp.calculated.variables.comp$calculated.dilution))==0){
                            calculated.variables.plot<-ggplot(tmp.calculated.variables.comp,aes(calculated.dilution,calculated.MFI,color=fit.type))+
                              scale_color_manual(values = fit.type.colors)+
                              theme_bw(base_size = 12)+
                              theme(legend.position='none')+
                              labs(x="dilution",y="MFI",color="fit type",title="compare calc. response points of fits",subtitle=paste(tmp.qc.sigma$fit.type,"used"))
                            
                          }else{
                             calculated.variables.plot<-ggplot(tmp.calculated.variables.comp,aes(calculated.dilution,calculated.MFI,color=fit.type))+
                                   geom_hline(yintercept = max(tmp.am.tidy.agg[,"MFI_normalized_mean"],na.rm=T),color="black",linetype="dashed")+
                                   geom_vline(xintercept = 1/min(tmp.am.tidy.agg[,dil.column.name.param],na.rm=T),color="hotpink",linetype="dashed")+ 
                                   geom_vline(xintercept = 1/max(tmp.am.tidy.agg[,dil.column.name.param],na.rm=T),color="hotpink",linetype="dashed")+        
                                   geom_point(data=tmp.am.tidy.agg,aes(x = 1/UQ(as.name(dil.column.name.param)),y = MFI_normalized_mean),color="black",alpha=0.5)+
                                   geom_point(alpha=0.5,size=4)+
                                   scale_color_manual(values = fit.type.colors)+
                                   theme_bw(base_size = 12)+
                                   theme(legend.position='none')+
                                   geom_label_repel(aes(x = calculated.dilution,
                                                                y = calculated.MFI,
                                                                label=paste("MFImax/2:",round(calculated.MFI,digits = 2),"\nxfold-Dil.:",round(1/calculated.dilution,digits = 2))),
                                                     label.size = NA, 
                                                     alpha = 0.75,
                                                     max.iter=10000,
                                                     size=4,
                                                     fontface = 'plain', color = 'black',
                                                     nudge_y = 0.05,
                                                     direction = "x",
                                                     na.rm=TRUE)+
                               
                                   labs(x="dilution",y="MFI",color="fit type",title="compare calc. response points of fits",subtitle=paste(tmp.qc.sigma$fit.type,"used"))
                          }
                          
                          
                          
                          #response overview plot

                          #list return
                          return(list(plots.out.raw.overview.plot=raw.plot,
                                      plots.out.fit.overview.plot=fit.overview.plot,
                                      plots.out.res.plot=res.plot,
                                      plots.out.response=response.plot,
                                      plots.calculated.variables.plot=calculated.variables.plot,
                                      used.fit.type = tmp.fit.type,
                                      used.fit.type.col = tmp.fit.type.box.color,
                                      selection.title.text = paste("Antigen:",input$antigen,"/","Sample:",input$sample,"/","fit:",tmp.fit.type),
                                      selection.file.name=paste(processed_data()$parameter$project.name,
                                                                "__Antigen_",
                                                                input$antigen,
                                                                "__Sample_",
                                                                input$sample,
                                                                "__single_antigen_sample.pdf",
                                                                sep="")

                                      ) 
                          )
                          
                          
                        })
                        
                        
                        
                        ### render single sample & single antigen output ----------
                        ################################################
                        
                            output$plots.out.gg.AntigenSample.all<-renderPlot({
                                                                  # Create a Progress object
                                                                  progress <- shiny::Progress$new()
                                                                  # Make sure it closes when we exit this reactive, even if there's an error
                                                                  on.exit(progress$close())
                                                                  
                                                                  progress$set(message = "render single sample/antigen inspection...", value = 0)
                                                                  
                                                                  progress$inc(1/1, detail = paste(""))
                                                                                        #select log scale format
                                                                                        if(length(input$axis.log.scale)==0){
                                                                                          fit.overview.plot.out<-plots.out.gg.AntigenSample()$plots.out.fit.overview.plot
                                                                                          raw.plot.out<-plots.out.gg.AntigenSample()$plots.out.raw.overview.plot
                                                                                          res.plot.out<-plots.out.gg.AntigenSample()$plots.out.res.plot
                                                                                          res.calculated.variables.plot.out<-plots.out.gg.AntigenSample()$plots.calculated.variables.plot
                                                                                        }else{
                                                                                          if(input$axis.log.scale=="x-axis"){
                                                                                            fit.overview.plot.out<-plots.out.gg.AntigenSample()$plots.out.fit.overview.plot+scale_x_log10()
                                                                                            raw.plot.out<-plots.out.gg.AntigenSample()$plots.out.raw.overview.plot+scale_x_log10()
                                                                                            res.plot.out<-plots.out.gg.AntigenSample()$plots.out.res.plot+scale_x_log10()
                                                                                            res.calculated.variables.plot.out<-plots.out.gg.AntigenSample()$plots.calculated.variables.plot+scale_x_log10()
                                                                                            
                                                                                          }
                                                                                          if(input$axis.log.scale=="y-axis"){
                                                                                            fit.overview.plot.out<-plots.out.gg.AntigenSample()$plots.out.fit.overview.plot+scale_y_log10()
                                                                                            raw.plot.out<-plots.out.gg.AntigenSample()$plots.out.raw.overview.plot+scale_y_log10()
                                                                                            res.plot.out<-plots.out.gg.AntigenSample()$plots.out.res.plot
                                                                                            res.calculated.variables.plot.out<-plots.out.gg.AntigenSample()$plots.calculated.variables.plot+scale_y_log10()
                                                                                           
                                                                                          }
                                                                                          
                                                                                          if(sum(input$axis.log.scale ==c("x-axis", "y-axis"))==2){
                                                                                            fit.overview.plot.out<-plots.out.gg.AntigenSample()$plots.out.fit.overview.plot+scale_x_log10()+scale_y_log10()
                                                                                            raw.plot.out<-plots.out.gg.AntigenSample()$plots.out.raw.overview.plot+scale_x_log10()+scale_y_log10()
                                                                                            res.plot.out<-plots.out.gg.AntigenSample()$plots.out.res.plot+scale_x_log10()
                                                                                            res.calculated.variables.plot.out<-plots.out.gg.AntigenSample()$plots.calculated.variables.plot+scale_x_log10()+scale_y_log10()
                                                                                            
                                                                                          }
                                                                                        }
                              
                                                                                                  #do plot output of single antigen / sample overview
                                                                                                  grid.arrange(
                                                                                                    arrangeGrob(
                                                                                                              raw.plot.out,
                                                                                                              fit.overview.plot.out,
                                                                                                              #res.calculated.variables.plot.out,
                                                                                                              res.plot.out,
                                                                                                              #plots.out.gg.AntigenSample()$plots.out.response,
                                                                                                              #ncol = 5,
                                                                                                              ncol = 3,
                                                                                                              layout_matrix = rbind(c(1,2,3))
                                                                                                              #layout_matrix = rbind(c(1,1,2,2,3,3,4,4,5))
                                                                                                    )
                                                                                                  )
                                                                        
                                                                 })


                        output$selectedAntigenSample<-renderText(plots.out.gg.AntigenSample()$selection.title.text)

                        output$plots.out.gg.AntigenSample.fit.type<-renderValueBox({
                          valueBox(color=plots.out.gg.AntigenSample()$used.fit.type.col,
                                   value = tags$p(plots.out.gg.AntigenSample()$used.fit.type, style = "font-size: 50%;"),
                                   subtitle = tags$p("fit type", style = "font-size: 200%;"),
    
                                   icon=icon("line-chart")
                          )
                        })
                        
          ### Render single plot download -----
                        
                        output$UIdownloadSinglePlots<-renderUI({
                          if(length(which(names(plots.out.gg.AntigenSample())=="plots.out.fit.overview.plot"))==1){
                            tagList(downloadButton(outputId = "singleAntigenSampleReport",label = "download single plot")
                                    #helpText("Note : Use Ctrl+Click (Windows) / option+Click (Mac) to open the download in background")
                                    )
                          }
                        })
                        

                        output$singleAntigenSampleReport <- downloadHandler(
                          # For PDF output, change this to "report.pdf"
                          filename = function(){plots.out.gg.AntigenSample()$selection.file.name},
                          content = function(file) {
                            #select log scale format
                            if(length(input$axis.log.scale)==0){
                              fit.overview.plot.out<-plots.out.gg.AntigenSample()$plots.out.fit.overview.plot
                              raw.plot.out<-plots.out.gg.AntigenSample()$plots.out.raw.overview.plot
                              res.plot.out<-plots.out.gg.AntigenSample()$plots.out.res.plot
                              res.calculated.variables.plot.out<-plots.out.gg.AntigenSample()$plots.calculated.variables.plot
                            }else{
                              if(input$axis.log.scale=="x-axis"){
                                fit.overview.plot.out<-plots.out.gg.AntigenSample()$plots.out.fit.overview.plot+scale_x_log10()
                                raw.plot.out<-plots.out.gg.AntigenSample()$plots.out.raw.overview.plot+scale_x_log10()
                                res.plot.out<-plots.out.gg.AntigenSample()$plots.out.res.plot+scale_x_log10()
                                res.calculated.variables.plot.out<-plots.out.gg.AntigenSample()$plots.calculated.variables.plot+scale_x_log10()
                                
                              }
                              if(input$axis.log.scale=="y-axis"){
                                fit.overview.plot.out<-plots.out.gg.AntigenSample()$plots.out.fit.overview.plot+scale_y_log10()
                                raw.plot.out<-plots.out.gg.AntigenSample()$plots.out.raw.overview.plot+scale_y_log10()
                                res.plot.out<-plots.out.gg.AntigenSample()$plots.out.res.plot
                                res.calculated.variables.plot.out<-plots.out.gg.AntigenSample()$plots.calculated.variables.plot+scale_y_log10()
                                
                              }
                              
                              if(sum(input$axis.log.scale ==c("x-axis", "y-axis"))==2){
                                fit.overview.plot.out<-plots.out.gg.AntigenSample()$plots.out.fit.overview.plot+scale_x_log10()+scale_y_log10()
                                raw.plot.out<-plots.out.gg.AntigenSample()$plots.out.raw.overview.plot+scale_x_log10()+scale_y_log10()
                                res.plot.out<-plots.out.gg.AntigenSample()$plots.out.res.plot+scale_x_log10()
                                res.calculated.variables.plot.out<-plots.out.gg.AntigenSample()$plots.calculated.variables.plot+scale_x_log10()+scale_y_log10()
                                
                              }
                            }
                            
                            #do plot output of single antigen / sample overview
                            pdf(file,width = 20,height = 5)
                            grid.arrange(
                              arrangeGrob(
                                raw.plot.out,
                                fit.overview.plot.out,
                                #res.calculated.variables.plot.out,
                                res.plot.out,
                               # plots.out.gg.AntigenSample()$plots.out.response,
                                #ncol = 5,
                                ncol = 3,
                               
                                top = textGrob(paste("Antigen:",input$antigen,"/","Sample:",input$sample,"/","fit:",plots.out.gg.AntigenSample()$used.fit.type,"\n"),
                                               vjust = 1,
                                               gp = gpar(fontface = "bold", cex = 2)
                                              ),
                                #layout_matrix = rbind(c(1,1,2,2,3,3,4,4,5))
                               layout_matrix = rbind(c(1,2,3))
                              )
                            )
                            dev.off()
                            
                          }
                        )
                        
                     
                        
                        
#_________________________________________ ====================
          
            # fit test comparison -----------
                        #render UI for selector
                       
                        
                  #do statistical testing using wilcox test   
                        #statistics.over.Meta.Sample<-function() list(response.stats=resp.stats,test.groups=lev.out,tmp.response=tmp.response)

                  statistics.over.Meta.Sample<-reactive({


                                    tmp.response<-processed_data()$response
                                    #remove sample with missing value in meta data
                                    tmp.response<-tmp.response[!is.na(tmp.response[,input$meta.sele]),]
                                    tmp.response[,input$meta.sele]<-as.character(unlist(tmp.response[,input$meta.sele]))
                                    #levels
                                    lev<-unique(unlist(tmp.response[,input$meta.sele]))
                                    lev.comb<-combn(lev,m = 2)
                                    lev.out<-list()
                                    for(i in 1:ncol(lev.comb)){
                                      lev.out[[i]]<-c(lev.comb[1,i],lev.comb[2,i])
                                    }
                                    
                                    
                                    
                                    resp.stats<-c()
                                    antis<-unique(tmp.response$antigen)
                                    withProgress(message = 'Calculation of stat. tests in progress...',
                                                 detail = 'This may take a while...', value = 0,{
                                    for(g in 1:length(antis)){
                                      incProgress(1/length(antis),detail = paste(g,"/",length(antis),"antigens"))
                                      resp.stats.tmp<-c()
                                              for(i in 1:length(lev.out)){
                                                tmp.resp.anti<-tmp.response[tmp.response$antigen%in%antis[g],]
                                                
                                                t1<-c();t1<-filter(tmp.resp.anti,UQ(as.name(input$meta.sele))==unlist(lev.out[i])[1])$response
                                                t2<-c();t2<-filter(tmp.resp.anti,UQ(as.name(input$meta.sele))==unlist(lev.out[i])[2])$response
                                                if(sum(is.na(t1))==length(t1) | sum(is.na(t2))==length(t2)){
                                                }else{
                                                  #Wilcoxon rank sum test with continuity correction
                                                  tmp.test<-try(wilcox.test(x = t1,y = t2,alt= "two.sided", conf.int= T, conf.level= 0.95, paired= FALSE,mu=0),silent = T)
                                                  resp.stats.tmp<-rbind(resp.stats.tmp,
                                                                    c(antigen = antis[g],
                                                                      median.ratio = median(t1,na.rm=T)/median(t2,na.rm = T),
                                                                      mean.ratio = mean(t1,na.rm=T)/mean(t2,na.rm = T),
                                                                      p.value = tmp.test$p.value,
                                                                      lower.confidence.level = as.numeric(tmp.test$conf.int)[1],
                                                                      upper.confidence.level = as.numeric(tmp.test$conf.int)[2],
                                                                      differnce.in.location = as.numeric(tmp.test$estimate),
                                                                      comparison = paste(unlist(lev.out[i]),collapse = " vs "),
                                                                      meta.category = input$meta.sele
                                                                    )
                                                                  )
                                                  
                                                }
                                                
                                               
                                                
                                              }
                                      resp.stats.tmp<-as.data.frame(resp.stats.tmp)
                                      if(nrow(resp.stats.tmp)>1){
                                        resp.stats.tmp$p.value.adjusted.BH<-p.adjust(as.numeric(as.character(resp.stats.tmp$p.value)),method = "BH")
                                      }else{
                                        resp.stats.tmp$p.value.adjusted.BH<-as.numeric(as.character(resp.stats.tmp$p.value))
                                      }
                                      #combine resp stats
                                      resp.stats<-rbind(resp.stats,resp.stats.tmp) 
                                    }
                                    })#end progress
                                    resp.stats<-as_tibble(resp.stats)
                                    resp.stats[,2:7]<-apply(resp.stats[,2:7],2,function(x) as.numeric(as.character(unlist(x))))
                                    resp.stats[,1]<-as.character(unlist(resp.stats[,1]))
                                    resp.stats$significant<-FALSE
                                    resp.stats$significant[resp.stats$p.value.adjusted.BH<=0.05]<-TRUE
                                    resp.stats<-resp.stats %>% select(meta.category,
                                                                      comparison,
                                                                      antigen,
                                                                      significant,
                                                                      p.value,
                                                                      p.value.adjusted.BH,
                                                                      median.ratio,
                                                                      mean.ratio,
                                                                      lower.confidence.level,
                                                                      upper.confidence.level,
                                                                      differnce.in.location) %>% arrange(p.value.adjusted.BH)
                                    
                                    
                                  
                                    
                                    
                                    return(
                                      list(response.stats=resp.stats,
                                           test.groups=lev.out,
                                           tmp.response=tmp.response
                                           )
                                      )
                          
                        })
                        
                      
                        
                
                        #statistics.over.Meta.Sample<-function()list(response.stats=resp.stats,test.groups=lev.out,tmp.response=tmp.response)
                  
                        #render response statistics DT based on meta data selection ------
                
                        output$stats.resptable <- DT::renderDataTable({
                          
                                        statistics.over.Meta.Sample()$response.stats
                                            },
                                            options = list(
                                              scrollX = TRUE,selection = 'single',searchHighlight = TRUE)
                                              
                                              )

                      #render valueBox for selection of metadata used for statistical testing 
                      output$stats.resptable.meta.selection<-renderValueBox({
                          valueBox(color=plots.out.gg.AntigenSample()$used.fit.type.col,
                                   value = tags$p(unique(unlist(statistics.over.Meta.Sample()$response.stats$meta.category)), style = "font-size: 100%;"),
                                   subtitle = tags$p("selected meta data", style = "font-size: 100%;"),
                                   icon=icon("clipboard-list")
                          )
                        })
                      
                      output$download.stats.resptable<-downloadHandler(
                            filename = function(){paste(format(Sys.Date(),"%Y_%m_%d"),processed_data()$parameter$project.name,"_xMAPr_statistics_selected_meta_data_",
                                            unique(unlist(statistics.over.Meta.Sample()$response.stats$meta.category)),".txt",sep="")},
                            content = function(fname) {
                                
                              write_delim(x = statistics.over.Meta.Sample()$response.stats,file = fname,delim = "\t",col_names = T)
                              
                        })
                          
                      
                      #volcano plots after test ========
                      #_____________________________________

                      
                      output$volcanoPlotUI <- renderUI({
                        addSpinner(plotOutput("plot_volcano", width = input$volcanoWidth, height = input$volcanoHeight), spin = "folding-cube", color = "#FF6600")
                      })
                      
                      plot_volcano_reactive <- reactive({
                        # Create a Progress object
                        progress <- shiny::Progress$new()
                        # Make sure it closes when we exit this reactive, even if there's an error
                        on.exit(progress$close())
                        
                        progress$set(message = "render volcano plot...", value = 0)
                        
                        progress$inc(1/1, detail = paste(""))
                        
                        fc_cut = input$volcano_FC_cut_off
                        p_val_cut = input$volcano_pvalue_cut_off
                        p_value_select <- ifelse(test = input$volcano_p_value_type=="adjusted",yes = "p.value.adjusted.BH",no = "p.value")
                        
                        
                        #filter for selected antigen
                        gg_tmp_stats <- statistics.over.Meta.Sample()$response.stats %>% 
                                        mutate(log2_median_ratio = log2(median.ratio)) %>% 
                                        mutate(changed = ifelse(test = log2_median_ratio >=log2(fc_cut) & UQ(as.name(p_value_select))<=p_val_cut,
                                                                yes = "up",
                                                                no = ifelse(test = log2_median_ratio<=log2(1/fc_cut) & UQ(as.name(p_value_select))<=p_val_cut,
                                                                            yes = "down",
                                                                            no = "none")
                                                                )
                                               ) %>% 
                                      ungroup()
      
                        
                        #color for volcano plot
                        color_changed = c(up = input$volcano_up_col,none = input$volcano_none_col, down = input$volcano_down_col)
                        
                        #generate violin plot
                        ggvolcano_plot <- ggplot(data = gg_tmp_stats,
                                               mapping = aes(log2_median_ratio,-log10(UQ(as.name(p_value_select))),color = changed))+
                          facet_wrap(~comparison,ncol = 3)+
                          geom_point(alpha=0.8)+
                          coord_cartesian(xlim = c(-max(abs(gg_tmp_stats$log2_median_ratio),na.rm=T),max(abs(gg_tmp_stats$log2_median_ratio),na.rm=T)))+
                          labs(title=input$meta.sele,x=expression(log[2]~"median ratio"), y = expression("-"~log[10]~"p-value"),caption=expression(log[2]~"median ratio vs. p-value"))+
                          scale_color_manual(values = color_changed)+
                          theme_minimal(base_size = input$volcano_theme_size)+
                          geom_vline(xintercept = c(log2(fc_cut),log2(1/fc_cut)),color="black",linetype="dashed")+
                          geom_hline(yintercept = -log10(p_val_cut),color="black",linetype="dashed")+
                          guides(color=F)
                        #add significant names
                        if(input$volcano_display_labels=="yes"){
                          ggvolcano_plot <- ggvolcano_plot+
                            geom_text_repel(data = gg_tmp_stats %>% filter(changed!="none"),
                                            mapping = aes(label=antigen),min.segment.length = 0.01)
                        }
                        
                        
                        #output volcano
                        list(ggvolcano_plot = ggvolcano_plot)
                        
                        
                      })
        
                      
                      #render & download volcano plot -----
                      #___________________________
                      
                      output$plot_volcano <- renderPlot({plot_volcano_reactive()$ggvolcano_plot})
                      
                      output$download_volcano_plot<-downloadHandler(
                        filename = function(){paste("Volcano_plot__FC_cutoff",input$volcano_FC_cut_off,"__p_value_cutoff",input$volcano_FC_cut_off,"__",input$meta.sele,".pdf",sep="")},
                        content = function(fname) {
                          ggsave(filename = fname,plot = plot_volcano_reactive()$ggvolcano_plot,device = "pdf",width = input$volcanoWidth/100,height = input$volcanoHeight/100)
                        })
                      
                      #
              #single violinplots after test ========
              #_____________________________________
                      
                  
                      plot_antigen_violin_test_reactive <- reactive({
                                                # Create a Progress object
                                                progress <- shiny::Progress$new()
                                                # Make sure it closes when we exit this reactive, even if there's an error
                                                on.exit(progress$close())
                                                
                                                progress$set(message = "render violin plot...", value = 0)
                                                
                                                progress$inc(1/1, detail = paste(""))
                                                #filter for selected antigen
                                                gg.tmp.dat.vio.plot.stats<-filter(statistics.over.Meta.Sample()$response.stats,antigen==input$antigen.stats)
                                                #generate stars for adjusted p-value
                                                gg.tmp.dat.vio.plot.stats$star_pvalue<-sapply(gg.tmp.dat.vio.plot.stats$p.value.adjusted.BH, function(x) x < c(.001, .01, .05, .1)) %>% 
                                                  colSums() %>% 
                                                  add(1) %>%
                                                  c("ns", ".", "*", "**", "***")[.]
                                                #add ranks
                                                gg.tmp.dat.vio.plot.stats$rank<-seq(1,nrow(gg.tmp.dat.vio.plot.stats))
                                                #generate comparisons
                                                gg.tmp.dat.vio.plot.stats.comp<-gg.tmp.dat.vio.plot.stats %>% separate(col = comparison,sep = " vs ",into = c("comp.group1","comp.group2"))  %>% group_by(rank) %>% summarize(comparisons=list(c(comp.group1,comp.group2))) 
                                            #generate violin plot
                                            gg.plot.violin<-ggplot(filter(statistics.over.Meta.Sample()$tmp.response,antigen==input$antigen.stats),
                                                                   aes(as.factor(UQ(as.name(input$meta.sele))),response,fill=UQ(as.name(input$meta.sele))))+
                                              geom_violin(trim=FALSE,color="white")+
                                              geom_boxplot(width=0.1, fill="grey65",color="grey15")+
                                              labs(subtitle=input$meta.sele,title=input$antigen.stats,x="", y = "response",caption="BH adjusted p-value per antigen / missing values in meta data removed\n
                                                                                                                                    <0.001 = *** | <0.01 = ** | <0.05 = * | <0.1 = . | >0.1 = not sure (ns)")+
                                              scale_fill_manual(values = c(meta.colors.selected()))+
                                              theme_classic(base_size = 15)+
                                              stat_signif(test = "wilcox.test",
                                                          comparisons = gg.tmp.dat.vio.plot.stats.comp$comparisons,
                                                          step_increase=0.2,
                                                          annotations = gg.tmp.dat.vio.plot.stats$star_pvalue
                                                          )+
                                              theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5))
                                            
                                            if(!is.null(input$axis.log.scale.test)){
                                                gg.plot.violin<-gg.plot.violin+scale_y_log10()
                                              }
                                            list(gg.plot.violin = gg.plot.violin)
                                            
                                            
                                              })

                      # render & download violinplot -----
                      #____________________________________
                      
                      output$plot.antigen.violin.test <- renderPlot({plot_antigen_violin_test_reactive()$gg.plot.violin})
                      output$download_violin_plot<-downloadHandler(
                        filename = function(){paste("Violin_plot__antigen_",input$antigen.stats,"__",input$meta.sele,".pdf",sep="")},
                        content = function(fname) {
                          ggsave(plot = plot_antigen_violin_test_reactive()$gg.plot.violin,filename = fname,device = "pdf")
                        })
                      
                      #render p-value adjustment plot---------
                      output$plot.p.values.test<-renderPlotly({
                        
                                      # Create a Progress object
                                      progress <- shiny::Progress$new()
                                      # Make sure it closes when we exit this reactive, even if there's an error
                                      on.exit(progress$close())
                                      
                                      progress$set(message = "render p-value plot...", value = 0)
                                      
                                      progress$inc(1/1, detail = paste(""))
                        
                                    ggplotly(
                                      ggplot(statistics.over.Meta.Sample()$response.stats,aes(p.value,p.value.adjusted.BH,text = paste("antigen:",antigen,
                                                                                                     "<br>comparison:",comparison,
                                                                                                     "<br>p-value:",scientific(p.value,digits = 5),
                                                                                                     "<br>p-value (BH):",scientific(p.value.adjusted.BH,digits = 5),
                                                                                                     "<br>median-ratio:",round(median.ratio,digits = 4))))+
                                        geom_point()+
                                        geom_point(data = filter(statistics.over.Meta.Sample()$response.stats,antigen==input$antigen.stats),mapping = aes(p.value,p.value.adjusted.BH),colour="red")+
                                        geom_abline(slope = 1,intercept = 0)+
                                        theme_bw(base_size = 12)+
                                        labs(x="p-value",y="BH adjusted p-value",title="p-value adjustment")+
                                        geom_vline(xintercept = 0.05,colour="tomato",linetype="dashed")+
                                        geom_hline(yintercept = 0.05,colour="tomato",linetype="dashed")+
                                        scale_y_log10()+
                                        scale_x_log10()+
                                        coord_cartesian(ylim = c(min(c(statistics.over.Meta.Sample()$response.stats$p.value,statistics.over.Meta.Sample()$response.stats$p.value.adjusted.BH),na.rm=T),1),
                                                        xlim =c(min(c(statistics.over.Meta.Sample()$response.stats$p.value,statistics.over.Meta.Sample()$response.stats$p.value.adjusted.BH),na.rm=T),1) 
                                                        ),
                                      tooltip="text",height = 400,width = 450, autosize=TRUE)
                        
                        })
                      

                      
                
                        
                        
  }
)
#server END
  


             