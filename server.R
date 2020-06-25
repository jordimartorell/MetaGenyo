

# options(encoding = "ISO-8859-1") # Codificación para permitir los carácteres españoles. ERROR CON SERVIDOR
# collapseStyle <-
#   "padding: 0;
#   margin: 0;
#   background-color: #ecf0f1;"


shinyServer(function(input, output, session) {
  ##########################################################################################
  ## En este bloque se generará el texto que se muestra en cada panel antes de subir ningún
  ## fichero, ya que no se puede reutilizar el mismo output en distintos paneles
  ##########################################################################################
  output$yournodata = renderText({
    if (is.null(input$inputfile) & input$inputMode != "Interactive table") # Si no se ha subido ningún archivo se mostrará este mensaje
      "You have not submitted your data yet."
    else # En cambio, si se ha subido alguno se mostrará este
      'Check if the input file is submitted correctly. If not, return to "Data input" panel.'
  })
  
  output$buttonnext1 = renderUI({
    if (is.null(input$inputfile) & input$inputMode != "Interactive table") # No se debe mostrar nada si no se ha subido ningún archivo
      return(NULL)
    else if (input$inputMode == "Interactive table"){
      validate(
        need(nrow(datatoshow()) > 1, FALSE)
      )
    }
    actionButton("next1", "NEXT")
  })
  
  
  output$hwnodata = renderText({
    if (is.null(input$inputfile) & input$inputMode != "Interactive table") # Si no se ha subido ningún archivo se mostrará este mensaje
      "You have not submitted your data yet."
    else # En cambio, si se ha subido alguno se mostrará este
      "Here is your data with the Hardy-Weinberg Equilibrium calculated."
  })
  
  output$assnodata = renderText({
    if (is.null(input$inputfile) & input$inputMode != "Interactive table") # Si no se ha subido ningún archivo se mostrará este mensaje
      "You have not submitted your data yet."
    else # En cambio, si se ha subido alguno se mostrará este
      return(NULL)
  })
  
  output$white = renderText({
    if (is.null(input$inputfile) & input$inputMode != "Interactive table"){ # Si no se ha subido ningún archivo se mostrará este mensaje
      return(NULL)
    }
    else{ # En cambio, si se ha subido alguno se mostrará este
      shinyjs::hide("wellpanel1")
      " "
    }
  })
  
  
  output$fornodata = renderText({
    if (is.null(input$inputfile) & input$inputMode != "Interactive table") # Si no se ha subido ningún archivo se mostrará este mensaje
      "You have not submitted your data yet."
    else # En cambio, si se ha subido alguno se mostrará este
      ""
  })
  
  output$white2 = renderText({
    if (is.null(input$inputfile) & input$inputMode != "Interactive table") # Si no se ha subido ningún archivo se mostrará este mensaje
      return(NULL)
    else # En cambio, si se ha subido alguno se mostrará este
      shinyjs::hide("wellpanel2")
      " "
  })
  
  output$funnodata = renderText({
    if (is.null(input$inputfile) & input$inputMode != "Interactive table") # Si no se ha subido ningún archivo se mostrará este mensaje
      "You have not submitted your data yet."
    else # En cambio, si se ha subido alguno se mostrará este
      ""
  })
  
  output$white3 = renderText({
    if (is.null(input$inputfile) & input$inputMode != "Interactive table") # Si no se ha subido ningún archivo se mostrará este mensaje
      return(NULL)
    else # En cambio, si se ha subido alguno se mostrará este
      shinyjs::hide("wellpanel3")
      " "
  })
  
  output$white4 = renderText({
    if (is.null(input$inputfile) & input$inputMode != "Interactive table") # Si no se ha subido ningún archivo se mostrará este mensaje
      return(NULL)
    else # En cambio, si se ha subido alguno se mostrará este
      " "
  })
  
  output$strnodata = renderText({
    if (is.null(input$inputfile) & input$inputMode != "Interactive table") # Si no se ha subido ningún archivo se mostrará este mensaje
      "You have not submitted your data yet."
    else # En cambio, si se ha subido alguno se mostrará este
      " "
  })
  
  output$robnodata = renderText({
    if (is.null(input$inputfile) & input$inputMode != "Interactive table") # Si no se ha subido ningún archivo se mostrará este mensaje
      "You have not submitted your data yet."
    else # En cambio, si se ha subido alguno se mostrará este
      ""
  })
  
  output$white5 = renderText({
    if (is.null(input$inputfile) & input$inputMode != "Interactive table") # Si no se ha subido ningún archivo se mostrará este mensaje
      return(NULL)
    else # En cambio, si se ha subido alguno se mostrará este
      shinyjs::hide("wellpanel5")
    " "
  })
  

  ##################
  ## Botones NEXT ##
  ##################
  # Acciones de los botones NEXT de cada sección
  observeEvent(input$next1,{
    updateTabsetPanel(session, "navbar", "Your data")
  }, autoDestroy=F)
  
  observeEvent(input$next2,{
    updateTabsetPanel(session, "navbar", "Hardy-Weinberg table")
  }, autoDestroy=F)
  
  observeEvent(input$next3,{
    updateTabsetPanel(session, "navbar", "Association values")
  }, autoDestroy=F)
  
  observeEvent(input$next4,{
    updateTabsetPanel(session, "navbar", "Forest plots")
  }, autoDestroy=F)
  
  observeEvent(input$next5,{
    updateTabsetPanel(session, "navbar", "Publication bias")
  }, autoDestroy=F)
  
  observeEvent(input$next6,{
    updateTabsetPanel(session, "navbar", "Subgroup analysis")
  }, autoDestroy=F)
  
  observeEvent(input$next7,{
    updateTabsetPanel(session, "navbar", "Sensitivity")
  }, autoDestroy=F)
  
  
  #########################
  ## Entrada interactiva ##
  #########################
  dataNew = eventReactive(c(input$riskAllele, input$refAllele, input$addCol),{
    col1 = paste0(input$riskAllele, input$riskAllele, "_Cases")
    col2 = paste0(input$riskAllele, input$refAllele, "_Cases")
    col3 = paste0(input$refAllele, input$refAllele, "_Cases")
    col4 = paste0(input$riskAllele, input$riskAllele, "_Controls")
    col5 = paste0(input$riskAllele, input$refAllele, "_Controls")
    col6 = paste0(input$refAllele, input$refAllele, "_Controls")

    dataTable = hot_to_r2(input$hotable1)
    numberCols = ncol(dataTable)
    namesCols = colnames(dataTable)
    if (namesCols[numberCols-6] != input$newCol1 & input$newCol1 != "") {
      dataOut = data.frame(dataTable[,1:(numberCols-6)], new = rep("",nrow(dataTable)), dataTable[,-(1:(numberCols-6))])
      colnames(dataOut) = c(namesCols[1:(numberCols-6)], input$newCol1, c(col1, col2, col3, col4, col5, col6))
    }
    else {
      dataOut = dataTable
      colnames(dataOut) = c(namesCols[1:(numberCols-6)], c(col1, col2, col3, col4, col5, col6))
    }
    
    return(dataOut)
  })
  
  buttonVal0 <- 0
  
  dataInt = reactive({
    buttonVal <<- input$loadEx
    colb = input$addCol

    if (buttonVal > buttonVal0) {
      buttonVal0 <<- buttonVal0 + 1
      updateSelectizeInput(session, "riskAllele", selected="A")
      updateSelectizeInput(session, "refAllele", selected="T")
      updateTextInput(session, "newCol1", value = "")

      col1 = paste0(input$riskAllele, input$riskAllele, "_Cases")
      col2 = paste0(input$riskAllele, input$refAllele, "_Cases")
      col3 = paste0(input$refAllele, input$refAllele, "_Cases")
      col4 = paste0(input$riskAllele, input$riskAllele, "_Controls")
      col5 = paste0(input$riskAllele, input$refAllele, "_Controls")
      col6 = paste0(input$refAllele, input$refAllele, "_Controls")

      dataTable = read_excel("www/Zhang.xlsx")
      colnames(dataTable) = c("Study", "Ethnicity", "Source_controls", "Genotyping_method", col1, col2, col3, col4, col5, col6)
      return(dataTable)

    }
    else {
      col1 = paste0(input$riskAllele, input$riskAllele, "_Cases")
      col2 = paste0(input$riskAllele, input$refAllele, "_Cases")
      col3 = paste0(input$refAllele, input$refAllele, "_Cases")
      col4 = paste0(input$riskAllele, input$riskAllele, "_Controls")
      col5 = paste0(input$riskAllele, input$refAllele, "_Controls")
      col6 = paste0(input$refAllele, input$refAllele, "_Controls")
      


      if (input$addCol == 0 & input$loadEx == 0) {
        if (is.null(input$hotable1)) {
          data.frame(matrix("", nrow = 10, ncol = 7, dimnames = list(1:10, c("Study", col1, col2, col3, col4, col5, col6))))
        }
        
        else {
          dataTable = isolate(hot_to_r2(input$hotable1))
          colnames(dataTable) = c(colnames(dataTable)[1], c(col1, col2, col3, col4, col5, col6))
          return(dataTable)
        }
      
      }
      else {
        dataNew()
      }
    }
  })
  

  output$hotable1 <- renderRHandsontable({rhandsontable(dataInt(), rowHeaders = NULL, useTypes = F) %>% 
                                                        hot_table(manualColumnResize=TRUE, enableComments = FALSE,
                                                                  manualColumnMove = FALSE, contextMenu = FALSE)})
  
  
  ###############################################################################################
  ## Se procesa la tabla input en dos funciones: una que siempre devuelve el input y servirá para
  ## mostrar los datos introducidos (datatoshow()) y otra que muestra error si el input no es 
  ## adecuado, que servirá para hacer los cálculos (datatocalc())
  ###############################################################################################
  
  output$char = renderUI({
    if(!is.null(input$inputfile)){
      if (grepl("Error", try(read_excel(input$inputfile[,4])))){
        list(tags$hr(),
             h4("Specify your input characteristics"),
             
             radioButtons('sep', 'Separator',
                          c(Comma=',',
                            Semicolon=';',
                            Tab='\t'),
                          '\t'),
             radioButtons('quote', 'Quote',
                          c('None'='',
                            'Double Quote'='"',
                            'Single Quote'="'"),
                          ''),
	     tags$hr()
        )
      }}
  })

  datatoshow = reactive({
    if (input$inputMode == "Interactive table") {
      data = hot_to_r2(input$hotable1)
      data = data[apply(data != "", 1, all),]
      if (nrow(data) > 0) {
        data[,1] = make.unique(as.character(data[,1]), sep = "_")
        data[,(ncol(data) - 5):(ncol(data))] = apply(data[,(ncol(data) - 5):(ncol(data))], 2, as.numeric)
      }
      return(data)
      
    }
    else {
      req(input$inputfile)
      if (grepl("Error", try(read_excel(input$inputfile[,4])))){
        data = read.delim(input$inputfile[,4], sep=input$sep,
                          header=T, quote=input$quote)
        data[,1] = make.unique(as.character(data[,1]), sep = "_")
        
      }
      
      else {
        data = read_excel(input$inputfile[,4])
        data[,1] = make.unique(as.character(data[,1]), sep = "_")
        
      }
      return(data)
      
    }

  })
  
  allele1 = reactive({
    if (input$inputMode == "Interactive table") {
      allele = input$riskAllele
    }
    else {
      allele = substr(colnames(datatoshow()[(ncol(datatoshow()) - 5)]), 1, 1)
    }
    return(allele)
    
  })
  
  allele2 = reactive({
    if (input$inputMode == "Interactive table") {
      allele = input$refAllele
    }
    else {
      allele = substr(colnames(datatoshow()[(ncol(datatoshow()) - 3)]), 1, 1)
    }
    return(allele)
  })
  
  ## GENETIC MODELS SELECTION
  
  alle = reactive({
    if (input$inputMode == "Interactive table") {
      paste0("Allele contrast (", allele1(), " vs. ", allele2(), ")")
    }
    else {
      if (input$ref == allele1()){
        paste0("Allele contrast (", allele1(), " vs. ", allele2(), ")")
      }
      else {
        paste0("Allele contrast (", allele2(), " vs. ", allele1(), ")")
      }
    }
  })
  
  rece = reactive({
    if (input$inputMode == "Interactive table") {
      paste0("Recessive model (", allele1(), allele1(), " vs. ", allele1(), allele2(), "+",  allele2(), allele2(),")")
    }
    else {
      if (input$ref == allele1()){
        paste0("Recessive model (", allele1(), allele1(), " vs. ", allele1(), allele2(), "+",  allele2(), allele2(),")")
      }
      else {
        paste0("Recessive model (", allele2(), allele2(), " vs. ", allele2(), allele1(), "+",  allele1(), allele1(),")")
      }
    }
  })
  
  domi = reactive({
    if (input$inputMode == "Interactive table") {
      paste0("Dominant model (", allele1(), allele1(), "+", allele1(), allele2(), " vs. ", allele2(), allele2(),")")
    }
    else {
      if (input$ref == allele1()){
        paste0("Dominant model (", allele1(), allele1(), "+", allele1(), allele2(), " vs. ", allele2(), allele2(),")")
      }
      else {
        paste0("Dominant model (", allele2(), allele2(), "+", allele2(), allele1(), " vs. ", allele1(), allele1(),")")
      }
    }
  })
  
  over = reactive({
    if (input$inputMode == "Interactive table") {
      paste0("Overdominant model (", allele1(), allele2(), " vs. ", allele1(), allele1(), "+", allele2(), allele2(),")")
    }
    else {
      if (input$ref == allele1()){
        paste0("Overdominant model (", allele1(), allele2(), " vs. ", allele1(), allele1(), "+", allele2(), allele2(),")")
      }
      else {
        paste0("Overdominant model (", allele2(), allele1(), " vs. ", allele2(), allele2(), "+", allele1(), allele1(),")")
      }
    }
  })
  
  pairw1 = reactive({
    if (input$inputMode == "Interactive table") {
      paste0(allele1(), allele1(), " vs. ", allele2(), allele2())
    }
    else {
      if (input$ref == allele1()){
        paste0(allele1(), allele1(), " vs. ", allele2(), allele2())
      }
      else {
        paste0(allele2(), allele2(), " vs. ", allele1(), allele1())
      }
    }
  })
  
  pairw2 = reactive({
    if (input$inputMode == "Interactive table") {
      paste0(allele1(), allele1(), " vs. ", allele1(), allele2())
    }
    else {
      if (input$ref == allele1()){
        paste0(allele1(), allele1(), " vs. ", allele1(), allele2())
      }
      else {
        paste0(allele2(), allele2(), " vs. ", allele2(), allele1())
      }
    }
  })
  
  pairw3 = reactive({
    if (input$inputMode == "Interactive table") {
      paste0(allele1(), allele2(), " vs. ", allele2(), allele2())
    }
    else {
      if (input$ref == allele1()){
        paste0(allele1(), allele2(), " vs. ", allele2(), allele2())
      }
      else {
        paste0(allele2(), allele1(), " vs. ", allele1(), allele1())
      }
    }
  })  
  

  
  list.model = reactive({
    listmodel = list()
    listmodel[[alle()]] = "allele"
    listmodel[[rece()]] = "recessive"
    listmodel[[domi()]] = "dominant"
    listmodel[[over()]] = "overdominant"
    listmodel[[pairw1()]] = "pairw1"
    listmodel[[pairw2()]] = "pairw2"
    listmodel[[pairw3()]] = "pairw3"
    return(listmodel)
  })
  

  output$modelasso = renderUI({
    radioButtons("methods", "Select comparison",
                 list.model())
  })
  
  output$modelforest = renderUI({
    radioButtons("methodsf", "Select comparison",
                 list.model())
  })
  
  output$modelbias = renderUI({
    radioButtons("methodsfun", "Select comparison",
                 list.model())
  })
  
  output$modelrob = renderUI({
    radioButtons("methodsrob", "Select comparison",
                 list.model())
  })
  
#   output$reference1strat = renderText({
#     base = substr(input$ref, 1, 1)
#     if (base == allele1()){
#       paste0("A = ", allele1())
#     }
#     else{
#       paste0("A = ", allele2())
#     }
#   })
  
#   output$reference2strat = renderText({
#     base = substr(input$ref, 1, 1)
#     
#     if (base == allele1()){
#       paste0("a = ", allele2())
#     }
#     else{
#       paste0("a = ", allele1())
#     }
#   })

  
  datatocalc = reactive({
    if (input$inputMode == "Interactive table") {
      data = hot_to_r2(input$hotable1)
      data = data[apply(data != "", 1, all),]
      if (nrow(data) > 0) {
        data[,1] = make.unique(as.character(data[,1]), sep = "_")
        data[,(ncol(data) - 5):(ncol(data))] = apply(data[,(ncol(data) - 5):(ncol(data))], 2, as.numeric)
      }
      validate(
        need(try(writemeta(data, "allele", columns())), "Something has failed. Please, check your data input.")
      )
      validate(
        need(nrow(data) > 1, "To perform the meta-analysis, more than one study data should be introduced.")
      )
      
      return(data)
      
    }
    else {
      req(input$inputfile)
      
      if (grepl("Error", try(read_excel(input$inputfile[,4])))){
        data = read.delim(input$inputfile[,4], sep=input$sep,
                          header=T, quote=input$quote)
        data[,1] = make.unique(as.character(data[,1]), sep = "_")
      }
      
      else {
        data = read_excel(input$inputfile[,4])
        data[,1] = make.unique(as.character(data[,1]), sep = "_")
      }
      validate(
        need(try(writemeta(data, "allele", columns())), "Something has failed. Please, check your data input.")
      )
      validate(
        need(nrow(data) > 1, "To perform the meta-analysis, more than one study data should be introduced.")
      )
      
      return(data)
    }
    

  })
  
  
  # Se eligen las columnas con el alelo de referencia
  output$refallele = renderUI({
    if (!is.null(input$inputfile) & input$inputMode == "Submit a file"){
      option1 = substr(colnames(datatoshow())[columns()+1], 1, 1)
      option2 = substr(colnames(datatoshow())[columns()+3], 1, 1)
      list(h4("Select the risk allele"),
           radioButtons("ref", NULL,
                        c(option1, option2)))}
  })
  
  # Se preparan los datos finales para realizar el meta-análisis teniendo en cuenta el alelo de referencia elegido
  datatocalcfinal = reactive({
    if (input$inputMode == "Interactive table") {
      data = datatocalc()
      return(data)
      
    }
    else {
      if (!is.null(input$inputfile)){
          data = datatocalc()
  
          if (input$ref == substr(colnames(data)[columns() + 1],1,1)) {
            return(data)
          }
          
          else {
            data[,columns()+1] = datatocalc()[,columns() + 3]
            colnames(data)[columns()+1] = colnames(datatocalc())[columns() + 3]
            data[,columns()+3] = datatocalc()[,columns() + 1]
            colnames(data)[columns()+3] = colnames(datatocalc())[columns() + 1]
            data[,columns()+4] = datatocalc()[,columns() + 6]
            colnames(data)[columns()+4] = colnames(datatocalc())[columns() + 6]
            data[,columns()+6] = datatocalc()[,columns() + 4]
            colnames(data)[columns()+6] = colnames(datatocalc())[columns() + 4]
            return(data)
            
          }
        
  
      }
    }
    })
  ###############
  ## Your data ##
  ###############
  
  ## Se muestra la tabla input al usuario
  output$yourdatafile = DT::renderDataTable({
    if (input$inputMode != "Interactive table") {
      validate(need(!is.null(input$inputfile), FALSE))
    }
    validate(
      need(nrow(datatoshow()) > 1, "You must introduce more than one study to perform a meta-analysis.")
    )
    if (input$inputMode != "Interactive table") {
      validate(
        need(try(writemeta(datatoshow(), "allele", columns())), "Something has failed. Please, check your data input.")
      )
    }
    shinyjs::show("br1")
    shinyjs::show("br2")
    
    datatable(datatoshow(), rownames = F, escape = T, selection = "none",
              options=list(paging=FALSE, searching=FALSE, autoWidth=F, columnDefs = list(list(class="dt-center", targets = "_all"))))
    })
  
  output$buttonnext2 = renderUI({
    if (is.null(input$inputfile) & input$inputMode != "Interactive table") # No se debe mostrar nada si no se ha subido ningún archivo
      return(NULL)
    # datatocalcfinal()
    validate(
      need(nrow(datatoshow()) > 1, FALSE)
    )
    
    actionButton("next2", "NEXT")
  })
  
  ##########################
  ## Hardy-Weinberg table ##
  ##########################
  
  ## Se crea la tabla con el valor de HWE incluido
  datahw = function(){
    validate(
      need(try(HW(datatocalc(), columns())), "Something has failed. Please, check your data input.")
    )
    
    HW(datatocalc(), columns())
  }
  
  ## Se muestra la tabla con el valor HWE
  output$hwPanel = renderUI({
    if (input$inputMode != "Interactive table") {
      req(input$inputfile)
      validate(
        need(nrow(datatoshow()) > 1, "You must introduce more than one study to perform a meta-analysis.")
      )
      list(br(id="br3"),
           br(id="br4"),
           selectInput("format", "Choose a format to download your data:", 
                       choices = c("Excel format (.xls)",
                                   "Plain-text format (.csv)",
                                   "Plain-text format (.tsv)"), width = 200),
           downloadButton('downhw', 'Download'),
           tags$hr(id="hr1"),
           sliderInput("thresholdHW", label = "Select a p-value threshold", min = 0, 
                       max = 1, value = 0.05, width = "400px")
      )
    }
    else {
      validate(
        need(nrow(datatoshow()) > 1, "You must introduce more than one study to perform a meta-analysis.")
      )
      list(br(id="br3"),
           br(id="br4"),
           selectInput("format", "Choose a format to download your data:", 
                       choices = c("Excel format (.xls)",
                                   "Plain-text format (.csv)",
                                   "Plain-text format (.tsv)"), width = 200),
           downloadButton('downhw', 'Download'),
           tags$hr(id="hr1"),
           sliderInput("thresholdHW", label = "Select a p-value threshold", min = 0, 
                       max = 1, value = 0.05, width = "400px")
      )
    }
    
  })
  output$hw = DT::renderDataTable({
    if (input$inputMode != "Interactive table") {
      validate(need(!is.null(input$inputfile), FALSE))
    }
    validate(
      need(nrow(datatoshow()) > 1, "You must introduce more than one study to perform a meta-analysis.")
    )
    
    # shinyjs::show("br3")
    # shinyjs::show("br4")
    # shinyjs::show("format")
    # shinyjs::show("downhw")
    # shinyjs::show("hr1")
    # shinyjs::show("thresholdHW")
    
    x = try(datatable(datahw(), rownames = F, escape = T, selection = "none", 
              options=list(paging=FALSE, searching=FALSE, autoWidth=F, columnDefs = list(list(class="dt-center", targets = "_all")))) %>% formatStyle("HW-adjusted.P.value", backgroundColor = styleInterval(thresholds$HW, c("tomato", "palegreen"))))
    if (class(x) == "try-error") {
      return(NULL)
    }
    else {
      return(x)
    }

    })
  
  thresholds = reactiveValues(HW = 0.05, ASSO = 0.05)
  
  observe ({
    invalidateLater(3000, session)
    isolate(thresholds$HW <- input$thresholdHW)
    isolate(thresholds$ASSO <- input$thresholdASSO)
  })
  
  
  output$buttonnext3 = renderUI({
    if (is.null(input$inputfile) & input$inputMode != "Interactive table") # No se debe mostrar nada si no se ha subido ningún archivo
      return(NULL)
    validate(
      need(try(HW(datatocalc(), columns())), FALSE)
    )
    validate(
      need(nrow(datatoshow()) > 1, FALSE)
    )
    actionButton("next3", "NEXT")
    
  })
  
  
  
  
  ## Se guarda el formato de descarga seleccionado por el usuario
  
  formatdown = reactive({
    switch(input$format,
           "Excel format (.xls)" = "xls",
           "Plain-text format (.csv)" = "csv",
           "Plain-text format (.tsv)" = "tsv")
    })
  
  sepdown = reactive({
    switch(input$format,
           "Excel format (.xls)" = "xls",
           "Plain-text format (.csv)" = ",",
           "Plain-text format (.tsv)" = "\t")
  })
  
  # Se crea la tabla con HWE a descargar
  output$downhw <- downloadHandler(
    filename = function() { 
      paste("HWTable.", formatdown(), sep='') 
    },
    content = function(file) {
      if (formatdown() == "xls") {
        WriteXLS(datahw(), file)
      }
      else {
        data=write.table(datahw(), file, sep = sepdown(), quote=FALSE, row.names=FALSE)
      }}
  )
  
  ########################
  ## Association values ##
  ########################
  
  ## Se crea la tabla de heterogeneicidad entre estudios
  # Primero se guarda el número de columnas y el modelo que se quiere analizar
  
  columns = reactive({
    cols = (ncol(datatoshow()) - 6)
    return(cols)
  })
  
  models = reactive({
    switch(input$methods,
           "allele" = "allele",
           "recessive" = "recessive", 
           "dominant" = "dominant",
           "overdominant" = "overdominant",
           "pairw1" = "pairw1",
           "pairw2" = "pairw2",
           "pairw3" = "pairw3")
  })
  

  # Se crean 2 metaresults para que solo se muestre el mensaje de error una vez
  metaresults = reactive({
    validate(
      need(nrow(datatoshow()) > 1, "You must introduce more than one study to perform a meta-analysis.")
    )
    validate(
      need(try(writemeta(datatocalcfinal(), models(), columns())), "Something has failed. Please, check your data input.")
    )
    writemeta(datatocalcfinal(), models(), columns())
  })
  
  metaresults2 = reactive({
    validate(
      need(nrow(datatoshow()) > 1, FALSE)
    )
    validate(
      need(try(writemeta(datatocalcfinal(), models(), columns())), "")
    )
    writemeta(datatocalcfinal(), models(), columns())
  })
  
  # Ahora, se crean los los resultados para el modelo seleccionado
  output$metaresults1 = renderText({
    metaresults()[[1]]
  })
  output$metaresults2 = renderTable({
    metaresults2()[[2]][,-4]
  }, bordered = T, align="c", striped = T)
  
  output$metanuevo = renderTable({
    metaresults2()[[12]]
  }, bordered = T, align="c", striped = T)
  
  output$metaresults3 = renderText({
    metaresults2()[[3]]
  })
#   output$metaresults4 = renderText({
#     metaresults2()[[4]]
#   })
#   output$metaresults5 = renderText({
#     metaresults2()[[5]]
#   })
#   output$metaresults6 = renderText({
#     metaresults2()[[6]]
#   })
#   output$metaresults7 = renderText({
#     metaresults2()[[7]]
#   })
#   output$metaresults8 = renderTable({
#     metaresults2()[[8]]
#   }, bordered = T, align="c", striped = T)
  output$metaresults9 = renderText({
    metaresults2()[[9]]
  })
  output$metaresults10 = renderText({
    metaresults2()[[10]]
  })
  output$metaresults11 = renderText({
    metaresults2()[[11]]
  })
  
  shinyjs::hide("hide2")
  shinyjs::hide("hide3")
  
  output$buttonnext4 = renderUI({
    metaresults2()
    shinyjs::hide("hide1", anim = T, animType = "fade", time = 1)
    shinyjs::show("hide2")
    shinyjs::show("hide3")
    list(br(), br(), actionButton("next4", "NEXT"))
  })
  
  
  
  ##################
  ## Forest plots ##
  ##################
  
  ## Se selecciona el modelo para hacer el forest plot
  modelsf = reactive({
    switch(input$methodsf,
           "allele" = "allele",
           "recessive" = "recessive", 
           "dominant" = "dominant",
           "overdominant" = "overdominant",
           "pairw1" = "pairw1",
           "pairw2" = "pairw2",
           "pairw3" = "pairw3")
  })
  
  height = reactive({
    dimension = (nrow(datatocalc())*15+200)
    return(dimension)
  })
  
  output$forest = renderPlot({
    validate(
      need(nrow(datatoshow()) > 1, "You must introduce more than one study to perform a meta-analysis.")
    )
    forestplot(datatocalcfinal(), modelsf(), columns(), input$fixRan)
  }, res=72, width = 800, height = height)
  
  output$downforest = downloadHandler(
    filename="Forest_plot.png",
    content=function(file) {
      altura = height()*2200/800
      png(file, 2200, altura, res=200)
      forestplot(datatocalcfinal(), modelsf(), columns(), input$fixRan)
      dev.off()
      }
    )
  
  output$buttonnext5 = renderUI({
    datatocalcfinal()
    validate(
      need(nrow(datatoshow()) > 1, FALSE)
    )
    list(br(), br(), actionButton("next5", "NEXT"))
  })
  ######################
  ## Publication Bias ##
  ######################
  
  # Modelo seleccionado
  modelsfun = reactive({
    switch(input$methodsfun,
           "allele" = "allele",
           "recessive" = "recessive", 
           "dominant" = "dominant",
           "overdominant" = "overdominant",
           "pairw1" = "pairw1",
           "pairw2" = "pairw2",
           "pairw3" = "pairw3")
  })
  
  # Funnel plot
  output$funnel = renderPlot({
    validate(
      need(nrow(datatoshow()) > 1, "You must introduce more than one study to perform a meta-analysis.")
    )
    funnelplot(datatocalcfinal(), modelsfun(), columns(), input$funLab)
  })
  
  output$downfunnel = downloadHandler(
    filename="Funnel_plot.png",
    content=function(file) {
      anchura = 800*2200/800
      altura = 600*2200/800
      png(file, anchura, altura, res=200)
      funnelplot(datatocalcfinal(), modelsfun(), columns(), input$funLab)
      dev.off()
    }
  )
  
  # Egger's test
  eggertest = reactive({
    validate(
      need(try(egger(datatocalcfinal(), modelsfun(), columns())), FALSE)
    )
    validate(
      need(nrow(datatoshow()) > 1, FALSE)
    )
    egger(datatocalcfinal(), modelsfun(), columns())
  })
  
#   output$egger1 = renderText({
#     eggertest()[[1]]
#   })
#   
#   output$egger2 = renderText({
#     eggertest()[[2]]
#   })
  
  output$egger3 = renderText({
    eggertest()[[1]]
  })
  
  output$buttonnext6 = renderUI({
    datatocalcfinal()
    validate(
      need(nrow(datatoshow()) > 1, FALSE)
    )
    list(br(), br(), actionButton("next6", "NEXT"))
  })
  
  
  
  ####################
  ## Subgroup analysis ##
  ####################
  
  # output$col = renderUI({
  #   if (is.null(input$inputfile) & input$inputMode != "Interactive table") # No se debe mostrar nada si no se ha subido ningún archivo
  #     return(NULL)
  #   shinyjs::delay(500, shinyjs::show("stratification"))
  #   selectInput('strcol', 'Choose the column for subgrouping data:',
  #               c("No subgrouping (results summary)",
  #                 colnames(datatocalcfinal())[1:(ncol(datatocalcfinal())-6)][-1]), width=500)
  # })
  
  
  ## Se crea la tabla con los resultados de los estudios estratificados
  datastr = function(){
    req(input$strcol)
    validate(
      need(nrow(datatoshow()) > 1, "You must introduce more than one study to perform a meta-analysis.")
    )
    validate(
      need(try(results_grouped(datatocalcfinal(), columns(), input$strcol)), "Something has failed. Please, check your data input.")
    )
    results_grouped(datatocalcfinal(), columns(), input$strcol)

  }
  
  ## Se muestra la tabla con los resultados
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Model'),
        # th(rowspan = 2, input$strcol),
        th(rowspan = 2, "Group"),
        th(rowspan = 2, 'Number of studies'),
        th(colspan = 3, 'Association test'),
        th(colspan = 3, 'Heterogeneity'),
        th('Publication bias')
        
      ),
      tr(
        lapply(c('OR', '95% CI', "p-val", "Model", "p-val", "I^2", "Egger's test p-val"), th)
      )
    )
  ))
  
  # shinyjs::hide("stratification")
  # shinyjs::hide("col")
  
  # observe({
  #   if (input$navbar == "Subgroup analysis"){
  #     shinyjs::show("col")
  #   }
  # })
  
  output$straPanel = renderUI({
    if (input$inputMode != "Interactive table") {
      req(input$inputfile)
      validate(
        need(nrow(datatoshow()) > 1, "You must introduce more than one study to perform a meta-analysis.")
      )
      div(id="straPanel",
          selectInput('strcol', 'Choose the column for subgrouping data:',
                      c("No subgrouping (results summary)",
                        colnames(datatocalcfinal())[1:(ncol(datatocalcfinal())-6)][-1]), width=500),
          br(),
          br(),
          selectInput("strformat", "Choose a format to download your data:", 
                      choices = c("Excel format (.xls)", "Plain-text format (.csv)", "Plain-text format (.tsv)"), width = 200),
          downloadButton('downstr', 'Download'),              
          tags$hr(),
          sliderInput("thresholdASSO", label = "Select a p-value threshold", min = 0, 
                      max = 1, value = 0.05, width = "400px"),

          DT::dataTableOutput("stratification")
          )
      
    }
    else {
      validate(
        need(nrow(datatoshow()) > 1, "You must introduce more than one study to perform a meta-analysis.")
      )
      div(id="straPanel",
          selectInput('strcol', 'Choose the column for subgrouping data:',
                      c("No subgrouping (results summary)",
                        colnames(datatocalcfinal())[1:(ncol(datatocalcfinal())-6)][-1]), width=500),
          br(),
          br(),
          selectInput("strformat", "Choose a format to download your data:", 
                      choices = c("Excel format (.xls)", "Plain-text format (.csv)", "Plain-text format (.tsv)"), width = 200),
          downloadButton('downstr', 'Download'),              
          tags$hr(),
          sliderInput("thresholdASSO", label = "Select a p-value threshold", min = 0, 
                      max = 1, value = 0.05, width = "400px"),

          DT::dataTableOutput("stratification")
          )
      
      
    }
  })
  

  output$stratification = DT::renderDataTable({
    # if (input$inputMode != "Interactive table") {
    #   validate(need(!is.null(input$inputfile), FALSE))
    # }
    # shinyjs::show("straPanel")
    req(input$strcol, input$thresholdASSO)
    
    straData = datastr()[-1,-1]
    nlines = nrow(straData)
    firstColumn = rep("", nlines)
    straData = data.frame(firstColumn, straData, stringsAsFactors = F)
    straData[1,1] = alle()
    straData[(nlines/7+1),1] = rece()
    straData[(nlines/7*2+1),1] = domi()
    straData[(nlines/7*3+1),1] = over()
    straData[(nlines/7*4+1),1] = pairw1()
    straData[(nlines/7*5+1),1] = pairw2()
    straData[(nlines/7*6+1),1] = pairw3()
    


    x = try(datatable(straData, rownames = F, escape = F, selection = "none", container = sketch,
              options=list(initComplete = JS(
                "function(settings, json) {",
                "var headerBorder = [2,3,4];",
                "var headerBorder2 = [2,5];",
                "var header = $(this.api().table().header()).find('tr:first > th').filter(function(index) {return $.inArray(index,headerBorder) > -1 ;}).addClass('cell-border-right');",
                "var header = $(this.api().table().header()).find('tr:eq(1) > th').filter(function(index) {return $.inArray(index,headerBorder2) > -1 ;}).addClass('cell-border-right');",
                "}"),paging=FALSE, searching=FALSE, autoWidth=F, columnDefs = list(list(class="dt-center", targets = "_all"),
                                                                                         list(orderable=FALSE, targets = "_all"),
                                                                                         list(className="dt-right cell-border-right",targets = "_all")))) %>% formatStyle(6, backgroundColor = styleInterval(thresholds$ASSO, c("palegreen", "tomato"))))
    if (class(x) == "try-error") {
      return(NULL)
    }
    else {
      return(x)
    }
    }
  )
  
  ## Se guarda el formato de descarga seleccionado por el usuario
  
  formatdownstr = reactive({
    switch(input$strformat,
           "Excel format (.xls)" = "xls",
           "Plain-text format (.csv)" = "csv",
           "Plain-text format (.tsv)" = "tsv")
  })
  
  sepdownstr = reactive({
    switch(input$strformat,
           "Excel format (.xls)" = "xls",
           "Plain-text format (.csv)" = ",",
           "Plain-text format (.tsv)" = "\t")
  })

  
  # Se crea la tabla a descargar
  output$downstr <- downloadHandler(
    filename = function() { 
      paste("SubgroupTable.", formatdownstr(), sep='') 
    },
    content = function(file) {
      if (formatdownstr() == "xls") {
        WriteXLS(datastr(), file)
      }
      else {
        data=write.table(datastr(), file, sep = sepdownstr(), quote=FALSE, row.names=FALSE)
      }}
  )
  
  output$buttonnext7 = renderUI({
    datatocalcfinal()
    validate(
      need(nrow(datatoshow()) > 1, FALSE)
    )
    list(br(), br(), actionButton("next7", "NEXT"))
  })
  
  ###########################
  ### Robustness analysis ###
  ###########################
  modelsrob = reactive({
      switch(input$methodsrob,
             "allele" = "allele",
             "recessive" = "recessive", 
             "dominant" = "dominant",
             "overdominant" = "overdominant",
             "pairw1" = "pairw1",
             "pairw2" = "pairw2",
             "pairw3" = "pairw3")
    
  })
  
  output$downrobPlot = downloadHandler(
    filename="Sensitivity_plot.png",
    content=function(file) {
      anchura = 800*2200/800
      altura = 600*2200/800
      png(file, anchura, altura, res=200)
      robustPlot(datatocalcfinal(), modelsrob(), columns(), input$fixRanRob)
      dev.off()
    }
  )

  output$robplot = renderPlot({
    validate(
      need(nrow(datatoshow()) > 1, "You must introduce more than one study to perform a meta-analysis.")
    )
    robustPlot(datatocalcfinal(), modelsrob(), columns(), input$fixRanRob)
  }, res=72, width = 800, height = height)
  
  
  robtest = reactive({
    validate(
      need(nrow(datatoshow()) > 1, FALSE)
    )
    robustness(datatocalcfinal(), modelsrob(), columns(), input$fixRanRob)
  })
  
  output$robtable= DT::renderDataTable({
    datatable(robtest(), rownames = F, escape = T, selection = "none",
              options=list(paging=FALSE, searching=FALSE, autoWidth=F, columnDefs = list(list(class="dt-center", targets = "_all"))))
  })
  
  
  formatdownrob = reactive({
    switch(input$robformat,
           "Excel format (.xls)" = "xls",
           "Plain-text format (.csv)" = "csv",
           "Plain-text format (.tsv)" = "tsv")
  })
  
  sepdownrob = reactive({
    switch(input$robformat,
           "Excel format (.xls)" = "xls",
           "Plain-text format (.csv)" = ",",
           "Plain-text format (.tsv)" = "\t")
  })
  
  output$downrobTable <- downloadHandler(
    filename = function() { 
      paste("Sensitivity_Table.", formatdownrob(), sep='') 
    },
    content = function(file) {
      if (formatdownrob() == "xls") {
        WriteXLS(robtest(), file)
      }
      else {
        data=write.table(robtest(), file, sep = sepdownrob(), quote=FALSE, row.names=FALSE)
      }}
  )
  

  
  
#   output$downforest = downloadHandler(
#     filename="Forest_plot.png",
#     content=function(file) {
#       altura = height()*2200/800
#       png(file, 2200, altura, res=200)
#       forestplot(datatocalcfinal(), modelsf(), columns(), input$fixRan)
#       dev.off()
#     }
#   )
})
