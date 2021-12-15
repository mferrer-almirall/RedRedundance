
# observe_helpers(help_dir = "help_mds")

##
output$fileInputs=renderUI({
  html_ui = " "
  for (i in 1:input$ncomps){
    html_ui <- paste0(html_ui, 
                        
                      h3(paste0("Comparison ", i)), br(),
                             textInput(paste0("CompName",i), "Name of comparison:", value=paste0("Comp",i)),
                      conditionalPanel(condition=("input.pathwDB=='GO' || input.pathwDB=='GO-Reactome'"), fileInput(paste0("GO_comp",i), label=paste0("Upload GO terms"))), 
                      conditionalPanel(condition=("input.pathwDB=='Reactome' || input.pathwDB=='GO-Reactome'"), fileInput(paste0("Reac_comp",i), label=paste0("Upload Reactome terms"))),
                      hr()
                      )
                      
  }
  HTML(html_ui)
})

output$fileInputs2=renderUI({
  html_ui2 = " "
  for (i in 1:input$ncomps){
    html_ui2 <- paste0(html_ui2, 
                       h3(paste0("Comparison ", i)), br(),
                       if (input$pathwDB=='GO'){ 
                                        tabsetPanel(
                         tabPanel("GO terms", DT::dataTableOutput(paste0("gotab",i))))},
                       if (input$pathwDB=='Reactome'){
                                        tabsetPanel(
                                          tabPanel("Reactome terms", DT::dataTableOutput(paste0("reactab",i))))},
                       if (input$pathwDB=='GO-Reactome'){
                                        tabsetPanel(
                                          tabPanel("GO terms", DT::dataTableOutput(paste0("gotab",i))),
                                        tabPanel("Reactome terms", DT::dataTableOutput(paste0("reactab",i))))},
                      hr()
                         )
  }
  HTML(html_ui2)
})

compnames <- reactive({
 sapply(1:input$ncomps, function(i){
  input[[paste0("CompName",i)]]
  })
})

output$compnames1 <- renderText({compnames()})

rr_read_data <- function(f, f_ext, f_sep, f_sheet,  f_dec) {
  if (f_ext=="xls/xlsx") {
    data <- read_excel(f, sheet=f_sheet)
    data <- data.frame(data, stringsAsFactors=FALSE, check.names=FALSE)
  }
  if (f_ext=="csv") {
    data <- read.csv(f, stringsAsFactors=FALSE, header=TRUE, sep=f_sep, dec=f_dec)
  }
  if (f_ext=="txt") {
   data <- read.csv(f, stringsAsFactors=FALSE, header=TRUE, sep="\t", dec=f_dec)
  }
  return(data)
}

pathwDB1 <- reactive({
  unlist(strsplit(input$pathwDB, "-"))
})

gofiles <- reactive({
  req(input$pathwDB=="GO" || input$pathwDB=="GO-Reactome")
      listgo <- lapply(1:input$ncomps, function(i){
      gofile <-input[[paste0("GO_comp",i)]]
      if (!is.null(gofile)) {
        godata <- rr_read_data(gofile$datapath, input$file_ext, input$file_sep, input$file_sheet, input$file_dec)
        return(godata)
      }
      })
      names(listgo) <- compnames()
      return(listgo)
    })

observe({
  req(input$pathwDB=="GO" || input$pathwDB=="GO-Reactome")
  lapply(1:length(gofiles()), function(i){
  dat <- data.frame(gofiles()[[i]])
  output[[paste0("gotab",i)]] <- DT::renderDataTable({DT::datatable(dat, options=list(pageLength=5, scrollX=TRUE))})
})
})

reacfiles <- reactive({
  req(input$pathwDB=="Reactome" || input$pathwDB=="GO-Reactome")
  listreac <- lapply(1:input$ncomps, function(i){
    reacfile <-input[[paste0("Reac_comp",i)]]
    if (!is.null(reacfile)) {
      reacdata <- rr_read_data(reacfile$datapath, input$file_ext, input$file_sep, input$file_sheet, input$file_dec)
      return(reacdata)
    }
  })
  names(listreac) <- compnames()
  return(listreac)
})

observe({
  req(input$pathwDB=="Reactome" || input$pathwDB=="GO-Reactome")
  lapply(1:length(reacfiles()), function(i){
  reacdat <- data.frame(reacfiles()[[i]])
  output[[paste0("reactab",i)]] <- DT::renderDataTable({DT::datatable(reacdat, options=list(pageLength=5, scrollX=TRUE))})
})
})

listofres.or.db <- reactive({
  if (input$pathwDB=="GO") listofres.or.db <- list(GO=gofiles())
  if (input$pathwDB=="Reactome") listofres.or.db <- list(Reactome=reacfiles())
  if (input$pathwDB=="GO-Reactome") listofres.or.db <- list(GO=gofiles(), Reactome=reacfiles())
  return(listofres.or.db)
})
