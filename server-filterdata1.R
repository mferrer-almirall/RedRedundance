
# observe_helpers(help_dir = "help_mds")

##

output$fileInputs2f=renderUI({
  html_ui2f = " "
  for (i in 1:input$ncomps){
    html_ui2f <- paste0(html_ui2f, 
                       h3(paste0("Comparison ", i)), br(),
                       if (input$pathwDB=='GO'){ 
                         tabsetPanel(
                           tabPanel("GO terms", DT::dataTableOutput(paste0("gotab.f",i))))},
                       if (input$pathwDB=='Reactome'){
                         tabsetPanel(
                           tabPanel("Reactome terms", DT::dataTableOutput(paste0("reactab.f",i))))},
                       if (input$pathwDB=='GO-Reactome'){
                         tabsetPanel(
                           tabPanel("GO terms", DT::dataTableOutput(paste0("gotab.f",i))),
                           tabPanel("Reactome terms", DT::dataTableOutput(paste0("reactab.f",i))))},
                       hr()
    )
  }
  HTML(html_ui2f)
})


rr_filter_data <- function(x) {
  if (is.null(x)) return(NULL) else {
    
  if (input$filter_pval) {
    x <- x[x[, input$col_pval] < input$pval_thr,]
    if (nrow(x)>0) x <- x[order(x[, input$col_pval]),]
    }
  if (input$filter_NES) {
    if (input$NES_dir=="up") x <- x[x[, input$col_NES] > input$NES_thr,] 
    if (input$NES_dir=="down") x <- x[x[, input$col_NES] < input$NES_thr,]
    if (input$NES_dir=="abs") x <- x[abs(x[, input$col_NES]) > abs(input$NES_thr),]
    if (nrow(x)>0) x <- x[order(abs(x[, input$col_NES]), decreasing=TRUE),]
  }
  if (nrow(x) > input$topN) x <- x[1:input$topN,]
  if (input$filter_ids) {
    ids_to_filt <- unlist(strsplit(x =input$ids_to_filt,split = '[\r\n]' ))
    x <- x[x$ID%in%ids_to_filt,]
  }
  return(x)
  }
}

listofres.db <- eventReactive(input$process,
                           ignoreNULL = TRUE, {
                             listofres.db <- lapply(names(listofres.or.db()), function(p) {
                               lapply(listofres.or.db()[[p]], rr_filter_data)
                             })
                             names(listofres.db) <- names(listofres.or.db())
                             return(listofres.db)
                           })

observe({
  if (input$pathwDB=="GO" || input$pathwDB=="GO-Reactome") {
    gofiles.f <- listofres.db()[["GO"]]
    lapply(1:length(gofiles.f), function(i){
      dat.f <- data.frame(gofiles.f[[i]])
      output[[paste0("gotab.f",i)]] <- DT::renderDataTable({DT::datatable(dat.f, options=list(pageLength=5, scrollX=TRUE))})
      })
  }
  if (input$pathwDB=="Reactome" || input$pathwDB=="GO-Reactome") {
   reacfiles.f <- listofres.db()[["Reactome"]]
    lapply(1:length(reacfiles.f), function(i){
      reacdat.f <- data.frame(reacfiles.f[[i]])
      output[[paste0("reactab.f",i)]] <- DT::renderDataTable({DT::datatable(reacdat.f, options=list(pageLength=5, scrollX=TRUE))})
    })
  }
})


