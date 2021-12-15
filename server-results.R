
# observe_helpers(help_dir = "help_mds")

observe({
  library(input$organism_annot, character.only=TRUE)
})


##
ggplotColours <- function(n = 6, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

#llista de partida segons si s'ha filtrat o no
# listofres.db <- reactive({
#   if (!base::exists(listofres.db())) listofres.or.db()
# })

#listofres ajuntant les llistes per pathwDB dins de cada comparacio (retorna llista de comparacions)
cat_lists <- function(list1, list2) {  
  keys <- unique(c(names(list1), names(list2)))
  map2(list1[keys], list2[keys], bind_rows) %>% 
    set_names(keys)  
  
}

listofres <- reactive({
  purrr::reduce(listofres.db(), cat_lists)
})

#listofres ajuntant les llistes per comparacions dins de cada pathwDB (retorna llista de pathwDB)
listofres.db1 <- reactive({
  lapply(listofres.db(), function(l) do.call(bind_rows,l))
})

#dataframe d'anotacions (cada terme amb els gens q conte). Per combinat de dB, agafem els genes de cada database corresponent
organism_sp_all <- c(org.Ag.eg.db="Anopheles gambiae",
                  org.At.tair.db="Arabidopsis thaliana",
                  org.Bt.eg.db="Bos taurus",
                  org.Ce.eg.db="Caenorhabditis elegans",
                  org.Cf.eg.db="Canis familiaris",
                  org.Dm.eg.db="Drosophila melanogaster",
                  org.Dr.eg.db="Danio rerio",
                  org.Gg.eg.db="Gallus gallus domesticus",
                  org.Hs.eg.db="Homo sapiens",
                  org.Mm.eg.db="Mus musculus",
                  org.Rn.eg.db="Rattus norvegicus",
                  org.Sc.sgd.db="Saccharomyces cerevisiae",
                  org.Ss.eg.db="Sus domesticus",
                  org.Xl.eg.db="Xenopus laevis")

db_annot_list <- reactive({
  withProgress(message="Preparing annotations...", value=0, {
  all_terms.db <- lapply(listofres.db1(), function(x) unique(x[,input$col_ID]))
  db_annot_list <- lapply(names(listofres.db1()), function(p){
  termids <- all_terms.db[[p]]
  if (p=="GO"){
    db_genes <- AnnotationDbi::select(eval(parse(text=input$organism_annot)),
                                      keys=termids,
                                      keytype="GOALL",
                                      column=c("ENTREZID"),
                                      multivals="first")
    db_terms <- AnnotationDbi::select(GO.db::GO.db,
                                      keys=termids,
                                      keytype="GOID",
                                      column=c("TERM"),
                                      multivals="first")
    db_annot_df <- db_genes %>%
      left_join(db_terms, by=c("GOALL"="GOID"))%>%
      group_by(GOALL, TERM) %>%
      summarise(genes=paste0(ENTREZID, collapse="/"), setSize=n()) %>%
      dplyr::rename(term="TERM", id="GOALL")
  } else if (p=="Reactome") {
    db_annot <- AnnotationDbi::select(reactome.db,
                                      keys=termids,
                                      keytype="PATHID",
                                      column=c("PATHNAME", "ENTREZID"),
                                      multivals="first")
    organism <- organism_sp_all[[input$organism_annot]]
    db_annot_df <- db_annot %>%
      group_by(PATHID, PATHNAME) %>%
      summarise(genes=paste0(ENTREZID, collapse="/"), setSize=n()) %>%
      mutate(PATHNAME=gsub(paste0("^",organism, ": "), "", PATHNAME)) %>%
      dplyr::rename(term="PATHNAME", id="PATHID")
  }
  return(db_annot_df)
})
names(db_annot_list) <- names(listofres.db1())
return(db_annot_list)
})
})

db_annot_df1_withnas <- reactive({
  db_annot_df1_withnas <- do.call(bind_rows,db_annot_list())
  db_annot_df1_withnas[db_annot_df1_withnas=="NA"] <- NA
  return(db_annot_df1_withnas)
  })

db_annot_df1 <- reactive({db_annot_df1_withnas()[!(is.na(db_annot_df1_withnas()$term)|is.na(db_annot_df1_withnas()$genes)),]})
 
#terms not found in databases
output$db_annot_df1_nas_id <- renderText({db_annot_df1_withnas()[is.na(db_annot_df1_withnas()$term)|is.na(db_annot_df1_withnas()$genes),]$id})

#Similarity matrix
mat <- reactive({
  withProgress(message="Calculating similarity matrix, this may take a while...", value=0, {
  all_terms_list <- strsplit(db_annot_df1()$genes, "/")
  names(all_terms_list) <- db_annot_df1()$id
  all_terms.f <- names(all_terms_list)
  if (input$pathwDB=="GO"){
    mat <- simplifyEnrichment::GO_similarity(all_terms.f, db=input$organism_annot, measure = input$dist_method)
  } else if (input$pathwDB=="Reactome"){
    mat <- simplifyEnrichment::term_similarity_from_Reactome(all_terms.f, method = input$dist_method)
  } else {
    # A list of gene sets where each gene set contains a vector of genes.
    mat <- simplifyEnrichment::term_similarity(all_terms_list, method = input$dist_method)
  }
  mat <- mat[!is.na(rownames(mat)),!is.na(colnames(mat))]
  dim(mat)
  return(mat)
})
})

output$mat <- DT::renderDataTable({DT::datatable(mat())})


#Heatmap esquerre
## llista de comparacions amb named vector with values corresponding to values to represent in left heatmap
lt <- reactive({
  lt <- lapply(listofres(), function(x) {
  if(input$col_val=="none") v <- rep(1, nrow(x)) else {v <- x[,input$col_val]}
  names(v) <- as.character(x[, input$col_ID])
  return(v)
})
  return(lt)
})

ht <- reactive({
  req(length(lt())>1)
  withProgress(message="Calculating left heatmap...", value=0, {
  all_terms <- unique(unlist(lapply(lt(), names)))
  n <- length(lt())
  init <- ifelse(input$col_val=="none", 0, 1)
  m = matrix(init, nrow = length(all_terms), ncol = n)
  rownames(m) = all_terms
  colnames(m) = names(lt())

  ##omplim amb pvalors o presencia/absencia
  for (i in 1:n) {
    m[names(lt()[[i]]), i] = lt()[[i]]
  }
  m <- m[rownames(mat()),, drop=FALSE]
  if(input$col_val=="none" | input$col_val=="NES") transform <- function(x) x else transform <- function(x) -log10(x)
  m = t(apply(m, 1, transform))

  if(input$col_val=="none") {
    if (length(lt())<2) {
      heatmap_param = list(
        breaks = c(1), at=c(1), col = c("darkgreen"),
        name = "", labels = c("available"), title="")
      
    } else {
    heatmap_param = list(
      breaks = c(0, 1), at=c(0,1), col = c("gray", "darkgreen"),
      name = "", labels = c("not available", "available"), title="")
    }
  } else if (input$col_val=="NES"){
    heatmap_param = list(
      title=input$col_val, col=c("gray", "darkgreen"))
  } else {
    heatmap_param = list(
      title=paste0("-log10(",input$col_val,")"), col=c("gray", "darkgreen"))
  }
  ht <- ComplexHeatmap::Heatmap(m,show_row_names = FALSE, cluster_columns = FALSE,
                              border = "black", col=heatmap_param$col, heatmap_legend_param = heatmap_param,
                              width = unit(0.5, "cm") * n, use_raster = TRUE)
  return(ht)
})
})

output$ht <- renderPlot({ht()})

# Comparison of clustering methods

observe({
  if (input$do_clust_comp){
    withProgress(message="Performing cluster comparison...", value=0, {
    set.seed(123)
    output$plot_clust_comparison1 <- renderPlot({simplifyEnrichment::compare_clustering_methods(mat(), method=input$comp_clust_methods)})
    output$plot_clust_comparison2 <- renderPlot({simplifyEnrichment::compare_clustering_methods(mat(), method=input$comp_clust_methods, plot_type ="heatmap", nrow=3)})
    })
}
})

#Main Heatmap with wordCloud annotation
cl <- reactive({
  simplifyEnrichment::cluster_terms(mat(), method = input$clust_method, control = list(), catch_error = FALSE, verbose = TRUE)
  })

hm <- reactive({
  if (is.null(input$min_term)) min_term = round(nrow(mat()) * 0.02) else min_term=input$min_term
  term_desc <- structure(as.character(db_annot_df1()$term), names = as.character(db_annot_df1()$id)) #named vector
  if (is.null(ht())){
    simplifyEnrichment::ht_clusters(mat(), cl(), draw_word_cloud=TRUE, word_cloud_grob_param = list(max_width = 80), term=term_desc,
                                    order_by_size = TRUE, min_term=min_term)
  } else { 
    simplifyEnrichment::ht_clusters(mat(), cl(), draw_word_cloud=TRUE, word_cloud_grob_param = list(max_width = 80), term=term_desc,
                                  order_by_size = TRUE, ht_list=ht(), min_term=min_term)
  }
})
output$hm <- renderPlot({hm()})

#fem el dataframe
df <- reactive({
  term_desc <- structure(as.character(db_annot_df1()$term), names = as.character(db_annot_df1()$id)) #named vector
  data.frame(id = rownames(mat()), term = term_desc, cluster = cl(), stringsAsFactors = FALSE)
})
output$df <- DT::renderDataTable({DT::datatable(df(), options=list(pageLength=5, scrollX=TRUE))})