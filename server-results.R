
# observe_helpers(help_dir = "help_mds")

observe({
  # library(input$organism_annot, character.only=TRUE)
  if (!require(input$organism_annot, character.only=TRUE, quietly = TRUE)) {
    BiocManager::install(input$organism_annot, character.only=TRUE)
    require(input$organism_annot, character.only=TRUE)
  }
  
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
organism_sp_all <- c(org.Hs.eg.db="Homo sapiens",
                     org.Mm.eg.db="Mus musculus",
                     org.Rn.eg.db="Rattus norvegicus",
                     org.Cf.eg.db="Canis familiaris",
                     org.Ss.eg.db="Sus domesticus",
                     org.Gg.eg.db="Gallus gallus domesticus",
                     org.Ag.eg.db="Anopheles gambiae",
                  org.At.tair.db="Arabidopsis thaliana",
                  org.Bt.eg.db="Bos taurus",
                  org.Ce.eg.db="Caenorhabditis elegans",
                  org.Dm.eg.db="Drosophila melanogaster",
                  org.Dr.eg.db="Danio rerio",
                  org.Sc.sgd.db="Saccharomyces cerevisiae",
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
  names(m) = names(lt()) #millor names q colnames pq no doni problemes quan nomes hi ha una llista

  ##omplim amb pvalors o presencia/absencia
  for (i in 1:n) {
    m[names(lt()[[i]]), i] = lt()[[i]]
  }
  m <- m[rownames(mat()),, drop=FALSE]
  if(input$col_val=="none" | input$col_val=="NES") transform <- function(x) x else transform <- function(x) -log10(x)
  if (n>1) m = as.data.frame(t(apply(m, 1, transform)), stringsAsFactors=FALSE) else m=as.data.frame(transform(m), stringsAsFactors=FALSE)
  names(m) = names(lt())
  # if (n>1) m = t(apply(m, 1, transform)) else m[]=transform(m)

  if(input$col_val=="none") {
    # if (length(lt())<2) {
    #   heatmap_param = list(
    #     breaks = c(1), at=c(1), col = c("darkgreen"),
    #     name = "", labels = c("available"), title="")
    #   
    # } else {
    heatmap_param = list(
      breaks = c(0, 1), at=c(0,1), col = c("gray", "darkgreen"),
      name = "", labels = c("not available", "available"), title="")
    # }
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

#Clustering
cl <- reactive({
  simplifyEnrichment::cluster_terms(mat(), method = input$clust_method, control = list(), catch_error = FALSE, verbose = TRUE)
  })

#Main Heatmap with wordCloud annotation
hm <- reactive({
  withProgress(message="Performing Heatmap with WordCloud...", value=0, {
  if (is.null(input$min_term)) min_term = round(nrow(mat()) * 0.02) else min_term=input$min_term
  term_desc <- structure(as.character(db_annot_df1()$term), names = as.character(db_annot_df1()$id)) #named vector
  if (is.null(ht())){
     ht_clusters_1.1.5(mat(), cl(), draw_word_cloud=TRUE, word_cloud_grob_param = list(max_width = 80), term=term_desc,
                                    order_by_size = TRUE, min_term=min_term)
    
  } else { 
     ht_clusters_1.1.5(mat(), cl(), draw_word_cloud=TRUE, word_cloud_grob_param = list(max_width = 80), term=term_desc,
                                  order_by_size = TRUE, ht_list=ht(), min_term=min_term)
    
  }
})
})

output$hm <- renderPlot({hm()})

#fem el dataframe
df <- reactive({
  term_desc <- structure(as.character(db_annot_df1()$term), names = as.character(db_annot_df1()$id)) #named vector
  df <- data.frame(id = rownames(mat()), term = term_desc, cluster = cl(), stringsAsFactors = FALSE)
  df <- left_join(df, db_annot_df1()[,c("id", "setSize")], by="id")
  colnames(df) <- c("id", "term", "cluster", "setSizeDB")
  return(df)
})
output$df <- DT::renderDataTable({DT::datatable(df(), options=list(pageLength=5, scrollX=TRUE))})

#Main Heatmap with annotation 2
df1 <- reactive({
  if (is.null(input$min_term)) min_term = round(nrow(mat) * 0.02) else min_term=input$min_term
  df1 <- df()
  for (comp in names(lt())){
    res <- listofres()[[comp]]
    colnames(res)[-1] <- paste0( colnames(res)[-1], ".", comp)
    df1 <- left_join(df1, res, by=c("id"=input$col_ID))
  }
  clusters <- sort(unique(df1$cluster))
  
  df1$cluster_annot_wordcloud <- NA
  df1$cluster_annot_maxsetsize <- NA
  if (input$annot_type=="annot_minpval") df1$cluster_annot_minpval <- NA

  for (c in clusters){
    df1_c <- df1 %>%
      dplyr::filter(cluster==c)
    #anotacio amb wordcloud
    word_clouds_annotations <- simplifyEnrichment::count_words(df1_c$term)
    words_f <- na.omit(word_clouds_annotations$word[1:input$num_words_wc])
    df1[df1$cluster==c, "cluster_annot_wordcloud"] <- paste0(words_f, collapse=" ")
    #anotacio amb term with min pval in any of comparisons
    if (input$annot_type=="annot_minpval") {
      cols_pval <- colnames(df1_c)[grepl(paste0("^",input$col_pval), colnames(df1_c))]
      pval_min <- apply(df1_c[,cols_pval],1,min, na.rm=TRUE)
      names(pval_min) <- df1_c$id
      anot_minpval <- names(pval_min)[which.min(pval_min)]
      anot_minpval1 <- as.character(df1_c[df1_c$id==anot_minpval,"term"])
      df1[df1$cluster==c, "cluster_annot_minpval"] <- anot_minpval1
    }
    #anotacio amb terme de major setSize (sera probablement el mes generic)
    anot_maxsetsize1 <- as.character(df1_c[which.max(df1_c$setSizeDB),"term"])
    df1[df1$cluster==c, "cluster_annot_maxsetsize"] <- anot_maxsetsize1
  }
  if (input$annot_type!="none"){
    #ordre del heatmap per tamany de cluster
    order_by_size <- TRUE
    cl = as.vector(cl())
    cl_tb = table(cl)
    cl[as.character(cl) %in% names(cl_tb[cl_tb < min_term])] = 0
    cl = factor(cl, levels = c(setdiff(sort(cl), 0), 0))
    if (order_by_size) {
      cl = factor(cl, levels = c(setdiff(names(sort(table(cl),
                                                    decreasing = TRUE)), 0), 0))
    }
    cl_annot_show <- paste0("cluster_",input$annot_type)
    df1$cluster_show <- ifelse(df1$cluster%in%levels(cl),df1$cluster,0)
    df1$cluster_show_names <- ifelse(df1$cluster_show==0, "other", df1[,cl_annot_show])
  }
  #reordenem columnes
  cols_first <- c("id", "term", "cluster", "cluster_annot_wordcloud","cluster_annot_maxsetsize")
  if (input$annot_type=="annot_minpval") cols_first <- c(cols_first, "cluster_annot_minpval")
  if (input$annot_type!="none") cols_first <- c(cols_first, "cluster_show", "cluster_show_names")
  df1 <- df1[,c(cols_first, colnames(df1)[!colnames(df1)%in%cols_first])]
  df1 <- df1[order(df1$cluster),]
  return(df1)
})
output$df1 <- DT::renderDataTable({DT::datatable(df1(), options=list(pageLength=5, scrollX=TRUE))})

hm3 <- reactive({
  req(input$annot_type!="none")
  if (is.null(input$min_term)) min_term = round(nrow(mat) * 0.02) else min_term=input$min_term
  df1 <- df1()
  width_wrap <- 45
  #ordre del heatmap per tamany de cluster
  order_by_size <- TRUE
  cl = as.vector(cl())
  cl_tb = table(cl)
  cl[as.character(cl) %in% names(cl_tb[cl_tb < min_term])] = 0
  cl = factor(cl, levels = c(setdiff(sort(cl), 0), 0))
  if (order_by_size) {
    cl = factor(cl, levels = c(setdiff(names(sort(table(cl),
                                                  decreasing = TRUE)), 0), 0))
  }
  od2 = unlist(lapply(levels(cl), function(le) {
    l = cl == le
    if (sum(l) <= 1) {
      return(which(l))
    }
    else {
      mm = mat()[l, l, drop = FALSE]
      which(l)[hclust(stats::dist(mm))$order]
    }
  }))

  #Anotacions (hm dret)
  ann_col <- ggplotColours(nlevels(cl))
  names(ann_col) <- as.character(sort(unique(cl)))

  ann_cl <- df1$cluster_show_names
  ann_cl <- stringr::str_wrap(ann_cl, width=width_wrap)

  names(ann_cl) <- df1$cluster_show
  ann_cl <- ann_cl[!duplicated(ann_cl)]
  ann_cl <- ann_cl[names(ann_col)]

  #Heatmaps
  ##heatmap principal
  col <- c("white", "red")
  col_fun = circlize::colorRamp2(seq(0, quantile(mat(), 0.95), length = length(col)),
                                 col)
  hm1 = Heatmap(mat(), col = col_fun, name = "Similarity",
                column_title = "", show_row_names = FALSE,
                show_column_names = FALSE, show_row_dend = FALSE,
                show_column_dend = FALSE, row_order = od2, column_order = od2,
                border = "#404040", row_title = NULL, use_raster = TRUE) +
    NULL
  ##afegim heatmap anotacions dret
  hm2 <- hm1 + Heatmap(as.character(cl),
                       col = ann_col,
                       width = unit(0.5, "cm"), heatmap_legend_param = list(title = "ClusterName",at = names(ann_cl), labels = ann_cl, fontsize=5),
                       show_column_names = FALSE)

  ##afegim heatmap comparacions esquerre
  gap = unit(2, "pt")
  ht_list <- ht()
  if (!is.null(ht_list)) {
    n = length(ht_list)
    hm3 = ht_list + hm2
    gap = unit.c(unit(rep(1, n), "mm"), gap)
  }
  hm3@ht_list[[1]]@heatmap_param$post_fun = function(ht) {
    decorate_heatmap_body("Similarity", {
      grid.rect(gp = gpar(fill = NA, col = "#404040"))
      cl = factor(cl, levels = unique(cl[od2]))
      tbcl = table(cl)
      ncl = length(cl)
      x = cumsum(c(0, tbcl))/ncl
      grid.segments(x, 0, x, 1, default.units = "npc",
                    gp = gpar(col = "#404040"))
      grid.segments(0, 1 - x, 1, 1 - x, default.units = "npc",
                    gp = gpar(col = "#404040"))
    })
  }
  hm3 <- draw(hm3, main_heatmap = "Similarity", gap = gap)
  return(hm3)
})

output$hm3 <- renderPlot({hm3()})

#Dotplot clusters
##aggregate NES and pvalues by cluster in each comparison
agg_stats <- c("pvalue", "p.adjust")
stat_color <- reactive({input$col_pval})
size_label_dotplot <- 10
df_agg <- reactive({
  df_agg <- df1() %>%
  dplyr::select(cluster, starts_with(agg_stats)) %>%
  group_by(cluster) %>%
  dplyr::summarise_at(vars(starts_with(agg_stats)), list(agg=function(x) exp(mean(log(x), na.rm=TRUE)), numTerms= function(x) sum(complete.cases(x)))) %>%
  left_join(dplyr::select(df1(), starts_with("cluster")), by="cluster") %>%
  dplyr::select(c(starts_with("cluster"), ends_with("_agg"), starts_with(stat_color())&ends_with("_numTerms"))) %>%
  unique()
  colnames(df_agg) <- gsub("_agg", "__agg", colnames(df_agg))
  colnames(df_agg) <- gsub("_numTerms", "__numTerms", colnames(df_agg))
  return(df_agg)
})

#plot
clustname <- reactive({switch(input$annot_type, annot_maxsetsize="cluster_annot_maxsetsize", annot_minpval="cluster_annot_minpval", none="cluster_annot_maxsetsize")})
datgg_dotplot <- reactive({
  comps <- names(lt())
  datgg_dotplot <- df_agg() %>%
  dplyr::select(clustname(), starts_with(stat_color()), ends_with("_numTerms")) %>%
  pivot_longer(cols=!clustname(), names_to=c("comp", ".value"), names_sep="__", names_prefix=paste0(stat_color(), ".")) %>%
  # mutate(ClusterName=factor(cluster_show_names, levels=rev(ann_cl))) %>%
  mutate(comp=factor(comp, levels=comps), numTerms=ifelse(is.na(agg), NA, numTerms)) #sino els nas surten grisos
  return(datgg_dotplot)
} )
dp <- reactive({
  ggplot(datgg_dotplot(), aes_string(x = "comp", y = clustname())) + 
  geom_point(aes(size = numTerms, color = agg)) +
  theme_bw(base_size = size_label_dotplot) +
  scale_colour_gradient(limits=c(0, input$pval_thr), low="red") +
  scale_y_discrete(labels=function(x) stringr::str_wrap(x, width=45)) +
  labs(y=NULL, x=NULL, color=paste0("agg ", stat_color()), size="number of terms") +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=12), axis.text.y=element_text(size=10)) 
})

output$dp <- renderPlot({dp()})
output$df_agg <- DT::renderDataTable({DT::datatable(df_agg(), options=list(pageLength=5, scrollX=TRUE))})

#Enrichment map via igraph+visNetwork (clusterprofiler: necessita enrichResult object)

g <- reactive({
    igraph::graph_from_adjacency_matrix(mat(), mode="undirected", weighted=TRUE, diag=FALSE)
})
visdata <- reactive({
  visdata <- toVisNetworkData(g())
  visdata$edges$value <- visdata$edges$weight
  return(visdata)
  })
nodes <- reactive({
  withProgress(message="Calculating clustering...", value=0, {
    visdata <- visdata()
    nodes <- visdata$nodes
    cluster <- eval(parse(text=paste0(input$netw_clust_method,"(g())"))) #el weights es passa automatic si s'han calculat weights
    
    cluster_df <- data.frame(cluster_igraph=cluster$membership, id=cluster$name, stringsAsFactors=FALSE) 
    nodes <- nodes %>%
      left_join(.,cluster_df, by="id") %>%
      left_join(., df1(), by="id")
    nodes <- nodes %>%
      mutate(group=factor(cluster_igraph, levels=as.character(1:max(nodes$cluster_igraph)))) %>%
      arrange(group)
    return(nodes)
})
})
    
edges <- reactive({visdata()$edges})
 
output$netplot <- renderVisNetwork({
  withProgress(message="Rendering Network...", value=0, {
  visNetwork(nodes(), edges(), width = "100%") %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE,selectedBy = list(variable="group", multiple=TRUE)
               # manipulation = list(enabled = TRUE, editEdgeCols = c("weight"), editNodeCols = c("id", "term"))
    ) %>%
    visIgraphLayout(layout=input$netw_layout, physics=FALSE, smooth=FALSE, randomSeed = 123)%>%
    # visClusteringByGroup(groups = unique(nodes$group)) %>%
    visLegend(stepY=50)
})
})   
output$df_netw <- DT::renderDataTable({DT::datatable(nodes(), options=list(pageLength=5, scrollX=TRUE))})
