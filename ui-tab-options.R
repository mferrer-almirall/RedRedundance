orgdb <- c(Anopheles="org.Ag.eg.db",
           Arabidopsis="org.At.tair.db",
           Bovine="org.Bt.eg.db",
           Worm="org.Ce.eg.db",
           Canine="org.Cf.eg.db",
           Fly="org.Dm.eg.db",
           Zebrafish="org.Dr.eg.db",
           `E coli strain K12`="org.EcK12.eg.db",
           `E coli strain Sakai`="org.EcSakai.eg.db",
           Chicken="org.Gg.eg.db",
           Human="org.Hs.eg.db",
           Mouse="org.Mm.eg.db",
           Rhesus="org.Mmu.eg.db",
           Malaria="org.Pf.plasmo.db",
           Chimp="org.Pt.eg.db",
           Rat="org.Rn.eg.db",
           Yeast="org.Sc.sgd.db",
           Pig="org.Ss.eg.db",
           Xenopus="org.Xl.eg.db")

fluidRow(
  
  column(width = 3,
         box(width=NULL, title="General parameters", status="warning",
             selectInput("organism_annot", label="Organism", selected="org.Hs.eg.db",
                         choices=orgdb[order(names(orgdb))]),
             radioButtons("pathwDB", "Source of terms:",
                          choices = c("GO" = 'GO',
                                      "Reactome" = 'Reactome',
                                      "GO and Reactome" = 'GO-Reactome'),
                          selected = 'GO'),
             textInput("col_ID", "Name of column containing GO/Reactome id:", value="ID"),
             textInput("label_FN", "Label", value="RedRedundance_Results")
             ),
         box(width=NULL, title="Grouping parameters", status="warning",
             conditionalPanel(condition= ("input.pathwDB=='GO'"), selectInput("dist_method1", label="Distance measure (GO only)", selected="Rel",
                         choices=c("Resnik", "Lin", "Rel", "Jiang", "Wang"))),
             conditionalPanel(condition= ("input.pathwDB=='Reactome' || input.pathwDB=='GO-Reactome'"), selectInput("dist_method2", label="Distance measure", selected="jaccard",
                                                                              choices=c("kappa", "jaccard", "dice", "overlap"))),
             conditionalPanel(condition= ("input.pathwDB=='GO'"), a("Help",
               href="https://www.bioconductor.org/packages/release/bioc/vignettes/GOSemSim/inst/doc/GOSemSim.html#semantic-similarity-measurement-based-on-go",
               target="_blank")),
             selectInput("clust_method", label="Clustering method", selected="fast_greedy",
                         choices=c("kmeans", "MCL", "binary_cut","fast_greedy","dynamicTreeCut", "apcluster")),
             checkboxInput("do_clust_comp", "Compare different clustering methods", FALSE),
             conditionalPanel(condition= ("input.do_clust_comp"), selectInput("comp_clust_methods", label="Select the clustering methods to compare:", selected=c("binary_cut", "MCL", "kmeans", "fast_greedy", "dynamicTreeCut", "apcluster"),
                                                                              choices= c("binary_cut", "MCL", "kmeans", "fast_greedy", "dynamicTreeCut", "apcluster"), multiple=TRUE))
         ),
         box(width=NULL, title="Plot Options", status="warning",
             numericInput("min_term", label="Minimum size of cluster names in heatmap", value=5, min=1),
             numericInput("num_words_wc", label="Number of words for Word Cloud annotation", value=10, min=1),
             textInput("col_val", label="Numeric column to highlight on left heatmap", value="none")
         ),
         
  )
)

