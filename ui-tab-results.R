fluidRow(
  # column(width = 3,
  #        wellPanel(
  #          
  #          
  #        )),
  # 
  column(width = 9,

         tabsetPanel(

           tabPanel("Results",
                    h4("Terms not found in database:"),br(),
                    textOutput("db_annot_df1_nas_id"), br(),
                    h4("Similarity matrix:"),br(),
                    DTOutput("mat")
                    ),
           # tabPanel("Heatmap_left", plotOutput("ht")),
           tabPanel("Cluster comparison", plotOutput("plot_clust_comparison1"), plotOutput("plot_clust_comparison2")),
           tabPanel("Heatmap_WordCloud", plotOutput("hm"), DTOutput("df")),
           tabPanel("Heatmap_Annot",plotOutput("hm3"), DTOutput("df1")),
           tabPanel("Dotplot",plotOutput("dp"), DTOutput("df_agg")),
           tabPanel("Network",visNetworkOutput("netplot"), DTOutput("df_netw"))
         ),
         fluidRow(
           bsCollapse(

             bsCollapsePanel(title = "", value = "proc_panel3", DT::dataTableOutput("contents_proc3"))

           )

         )
  )
)

