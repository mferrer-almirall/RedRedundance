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
                    textOutput("check"),br(),
                    DTOutput("mat")
                    ),
           tabPanel("Heatmap_left", plotOutput("ht")),
           tabPanel("Cluster comparison", plotOutput("plot_clust_comparison1"), plotOutput("plot_clust_comparison2")),
           tabPanel("Heatmap_WordCloud", plotOutput("hm"), DTOutput("df"))
         ),
         fluidRow(
           bsCollapse(

             bsCollapsePanel(title = "", value = "proc_panel3", DT::dataTableOutput("contents_proc3"))

           )

         )
  )
)

