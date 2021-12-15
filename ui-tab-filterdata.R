fluidRow(
  
  column(width = 3,
         box(width=NULL, title="Filtering", status="warning",
             checkboxInput("filter_pval", "Filter terms by p-value", FALSE),
             conditionalPanel(condition=("input.filter_pval"), textInput("col_pval",label="Name of p-value/FDR column to filter by", value=NULL)),
             conditionalPanel(condition=("input.filter_pval"), numericInput("pval_thr",label="Threshold of p-value/FDR", value=0.05)),
             checkboxInput("filter_NES", "Filter terms by NES", FALSE),
             conditionalPanel(condition=("input.filter_NES"), textInput("col_NES",label="Name of NES column", value="NES")),
             conditionalPanel(condition=("input.filter_NES"), numericInput("NES_thr",label="Threshold:", value=0)),
             conditionalPanel(condition=("input.filter_NES"), selectInput("NES_dir",label="Direction:", selected="up", choices=c("up","down","abs"))),
             checkboxInput("filter_ids", "Filter terms by ID", FALSE),
             conditionalPanel(condition=("input.filter_ids"), textAreaInput("ids_to_filt",label="Paste the IDs to keep, one by line", value=NULL, height=300)),
             numericInput("topN", "Maximum number of terms to keep for each comparison:", value=100)),
         actionButton("process","Process", icon("step-forward"),
                      style="color: #fff; background-color: #00b300; border-color: #009900")
  ),
column(9,

         fluidRow(
           
           uiOutput("fileInputs2f")

         ),

         fluidRow(
           bsCollapse(

                      bsCollapsePanel(title = "", value = "proc_panel1", DT::dataTableOutput("contents_proc1"))

           )

         )
  )
)

