fluidRow(
  
  column(width = 3,
           
           
           box(width=NULL,
               numericInput("ncomps", "Number of comparisons", value = 1, min = 1, step = 1),
               column(width=5,
                      
               radioButtons("file_ext", "Data format:", choices = c("xls/xlsx", "csv", "txt"), selected = "xls/xlsx", inline=F)
               ),
               column(width=3,
                              conditionalPanel(condition=("input.file_ext=='xls/xlsx'"), numericInput("file_sheet", "Sheet:", value=1, min=1)),
               conditionalPanel(condition=("input.file_ext=='csv'"), selectInput("file_sep", "Separator:",  choices = c(",", ";", "\t"), selected = ","))
               
           ),
           column(width=3,
                  selectInput("file_dec", "Decimal:",  choices = c(".", ","), selected = ".")
           )
           ),
         wellPanel(
           uiOutput("fileInputs")
         )
         
  ),
  
  column(9,

         fluidRow(

           uiOutput("fileInputs2")

         ),

         fluidRow(
           bsCollapse(
           
                      bsCollapsePanel(title = "", value = "proc_panel", DT::dataTableOutput("contents_proc"))

           )

         )
  )
)

