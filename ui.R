
source("helpers.R")

dashboardPage(
  
  dashboardHeader(
    
  title = "RedRedundance",
  
  tags$li(a(href = 'http://www.ueb.vhir.org',
            img(src = 'ueb.png',
                title = "UEB", height = "30px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown"),
  
  tags$li(a(href = 'https://vhir.vallhebron.com/',
            img(src = 'vhir.png',
                title = "VHIR", height = "30px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown")
  ),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Parameters", tabName = "options", icon = icon("cog")),
    menuItem("Input Data", tabName = "inputdata", icon = icon("upload")),
    menuItem("Filter and summary", tabName = "filterdata", icon = icon("filter")),
    menuItem("Results", tabName = "results", icon = icon("object-group")),
    menuItem("About", tabName = "about", icon = icon("question"))
    )),
  
  dashboardBody(
      
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "mycss.css")
    ),
    
      shinyDashboardThemes(
        theme = "onenote"),

      tabItems(
        tabItem(tabName = "options",
                source("ui-tab-options.R",local=TRUE)$value),
        tabItem(tabName = "inputdata",
                source("ui-tab-inputdata.R",local=TRUE)$value),
        tabItem(tabName = "filterdata",
                source("ui-tab-filterdata.R",local=TRUE)$value),
        tabItem(tabName = "results",
                source("ui-tab-results.R",local=TRUE)$value),
        tabItem(tabName = "about",
                source("ui-tab-about.R",local=TRUE)$value)
      ),
      
      tags$hr(),
      
      ## FOOTER
      
      # tags$footer(p(strong(a("Mireia Ferrer Almirall", 
      #                        # href = "https://mferrer-almirall.github.io"),  
      #                        "and",
      #                      a("Alex Sánchez Pla", 
      #                        # href = "https://webgrec.ub.edu/webpages/000011/cat/asanchez.ub.edu.html"
      #                        )), 
      #               align="center", width=3),
      #             p("Statistics and Bioinformatics Unit", align="center", width=3),
      #             p("Contact: mireia.ferrer.vhir@gmail.com"), align="center", width=3),
      #             p(("Copyright (C) 2021, app licensed under GPLv3"), align="center", width=4), align="center", width=4,
      #             p(("Code available on Github:"),a("https://github.com/mferrer-almirall/RedRedundance",
      #                                               href="https://github.com/mferrer-almirall/RedRedundance"))),
      
      ## GOOGLE ANALYTICS
      
      # tags$head(includeScript("google-analytics.js"))
    )
)

