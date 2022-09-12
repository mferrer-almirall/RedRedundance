
#list of packages required
list.of.packages <- c("pacman", "BiocManager", "shiny", "shinydashboard", "tidyverse", "dashboardthemes", "shinyWidgets","shinyhelper","shinyBS","plotly","reactome.db", "GO.db", 
                      # "simplifyEnrichment", 
                      "magick", "AnnotationDbi", "ComplexHeatmap","org.Hs.eg.db", "org.Mm.eg.db", "DT", "readxl")

#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
if(length(new.packages)) BiocManager::install(new.packages, dependencies = TRUE)

#Current version of RedRedundance needs version 1.1.5 of simplifyEnrichment package. This version can be downloaded from https://github.com/jokergoo/simplifyEnrichment/releases 
#and installed using:
# install.packages("./simplifyEnrichment-1.1.5.tar.gz", repos = NULL, type = "source")