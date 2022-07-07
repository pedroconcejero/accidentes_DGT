#
# Pedro Concejero para R madRid
# 7 julio 2022


library(shiny)
library(tidyverse)
library(readxl)
library(vcd)
library(DT)
library(ggplot2)

# setwd("/home/pedro/Escritorio/00_madRid_julio_22/accidentes_dgt")
## Cargamos datos

dataset <- read_excel("TABLA_ACCIDENTES_20 v2.xlsx",
                      na = c("999", "9999"),
                      col_types = c(rep("text", 2),
                                    rep("numeric", 2),
                                    rep("text", 13),
                                    rep("numeric", 33),
                                    rep("text", 25)))
summary(dataset)

equivalenciasISO = read_excel("EquivalenciasCodigoISO.xlsx")

# distinguimos variables "a nivel de intervalo" ("continuas" para ggplot)
nums <- sapply(dataset, is.numeric)
continuas <- names(dataset)[nums]
continuas
# y variables "categóricas" ("discretas" para ggplot)
cats <- sapply(dataset, is.character)
categoricas <- names(dataset)[cats]


shinyServer(function(input, output) {
  
  output$plot <- renderPlot({
    
    if (input$ceros){
    
      df <- dataset %>% filter(get(input$x) > 0)
      mosaic((as.formula(paste("~ ", input$y, "+" , input$x))), 
           data = df,
           main = "DGT", shade = TRUE, legend = TRUE)
    } else {
      mosaic((as.formula(paste("~ ", input$y, "+" , input$x))), 
             data = dataset,
             main = "DGT", shade = TRUE, legend = TRUE)
      
    }
    
  })

  output$table <- 
    DT::renderDataTable(DT::datatable({
      data <- dataset[, c(input$x2, input$y2)] 
      by_group <- data %>% group_by_at(input$y2) %>% 
        summarise(across(everything(), sum))
      by_group
    }))
  
  # Evolucion en España o por provincia
  output$serie_t_plot = renderPlot({
    
    # Caso seleccion España
    if (input$provincia == "España"){
      datos_serie_t = dataset[ , c("MES", input$x3)]
      datos_serie_t %>% group_by(MES) %>% 
        summarise(total = sum(get(input$x3))) %>% 
        ggplot(aes(x = as.factor(MES), 
                   y = total)) +
        geom_bar(stat = "identity", position = "dodge") 

    # Caso seleccion una provincia. Obtenemos su equivalencia iso  
    } else{
      datos_serie_t = dataset[ , c("COD_PROVINCIA",
                                   "MES", 
                                   input$x3)] %>%
        filter(COD_PROVINCIA == toString(equivalenciasISO$CPOSTAL[equivalenciasISO$Provincia==input$provincia]))
      datos_serie_t %>% group_by(MES) %>% 
        summarise(total = sum(get(input$x3))) %>% 
        ggplot(aes(x = as.factor(MES), 
                   y = total)) +
        geom_bar(stat = "identity", position = "dodge") 
    }
    
    # print(p)
    
   
  })  
})
