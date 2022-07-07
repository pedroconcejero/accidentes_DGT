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
categoricas

shinyUI(
  navbarPage("Shiny Accidentes DGT",
             tabPanel("Descripción del trabajo",
                      mainPanel(
                        h1("Análisis accidentes DGT a partir microdatos 2020", align = "center"),
                        h2("Propuesto por Pedro Concejero para grupo R - madRid", align = "center"),
                        p("Primera pestaña: exploración de factores relevantes para gravedad accidente"),
                        p("Segunda pestaña: tablas detalladas por factor de accidente"),
                        p("Tercera pestaña: serie temporal (hasta donde se puede con estos datos) por año y por provincia")
                      )),
             tabPanel("Exploraciones vcd",
                      sidebarPanel(
                        
                        selectInput('x', 
                                    'Elige variable numérica', 
                                    continuas, continuas[[3]]),
                        selectInput('y', 
                                    'Elige variable cualitativa', 
                                    categoricas, categoricas[[34]]),
                        checkboxInput('ceros', 'Excluye ceros')
                      ),
                      
                      mainPanel(
                        plotOutput('plot',
                                   height=1000)
                      )
             ),
             tabPanel("Tablas",
                      sidebarPanel(
                        
                        selectInput('x2', 
                                    'Elige variable numérica', 
                                    continuas, continuas[[3]]),
                        selectInput('y2', 
                                    'Elige variable cualitativa', 
                                    categoricas, categoricas[[34]])
                        # checkboxInput('ceros', 'Excluye ceros'),
                      ),
                      
                      mainPanel(
                        DT::dataTableOutput("table")
                      )
                      
                        
                      ),
             tabPanel("Serie temporal en España o por provincia",
                      sidebarPanel(
                        selectInput('x3', 
                                    'Elige variable numérica', 
                                    choices = continuas, 
                                    selected = continuas[[3]]),
                        selectInput(inputId = 'provincia',
                                    "Seleccione España o una provincia",
                                    choices = equivalenciasISO$Provincia,
                                    selected = "España"), # Por defecto España

                      mainPanel(
                        plotOutput(outputId = 'serie_t_plot',
                                   height = 600,
                                   width = 800 
                        )
                        
                      )
             ))
  )
)

