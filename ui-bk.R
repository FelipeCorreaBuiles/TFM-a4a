
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#
library(shiny)

shinyUI(fluidPage(
  
  titlePanel('Opciones'),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition = "input.tabs == 'data'",
      fileInput('file1', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t',Otro='otro'),
                   
                   ';'),
      conditionalPanel(condition = "input.sep == 'otro'",textInput("otro_sep", "Separador de columnas del archivo", ">")),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      selectInput("decimal", "Separador de Decimales:",
                  c("Punto (.)" = ".",
                  "Coma (,)" = ","
                    )),
      textInput("na_strings", "Definición de nulos en cadenas EJ: NULL, NA", "NULL"),
      checkboxInput('string_as_factors', 'Convertir Cadenas en Variables categóricas', TRUE),
      actionButton("leerArchivo", "Leer Archivo"),
      tags$hr()
      
    ),
    conditionalPanel(condition = "input.tabs == 'preprocesing'",tags$hr(),uiOutput("VarControls") ),
    conditionalPanel(condition = "input.tabs == 'preprocesing'",tags$hr(),checkboxInput("aggregates", "Aggregate", FALSE)),
    conditionalPanel(condition = "input.aggregates == true & input.tabs == 'sumarizacion'",uiOutput("groupby"),uiOutput("aggregationColumns"),uiOutput("aggMethods")),
    conditionalPanel(condition = "input.tabs == 'preprocesing'",tags$hr(),checkboxInput("scale", "Scale", FALSE),checkboxInput("outliers", "Remove Outliers", FALSE),checkboxInput("nzv", "Remove Zero Variance", FALSE))
        ),
    mainPanel(
      tabsetPanel(id = "tabs",type = "tabs",
                  tabPanel("Data Uploaded",value = "data", 
      tableOutput('contents')),
      tabPanel("Preprocesing", value = "preprocesing",
                   tableOutput('contents_pre')),
      tabPanel("Transformacion de Variables", value = "sumarizacion",
               tableOutput('summary')),
      tabPanel("Mineria de Datos", value = "mineria"
              ),
      tabPanel("Despliegue", value = "despliegue"
             )
      
      )
    )
  )
))