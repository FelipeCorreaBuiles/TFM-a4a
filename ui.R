# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#
library(shiny)
library(shinydashboard)
#shinyUI(#fluidPage(
dashboardPage(skin = "green",#dashboardHeader(disable = TRUE),
  #titlePanel('Opciones'),
  dashboardHeader(title = "Opciones"),
 # sidebarLayout(
 dashboardSidebar(
    #sidebarPanel(width = 3,
    
      textInput("pass", "token", "unirTFM"),
      #este panel guarda todas los menus laterales
          conditionalPanel(condition = "input.pass == 'unirTFM'",
          #############################################
          #Panel de control de datos            
          ############################################
              conditionalPanel(condition = "input.tabs == 'data'",
              fileInput('file1', 'Seleccione el archivo que desea subir (debe ser texto)',
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
              checkboxInput('auto_reading', 'Ver Opciones Avanzadas de Lectura', FALSE),conditionalPanel(condition = "input.auto_reading == true",
              checkboxInput('header', 'Nombre De Columnas en la primera fila', TRUE),
              radioButtons('sep', 'Separador de Columnas',
                           c("Coma (,)"=',',
                             "Punto y Coma (;)"=';',
                             "Tabulación"='\t',"Otro"='otro'),
                           ';'),
              conditionalPanel(condition = "input.sep == 'otro'",textInput("otro_sep", "Separador de columnas del archivo", ">")),
              radioButtons('quote', 'Quote',
                           c(None='',
                             'Comillas Dobles'='"',
                             'Comillas Simples'="'"),
                           '"'),
              selectInput("decimal", "Separador de Decimales:",
                          c("Punto (.)" = ".",
                          "Coma (,)" = ","
                            )),
              textInput("na_strings", "Definición de nulos en cadenas EJ: NULL, NA", "NULL"),
              checkboxInput('string_as_factors', 'Convertir Cadenas en Variables categóricas', TRUE)),
              actionButton("leerArchivo", "Leer Archivo",width = '100%'),
              tags$hr()#,
              #checkboxInput('vars_edit', 'Realizar filtrado de variables No necesarias', FALSE)
             
              
            ),
          ###################################################
          # Fin control de datos
          ##################################################
          
          ###################################################
          # Inicio Preprocesamiento
          ##################################################
        #conditionalPanel(condition = "input.tabs == 'data' && input.vars_edit  == '1'",
                 #      tags$hr('Selección de Variables')#,
                       #uiOutput("VarControls")
                      # actionButton("var_filter", "Leer Archivo",width = '100%')
                    #   ),
        ###################################################
        # fin Preprocesamiento
        ##################################################
        
        ###################################################
        # Inicio Auditar datos
        ##################################################
        conditionalPanel(condition = "input.tabs == 'auditar'",
                         tags$hr('Auditar Datos'),
                         actionButton("do_correlation", "Calcular Correlaciones",width = '100%'),
                         tags$hr('Imputación de Datos Vacíos o Perdidos'),
                         selectInput("string_na", "Imputación Variables Categóricas (String)" , 
                                     c("No imputar" = "nothing",
                                       "Valor mas Frecuente" = "most_frecuent",
                                       "Valor Fijo Remplazar Con" = "fixed",
                                       "Eliminar Fila" = "erase_row"
                                     )),
                         conditionalPanel(condition = "input.string_na == 'fixed'",
                                          textInput("fixed_string_na", "Valor para reemplazar String", "")
                                          ),
                         selectInput("numeric_na", "Imputación Variables Numéricas" , 
                                     c("No imputar" = "nothing",
                                       "media" = "median",
                                       "minimo" = "minimum",
                                       "maximo" = "maximun",
                                       "Valor Fijo Remplazar Con" = "fixed",
                                       "Eliminar Fila" = "erase_row"
                                     )),
                         conditionalPanel(condition = "input.numeric_na == 'fixed'",
                                          textInput("fixed_numeric_na", "Valor para reemplazar Variables numéricas", "0")
                                          ),
                         actionButton("do_na_fix", "Aplicar imputaciones",width = '100%')
                         ),
        
        ###################################################
        # fin  Auditar datos
        ##################################################
        ###################################################
        # Inicio sumarizacion
        ##################################################
        
        conditionalPanel(condition = "input.tabs == 'sumarizacion'"
                         ,tags$hr(),
                         checkboxInput("aggregates", "Sumarización de Variables", FALSE)),
              #si se selecciona el chech agreggates se visualiza este panel
              conditionalPanel(condition = "input.aggregates == true & input.tabs == 'sumarizacion'",
                               uiOutput("groupby"),
                               uiOutput("aggregationColumns"),
                               uiOutput("aggMethods"),
                               actionButton("ApplyAgg", "Aplicar Cambios al conjunto de datos")
                               ),
        ###################################################
        # fin sumarizacion
        ##################################################
        ###################################################
        # inicio visualizacion
        ##################################################
        conditionalPanel(condition = "input.tabs == 'visualizacion'",
                         selectizeInput("plotType", "Seleccione hasta dos gráficos para visualizar:",
                                     c("Scatter Plot" = "scatter",
                                       "Scatter Matrix" = "matrix",
                                       "Box Plot" = "box"),
                                     multiple = TRUE,
                                     options = list(maxItems = 2)),
                         conditionalPanel(condition = "input['plotType']!= null && input['plotType'].indexOf('scatter')>-1 ",
                                          h4("Seleccione las opciones del gráfico"),
                                          textInput("scatter_titulo", "Tìtulo de tu gráfico de Dispersión", "Titulo"),
                                          uiOutput("x_col"),
                                          uiOutput("scatter_xlab"),
                                          uiOutput("y_col"),
                                          uiOutput("scatter_ylab"),
                                          uiOutput("color_scatter"),
                                          tags$hr()
                                          ),
                               conditionalPanel(condition = "input['plotType']!= null && input['plotType'].indexOf('matrix')>-1 ",
                                                h4("Seleccione las variables del gráfico"),
                                                uiOutput("matrix_menu"),
                                                uiOutput("color_matrix"),
                                                radioButtons("matrix_advance", label = h3("Configuración del gráfico"),
                                                            choices = list("Utilizar configuración por defecto" = 1, 
                                                                           "Mostrar controles avanzados" = 2), 
                                                            selected = 1),
                               conditionalPanel(condition = "input.matrix_advance == '2'",
                                                tags$hr("Tipos de Gráficos para la diagonal Superior"),
                                                tags$hr(),
                                                checkboxInput("show_upper", "Mostrar diagonal superior", TRUE),
                                                conditionalPanel(condition = "input.show_upper == '1'",
                                                        selectInput("cont_upper", "Tipo de gráfico para variables continuas",
                                                                    c("Puntos"="points", "Suavizado"="smooth", "Densidad"="density","Correlaciones" = "cor","sin Gráfico" = "blank"),
                                                                    selected = 'cor'
                                                        ),
                                                        selectInput("combo_upper", "Tipo de gráfico para variables combo",
                                                                    c("Box Plot" = "box", "Dot" = "dot", "Facethist" = "facethist", "Facetdensity" = "facetdensity", "Denstrip" = "denstrip","sin Gráfico" = "blank"),
                                                                    selected = 'box'
                                                        ),                                               
                                                        
                                                        selectInput("discrete_upper", "Tipo de gráfico para variables discretas",
                                                                    c("Facetbar" = "facetbar", "ratio" = "ratio", "sin Gráfico" = "blank")
                                                        )
                                                  ) , 
                                                
                                                tags$hr("Tipos de Gráficos para la diagonal inferior"),
                                                tags$hr(),
                                                checkboxInput("show_lower", "Mostrar diagonal inferior", TRUE),
                                                conditionalPanel(condition = "input.show_lower == '1'",
                                                      selectInput("cont_lower", "Tipo de gráfico para variables continuas",
                                                                  c("Puntos"="points", "Suavizado"="smooth", "Densidad"="density","Correlaciones" = "cor","sin Gráfico" = "blank"),
                                                                  selected = 'points'
                                                      ),
                                                      selectInput("combo_lower", "Tipo de gráfico para variables combo",
                                                                  c("Box Plot" = "box", "Dot" = "dot", "Facethist" = "facethist", "Facetdensity" = "facetdensity", "Denstrip" = "denstrip","sin Gráfico" = "blank"),
                                                                  selected = 'facethist'
                                                      )  ,                                               
                                                      
                                                      selectInput("discrete_lower", "Tipo de gráfico para variables discretas",
                                                                  c("Facetbar" = "facetbar", "ratio" = "ratio", "sin Gráfico" = "blank")
                                                      ) 
                                                ),
                                                tags$hr("Tipos de Gráficos para la diagonal inferior"),
                                                tags$hr(),
                                                
                                                checkboxInput("show_diag", "Mostrar diagonal principal", TRUE),
                                                
                                                conditionalPanel(condition = "input.show_diag == '1'",
                                                
                                                    selectInput("cont_diag", "Tipo de gráfico para variables Continuas",
                                                                c("densityDiag", "barDiag", "blankDiag")
                                                    )  , 
                                                    selectInput("discrete_diag", "Tipo de gráfico para variables Discretas",
                                                                c("barDiag", "blankDiag")
                                                    )  
                                                )),
                               
                               tags$hr()
                         ),
                         
                         uiOutput("box_menu")
                         
                         ),
        ###################################################
        #               fin visualizacion                 #
        ##################################################
        conditionalPanel(condition = "input.tabs == 'mineria_previa'",
                          tags$h5('Seleccion de Variables para Modelar'),
                          uiOutput("ModelVarControls"),
                          tags$hr('Transformación de Variables para Modelar'),
                          checkboxInput("scale_on", "Normalizar Variables", FALSE),
                          checkboxInput("nzv", "Selección Automática de Variables", FALSE),
                          checkboxInput("outliers", "Remove Outliers", FALSE),
                          checkboxInput("nulos", "Remove nulls", FALSE),
                          actionButton("do_model_var_transform", HTML("Aplicar Transformaciones a las</br>variables seleccionadas")) ,
                          sliderInput('number_clusters_validation', 'Chose the interval for Cluster Selection Plot. You can select how many centers to compare in order to make the right decision',
                                    value = c(2,8), min = 2, max = 15, step = 1),
                          actionButton("do_cluster_test", "Realizar Prueba de Clústers")),
                          conditionalPanel(condition = "input.tabs == 'clustering'",
                          sliderInput('number_centers', 'Select the Number of Centers of the Solution.  The plot of the within groups sum of squares by number of clusters extracted can help you to determinate the appropriate number of clusters',value = 4, min = 2, max = 15, step = 1),
                          actionButton("do_cluster", "Generar Modelo de Clúster")
                         
                         ),
        
        conditionalPanel(condition = "input.tabs == 'despliegue'",tags$hr('Que desea hacer con el Modelo'),
                         actionButton("aplicar_modelo", "Aplicar Modelo A los datos"),
                         tags$hr(),
                         actionButton("aplicar_modelo_datos_nuevos", "Aplicar Modelo A datos nuevos"),
                         tags$hr(),
                         actionButton("descargar_config_proyecto", "Descargar Modelo y Configuración")
                         ),
        conditionalPanel(condition = "input.tabs == 'classify'",
                         tags$hr('Modelos de clasificación'),
                         selectizeInput("class_mod_type", "Seleccione hasta 2 modelos para comparar resultados:",
                                        c("Árbol de Clasificación" = "tree",
                                          #"Random Forest" = "random_forest",
                                          "Máquina de Vectores" = "svm"),
                                        multiple = TRUE,
                                        selected = c("tree","svm"),
                                        options = list(maxItems = 2)),
                         uiOutput('out_class_var'),
                         uiOutput('choose_columns_classify'),
                         actionButton("do_classify", "Realizar Entrenamiento de Modelos",width = '100%')
                         )
        
            )
    )
    ,
    #mainPanel(
    dashboardBody(
      #tags$head(tags$style(HTML('
     # .main-header .logo {
     #                           font-family: "Georgia", Times, "Times New Roman", serif;
       #                         font-weight: bold;
       #                         font-size: 24px;
      #                          }
       #                         '))),
      tabsetPanel(id = "tabs",type = "tabs",
                  tabPanel("Carga de Datos",value = "data", 
                           fluidRow(
                           column(3,
                           box(side= 'right',
                             title = "Filtrar Variables",width = NULL, solidHeader = FALSE,status="success",color="black",
                             collapsible = TRUE,collapsed = FALSE,uiOutput("VarControls")
                           ),box(
                             title = "Renombrar Variables",width = NULL, solidHeader = FALSE,status="success",
                             collapsible = TRUE,collapsed = TRUE,uiOutput('VarControls_rename')
                           )),
                           column(9,offset = 0,
                                  box(
                                    title = "Data",width = NULL, solidHeader = FALSE,status="success",
                                    collapsible = TRUE, dataTableOutput('contents'))))),
                  
      tabPanel("Auditar Variables", value = "auditar",
               radioButtons('summary_method', 'Seleccione el Paquete Para Generar los estadísticos',
                            c("Basico"='basico',
                              # "Hmisc"='hmisc',
                              "pastecs"='pastecs',"psych"='psych'),
                            
                            'basico',inline = TRUE),
               dataTableOutput('summary_vars'),
               dataTableOutput('na_fix_table'),
               h4("Correlación de Variables",align = "center"),
               plotOutput('out_plot_correlations')
               ),
      
      
      tabPanel("Transformación de Variables", value = "sumarizacion",
               dataTableOutput('summary')),
      
      tabPanel("Visualización", value = "visualizacion",
               conditionalPanel(
                 condition = "input['plotType']!= null && input['plotType'].indexOf('scatter')>-1",
               h4("Seleccione la región en la cual desea hacer ZOOM usando clic sostenido y luego de doble clic en ella."),
               h4("Para volver al gráfico original, de doble clic en cualquier parte del gráfico"),
               plotOutput("scatter_plot", height = 300,
                          dblclick = "scatter_plot_dblclick",
                          brush = brushOpts(
                            id = "scatter_plot_brush",
                            resetOnNew = TRUE
                          ))
               
               ),
               conditionalPanel(
                 condition ="input['plotType']!= null && input['plotType'].indexOf('matrix')>-1 ",
               plotOutput("matrix_plot")
               )
               ),
      tabPanel("Preprocesamiento para Mineria", value = "mineria_previa",
               h5("This plot will help you to determinate the appropriate Number of Clusters(look for a bend in the plot )"),
               dataTableOutput('modified_final_vars'),
               plotOutput('numberOfClusters',width = "500px", height = "400px")),
      
      tabPanel("Generación de Modelos De Clustering", value = "clustering",
               h5("Look at the Cluster results"),
               plotOutput('fitCluster',width = "500px", height = "400px"),
               h5("Estadisticas de los grupos generados (Medias por variable y grupo)"),
               dataTableOutput("centers"),
               h5("Clusters Distribution"),
               plotOutput('clusters',width = "500px", height = "400px")
               ),
      tabPanel("Generación de Modelos De Clasificación", value = "classify",
               fluidRow(column(4, sliderInput("train_percent", "Porcentaje de registros para entrenamiento", 
                           min=50, max=90, value=75)),
               column(5,sliderInput("train_cv", "Número de Validaciones cruzadas", 
                           min=2, max=10, value=3))),
               #h3("Salida de los modelos"),
               fluidRow(column(4, h4("Efectividad Arbol de clasificación"),tableOutput('confusionMatrixTree_table'),tableOutput('confusionMatrixTree_byClass')),
                        column(6,  h4("Efectividad Máquina de Vectores"),tableOutput('confusionMatrixSVM_table'),tableOutput('confusionMatrixSVM_byClass'))),
               plotOutput('out_classify_chart'),
               plotOutput('out_classify_chart2'),
               plotOutput('trained_tree')
               ),
      
      
      tabPanel("Despliegue del Modelo", value = "despliegue")
      
      )
    )
  )


#)#)