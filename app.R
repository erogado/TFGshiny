library(shiny)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(dplyr)
library(DT)
library(devtools)
library(plotly)
library(shinythemes)
library(shinydashboard)
library(VIM)
library(FactoMineR)
library(magrittr)
library(mice)
library(nortest)
library(boot)
library(bootstrap)
library(stats)
library(corrplot)
library(MASS)
library(forecast)
library(e1071)  

ui <- fluidPage(theme = shinytheme("sandstone"),
                
                navbarPage("Appstatistics",
                           
                           tabPanel("Datos",
                                    
                                    tabsetPanel(
                                      
                                      tabPanel("Sube tus datos",
                                               
                                               sidebarLayout(
                                                 
                                                 sidebarPanel(width = 2,
                                                              
                                                              fileInput("subirDat", 
                                                                        "Cargar datos CSV",
                                                                        multiple = TRUE,
                                                                        accept = c("text/csv",
                                                                                   "text/comma-separated-values,text/plain",
                                                                                   ".csv")),
                                                              
                                                              checkboxInput("header", 
                                                                            "Cabecera", TRUE),
                                                              
                                                              
                                                              radioButtons("sep", 
                                                                           "Separador",
                                                                           choices = c(Coma = ",",
                                                                                       Puntocoma = ";",
                                                                                       Tab = "\t"),
                                                                           selected = ","),
                                                              
                                                              radioButtons("dec", 
                                                                           "Decimal",
                                                                           choices = c(Coma = ",",
                                                                                       Punto = "."),
                                                                           selected = ","),
                                                              
                                                              
                                                              radioButtons("quote", "Quote",
                                                                           choices = c(None = "",
                                                                                       "Doble Quote" = '"',
                                                                                       "Un Quote" = "'"),
                                                                           selected = '"')
                                                              
                                                              
                                                 ), # Fin absolutePanel Sube tus datos
                                                 
                                                 mainPanel(
                                                   
                                                   fluidRow(
                                                     
                                                     column(width = 12,
                                                            DT::dataTableOutput("tabladatos"))
                                                     
                                                   ) # Fin fluidRow
                                                 ) # Fin mainPanel Sube tus datos
                                                 
                                               ) # Fin del sidebarLayout Sube datos
                                      ), # Fin tabPanel Sube tus datos
                                      
                                      
                                      tabPanel("Estudio preliminar",
                                               
                                               sidebarLayout(
                                                 
                                                 sidebarPanel(width = 2,
                                                              
                                                              checkboxInput("diag_NA", "Mostrar diasnostico NA"),
                                                              
                                                              radioButtons("limpia_NA", "Sustituir los NA por:",
                                                                           inline = T,
                                                                           select = NULL, 
                                                                           choices = c("Media" = "media_NA",
                                                                                       "Predicciones" = "pred_NA",
                                                                                       "Eliminar" = "eliminar_NA")),
                                                              
                                                              
                                                              
                                                              fluidRow(h5("Distr. normal"),
                                                                       
                                                                       column(width = 12,
                                                                              sliderInput("nNorm", width = '450px',
                                                                                          label = h5("Muestra"),
                                                                                          min = 1, max = 1000, value = 500, step = 1))
                                                                       
                                                                       
                                                              ), # fin fluidRow Dist.Normal (muestra)
                                                              
                                                              fluidRow(
                                                                
                                                                column(width = 4, offset = 0.1,
                                                                       numericInput("media", width = '75px',
                                                                                    label = h5("Media"), 
                                                                                    value = 0)),
                                                                
                                                                column(width = 4, offset = 0.1,
                                                                       numericInput("varianza", width = '75px', 
                                                                                    label = h5("Varianza"), min = 0,
                                                                                    value = 1)),
                                                                
                                                                column(width = 4, offset = 0.1,
                                                                       h5("Accion"),
                                                                       actionButton("creaNorm", "Generar", width = '75px'))
                                                                
                                                              ), # fin fluidRow Dist.Normal (media, varianza y generar)
                                                              
                                                              hr(),
                                                              
                                                              fluidRow(h5("Distr. binomial"),
                                                                       
                                                                       column(width = 12,
                                                                              sliderInput("nBin", width = '450px',
                                                                                          label = h5("Muestra"),
                                                                                          min = 1, max = 1000, value = 500, step = 1))
                                                                       
                                                                       
                                                              ), # fin fluidRow Dist.Binomial (muestra)
                                                              
                                                              fluidRow(
                                                                
                                                                column(width = 4, offset = 0.1,
                                                                       numericInput("tamanno", width = '75px', 
                                                                                    label = h5("Tamanno"), 
                                                                                    value = 10)),
                                                                
                                                                column(width = 4, offset = 0.1,
                                                                       numericInput("prob", width = '75px', 
                                                                                    label = h5("Probabilidad"), 
                                                                                    value = 0.5, step = 0.1)),
                                                                
                                                                column(width = 4, offset = 0.1,
                                                                       h5("Accion"),
                                                                       actionButton("creaBinom", "Generar", width = '75px'))
                                                                
                                                              ), # fin fluidRow Dist.Binomial (media, varianza y generar)
                                                              
                                                              hr(),
                                                              
                                                              fluidRow(h5("Distr. Poisson"),
                                                                       
                                                                       column(width = 12,
                                                                              sliderInput("nPois", width = '450px',
                                                                                          label = h5("Muestra"),
                                                                                          min = 1, max = 1000, value = 500, step = 1))
                                                                       
                                                                       
                                                              ), # fin fluidRow Dist.Binomial (muestra)
                                                              
                                                              fluidRow(
                                                                
                                                                column(width = 4, offset = 0.1,
                                                                       numericInput("lambda", width = '75px',
                                                                                    label = h5("Lambda"), 
                                                                                    value = 1)),
                                                                
                                                                column(width = 4, offset = 0.1,
                                                                       h5("Accion"),
                                                                       actionButton("creaPoiss", "Generar", width = '75px'))
                                                                
                                                              ) # fin fluidRow Dist.Binomial (lambda y generar)
                                                              
                                                              
                                                 ), # Fin sidebarPanel Estudio preliminar
                                                 mainPanel(
                                                   
                                                   h4("Summary de los datos"),
                                                   
                                                   br(), 
                                                   
                                                   fluidRow(
                                                     column(width = 12, offset = 0.5,
                                                            verbatimTextOutput("summaryDa"))
                                                   ),
                                                   
                                                   br(), br(), 
                                                   
                                                   h4("Clase de las variables"),
                                                   
                                                   br(), 
                                                   
                                                   fluidRow(
                                                     column(width = 12, offset = 0.5,
                                                            verbatimTextOutput("classDa"))
                                                     
                                                   ),
                                                   
                                                   br(), br(),
                                                   
                                                   h4("Diagnostico NA"),
                                                   
                                                   br(),
                                                   
                                                   fluidRow(
                                                     column(width = 5, offset = 0.25,
                                                            plotOutput("diagnostico_NA", width = "500px")),
                                                     
                                                     
                                                     column(width = 5, offset = 0.25,
                                                            plotOutput("marginalDiagnostico_NA", width = "500px")),
                                                     
                                                     column(width = 2, offset = 0.25,
                                                            uiOutput("selecvarXDiag_NA"),
                                                            uiOutput("selecvarYDiag_NA"))
                                                   ),
                                                   
                                                   br(), br(),
                                                   
                                                   fluidRow(
                                                     column(width = 8, offset = 2,
                                                            tableOutput("tabla_NA"))
                                                   ),
                                                   
                                                   conditionalPanel(condition = "input.limpia_NA == 'pred_NA'",
                                                                    fluidRow(
                                                                      column(width = 6, offset = 0.25,
                                                                             h4("Imputacion MICE"), br(),
                                                                             verbatimTextOutput("mids")),
                                                                      
                                                                      column(width = 4, offset = 0.25,
                                                                             h4("Valores imputados"), br(),
                                                                             verbatimTextOutput("val_input")),
                                                                      
                                                                      column(width = 2, offset = 0.25,
                                                                             uiOutput("selec_varImp"),
                                                                             numericInput("iteraccion_num", "Iteraccion elegida:",
                                                                                          min = 0, max = 5, value = NULL, step = 1),
                                                                             actionButton("camb_NA_pred", "Aplicar")))   
                                                                    
                                                   ),
                                                   
                                                   br(), 
                                                   
                                                   conditionalPanel(condition = "input.limpia_NA == 'media_NA'",
                                                                    fluidRow(
                                                                      column(width = 10, offset = 0.5,
                                                                             h4("Media por variable"), br(),
                                                                             verbatimTextOutput("media_text")),
                                                                      column(width = 1, offset = 0.5,
                                                                             actionButton("camb_NA_media", "Aplicar"))
                                                                      
                                                                    )),
                                                   
                                                   br(), 
                                                   
                                                   conditionalPanel(condition = "input.limpia_NA == 'eliminar_NA'",
                                                                    fluidRow(
                                                                      column(width = 10, offset = 0.5,
                                                                             h4("Observaciones a eliminar:"), br(),
                                                                             verbatimTextOutput("eliminar_text")),
                                                                      column(width = 1, offset = 0.5,
                                                                             actionButton("camb_NA_eliminar", "Aplicar"))
                                                                      
                                                                    ))
                                                   
                                                   
                                                 ) # Fin mainPanel Estudio preliminar
                                                 
                                               ) # Fin sidebarLayout Estudio preliminar
                                               
                                      ), # Fin tabPanel Summary y clase
                                      
                                      tabPanel("Estudio normalidad",
                                               
                                               sidebarLayout(
                                                 
                                                 sidebarPanel(width = 2,
                                                              
                                                              uiOutput("var_normalidad"),
                                                              
                                                              checkboxInput("log_apli", "Aplicar logaritmos", value = F), 
                                                              
                                                              conditionalPanel(condition = 'input.log_apli',
                                                                               helpText("Seleccionar variables a aplicar logaritmos."),
                                                                               uiOutput("var_log1")),
                                                              
                                                              conditionalPanel(condition = 'input.log_apli',
                                                                               uiOutput("var_log2")),
                                                              
                                                              
                                                              conditionalPanel(condition = 'input.log_apli',
                                                                               p("Pulsar el boton para annadir variables."), 
                                                                               actionButton("apli_logBOTON", "Aplicar"),
                                                                               actionButton("apli_logELIMINAR", "Eliminar")), 
                                                              
                                                              br(),
                                                              
                                                              checkboxInput("scale_apli", "Aplicar escalado", value = F),
                                                              
                                                              conditionalPanel(condition = 'input.scale_apli',
                                                                               helpText("Seleccionar variables a escalar."),
                                                                               uiOutput("var_scale1")),
                                                              
                                                              conditionalPanel(condition = 'input.scale_apli',
                                                                               uiOutput("var_scale2")),
                                                              
                                                              conditionalPanel(condition = 'input.scale_apli',
                                                                               p("Pulsar el boton para annadir variable."), 
                                                                               actionButton("apli_scaleBOTON", "Aplicar"),
                                                                               actionButton("apli_scaleELIMINAR", "Eliminar")),
                                                              br(),
                                                              
                                                              checkboxInput("boxcox", "Transformaciones Box Cox", value = F),
                                                              
                                                              conditionalPanel(condition = 'input.boxcox', 
                                                                               actionButton("apli_cox", "Aplicar")),
                                                              
                                                              br(), helpText("A continuacion se habilitan los controles necesarios para visualizar
                                                                             la media y la varianza poblacionales estimadas a partir de bootstrapping. 
                                                                             Tambien se proporcionan los IC"),
                                                              
                                                              checkboxInput("media_boot", "Estimar media poblacional", value = F),
                                                             
                                                              checkboxInput("varianza_boot", "Estimar varianza poblacional", value = F),
                                                              
                                                              conditionalPanel(condition = 'input.media_boot || input.varianza_boot',
                                                                               numericInput("repli", "Replicas bootstrap", min = 500, max = 5000, value = 1000, step = 100)),
                                                              
                                                              conditionalPanel(condition = 'input.media_boot || input.varianza_boot',
                                                                               numericInput("confianza", "Confianza del intervalo", min = 0.05, max = 0.95, value = 0.95, step = 0.05))
                                                              
                                                              
                                                              ), # Fin sidebarPanel Estudio normalidad
                                                 
                                                 mainPanel(
                                                   
                                                   fluidRow(
                                                     column(width = 5, offset = 0.5,
                                                            br(),
                                                            h4("Test de normalidad de Pearson"), 
                                                            verbatimTextOutput("test_normPearson")),
                                                     
                                                     column(width = 5, offset = 0.5,
                                                            br(),
                                                            h4("Test de normalidad Shapiro"), 
                                                            verbatimTextOutput("test_normSP"))
                                                     
                                                   ), # Fin fluidRow mainPanel 
                                                   
                                                   fluidRow(
                                                     conditionalPanel(condition = 'input.log_apli',
                                                                      column(width = 5, offset = 0.5,
                                                                             br(),
                                                                             h4("Variable 1 con los logaritmos aplicados"), 
                                                                             tableOutput("tabla_log1"))),
                                                     
                                                     
                                                     conditionalPanel(condition = 'input.log_apli',
                                                                      column(width = 5, offset = 0.5,
                                                                             br(),
                                                                             h4("Variable 2 con los logaritmos aplicados"), 
                                                                             tableOutput("tabla_log2")))
                                                     
                                                   ), 
                                                   
                                                   fluidRow(
                                                     conditionalPanel(condition = 'input.scale_apli',
                                                                      column(width = 5, offset = 0.5,
                                                                             br(),
                                                                             h4("Variable 1 con el escalado aplicado"), 
                                                                             tableOutput("tabla_scale1")),
                                                                      
                                                                      
                                                                      conditionalPanel(condition = 'input.scale_apli',
                                                                                       column(width = 5, offset = 0.5,
                                                                                              br(),
                                                                                              h4("Variable 2 con escalado aplicado"), 
                                                                                              tableOutput("tabla_scale2"))))
                                                   ),
                                                   
                                                   fluidRow(
                                                     conditionalPanel(condition = 'input.media_boot',
                                                                      column(width = 5, offset = 0.5,
                                                                             br(),
                                                                             h4("Estimacion de la media poblacional"), 
                                                                             verbatimTextOutput("est_media"), br(),
                                                                             h4("Intervalos de confianza"),
                                                                             verbatimTextOutput("ic_est_media"))),
                                                     
                                                     conditionalPanel(condition = 'input.varianza_boot',
                                                                      column(width = 5, offset = 0.5,
                                                                             br(),
                                                                             h4("Estimacion de la varianza poblacional"), 
                                                                             verbatimTextOutput("est_varianza"), br(),
                                                                             h4("Intervalos de confianza"),
                                                                             verbatimTextOutput("ic_est_varianza")))
                                                     
                                                     
                                                   ) # Fin fluidRow mainPanel 
                                                   
                                                   
                                                 ) # Fin mainPanel Estudio normalidad
                                                 
                                                 
                                                 ) # Fin sidebarLayout Estudio normalidad
                                               
                                      ), # Fin tabPanel Estudio normalidad
                                      
                                      tabPanel("Estudio de independencia",
                                               
                                               sidebarLayout(
                                                 
                                                 sidebarPanel(width = 2,
                                                              
                                                              helpText("Seleccionar variables para agrupar en intervalos."),
                                                              
                                                              uiOutput("var_ind1"), 
                                                              
                                                              uiOutput("var_ind2"),
                                                              
                                                              checkboxInput("prop", "Mostrar proporciones muestrales"), br(),
                                                              
                                                              helpText("Notese que se emplea la metodologia bootstrap para estimar las proporciones poblacionales"),
                                                              
                                                              checkboxInput("prop_boot", "Mostrar las proporciones pobacionales estimadas"),
                                                              
                                                              checkboxInput("plot_recuentos", "Mostrar grafico de los recuentos")
                                                              
                                                              
                                                 ), # Fin sidebarPanel Estudio de independencia
                                                 
                                                 mainPanel(
                                                   
                                                   fluidRow(
                                                     column(width = 6, offset = 0.25,
                                                            br(),
                                                            h4("Intervalos y recuentos varianle 1"),
                                                            verbatimTextOutput("intervalos_v1")),
                                                     column(width = 6, offset = 0.25,
                                                            br(),
                                                            h4("Intervalos y recuentos varianle 2"),
                                                            verbatimTextOutput("intervalos_v2"))
                                                   ),
                                                   
                                                   conditionalPanel(condition = 'input.prop',
                                                                    fluidRow(
                                                                      column(width = 6, offset = 0.25,
                                                                             br(),
                                                                             h4("Proporciones variable 1"),
                                                                             verbatimTextOutput("prop_v1")),
                                                                      column(width = 6, offset = 0.25,
                                                                             br(),
                                                                             h4("Proporciones variable 2"),
                                                                             verbatimTextOutput("prop_v2"))
                                                                    )),
                                                   
                                                   conditionalPanel(condition = 'input.plot_recuentos',
                                                                    fluidRow(
                                                                      column(width = 6, offset = 0.25,
                                                                             br(),
                                                                             h4("Graficos proporciones variable 1"),
                                                                             plotlyOutput("plot_v1")),
                                                                      column(width = 6, offset = 0.25,
                                                                             br(),
                                                                             h4("Graficos proporciones variable 2"),
                                                                             plotlyOutput("plot_v2"))
                                                                    ))
                                                   
                                                   
                                                 ) # Fin mainPanel Estudio independencia
                                                 
                                               ) # Fin sidebarLayout Estudio de independencia
                                               
                                      ) # Estudio de independencia
                                      
                                      
                                    ) # Fin tabsetPanel
                           ), # Fin del tabPanel Datos 
                           
                           tabPanel("Analisis descriptivo",
                                    
                                    tabsetPanel(
                                      
                                      tabPanel("Grafico de dispersion",
                                               
                                               sidebarLayout(
                                                 
                                                 sidebarPanel(width = 2,
                                                              
                                                              uiOutput("controles1"),
                                                              
                                                              uiOutput("controles2"),
                                                              
                                                              selectInput("tipReg", "Tipo de regresion:",
                                                                          c("Lineal simple" = "ls",
                                                                            "Logit" = "logit",
                                                                            "Loess" = "loess"),
                                                                          selected = NULL),
                                                              
                                                              checkboxInput("aplicarlogx",
                                                                            "Aplicar logaritmo a X"),
                                                              
                                                              checkboxInput("aplicarlogy",
                                                                            "Aplicar logaritmo a Y"),
                                                              
                                                              
                                                              uiOutput("controlesApliReg"),
                                                              
                                                              
                                                              uiOutput("controlesResidLS"),
                                                              
                                                              conditionalPanel(condition = "input.tipReg == 'loess'",
                                                                               numericInput("span", "Span (alisado):",
                                                                                            label = NULL, min = 0.1, max = 1, step = 0.05))
                                                              
                                                              
                                                 ), # Fin sidebarPanel Analisis descriptivo
                                                 
                                                 mainPanel(
                                                   
                                                   fluidRow(
                                                     
                                                     br(), br(),
                                                     
                                                     column(width = 9, offset = 3,
                                                            h4("Grafica de dispersion"),
                                                            plotlyOutput("plotdisp"),
                                                            uiOutput("smLS"),
                                                            uiOutput("smLOE")),
                                                     
                                                     
                                                     column(width = 4, offset = 2,
                                                            uiOutput("hisResidLS"),
                                                            uiOutput("hisResidLOE")),
                                                     
                                                     column(width = 3, offset = 1,
                                                            uiOutput("plResidLS"),
                                                            uiOutput("plResidLOE")))
                                                   
                                                 ) # Fin mainPanel
                                                 
                                               ) # Fin sidebarLayout Grafico de dispersion
                                      ), # Fin tabPanel Grafico de dispersion
                                      
                                      tabPanel("Grafico de densidad",
                                               
                                               sidebarLayout(
                                                 
                                                 sidebarPanel(width = 2,
                                                              
                                                              uiOutput("controles3"),
                                                              
                                                              uiOutput("trazaQuan")
                                                              
                                                 ), # Fin absolutePanel Grafico Densidad
                                                 
                                                 
                                                 mainPanel(
                                                   
                                                   fluidRow(
                                                     
                                                     br(), br(),
                                                     
                                                     h4("Grafico de densidad"),
                                                     
                                                     br(),
                                                     
                                                     column(width = 9, offset = 3,
                                                            plotlyOutput("plotdens")))
                                                   
                                                 ) # Fin mainPanel Grafico Densidad
                                               ) # Fin sidebarLayout Grafico Densidad
                                      ), # Fin tabPanel Grafico Densidad
                                      
                                      tabPanel("Histograma",
                                               sidebarLayout(
                                                 sidebarPanel(width = 2,
                                                              
                                                              uiOutput("controles4"),
                                                              
                                                              br(),
                                                              
                                                              uiOutput("controles5"),
                                                              
                                                              br(),
                                                              
                                                              uiOutput("controles6"),
                                                              
                                                              br()
                                                              
                                                 ), # Fin absolutePanel Histograma
                                                 
                                                 mainPanel(
                                                   
                                                   fluidRow(
                                                     
                                                     column(width = 6, offset = 1,
                                                            br(), br(), br(),
                                                            plotlyOutput("histograma", width = "750px"))
                                                     
                                                   ) # Fin fluidRow mainPanel Histograma
                                                   
                                                 ) # Fin mainPanel Histograma
                                                 
                                               ) # Fin sidebarLayout Histograma
                                               
                                      ) # Fin tabPanel histograma
                                    ) # Fin tabsetPanel Analisis descriptivo             
                                    
                                    
                           ), # Fin tabPanl Analisis descriptivo
                           
                           tabPanel("Contrastes",
                                    sidebarLayout(
                                      sidebarPanel(width = 2,
                                                   uiOutput("var1"),
                                                   uiOutput("var2")
                                      ), # Fin sidebarPanel Contrastes
                                      
                                      mainPanel(
                                        
                                        fluidRow(
                                          column(width = 6, offset = 0.5,
                                                 br(),br(),
                                                 h4("Test de Pearson"),
                                                 br(),
                                                 verbatimTextOutput("cortest")),
                                          
                                          column(width = 6, offset = 0.5,
                                                 br(),br(),
                                                 h4("T-test"),
                                                 br(),
                                                 verbatimTextOutput("ttest"))
                                        ), # Fin fluidRow
                                        
                                        br(),
                                        
                                        fluidRow(
                                          column(width = 6, offset = 0.5,
                                                 br(),br(),
                                                 h4("Test de varianzas"),
                                                 br(),
                                                 verbatimTextOutput("vartest"))
                                        ) # Fin fluidRow
                                        
                                      ) # Fin mainPanel Contrastes
                                    ) # Fin sidebarLayout Contrastes
                                    
                           ), # Fin del tabPanel Contrastes
                           
                           tabPanel("Analisis multivariante",
                                    
                                    tabsetPanel(
                                      
                                      tabPanel("Regresion lineal multiple",
                                               
                                               sidebarLayout(
                                                 sidebarPanel(width = 2,
                                                              
                                                              checkboxInput("corr", "Matriz y test de correlaciones"),
                                                              
                                                              uiOutput("rm_dep"), uiOutput("rm_Vars"), hr(),
                                                              
                                                              checkboxInput("regMul", "Resultados de la regresion"), hr(),
                                                              
                                                              checkboxInput("colEstu", "Estudio de la colinealidad")
                                                              
                                                 ), # Fin sidebarPanel regresion lineal multiple 
                                                 mainPanel(
                                                   
                                                   conditionalPanel(condition = 'input.corr',
                                                                    fluidRow(
                                                                      column(width = 5, offset = 0.25, br(),
                                                                             h4("Matriz de correlaciones"), 
                                                                             plotOutput("corrPlot")),
                                                                      
                                                                      column(width = 5, offset = 0.25, br(),
                                                                             h4("Test de correlacion de Pearson"),
                                                                             verbatimTextOutput("corrTestPear")),
                                                                      
                                                                      column(width = 2, offset = 0.25, br(),
                                                                             uiOutput("vxCorr"),
                                                                             uiOutput("vyCorr"))
                                                                    )),
                                                   
                                                   conditionalPanel(condition = 'input.regMul',
                                                                    fluidRow(
                                                                      column(width = 5, offset = 0.25, br(),
                                                                             h4("Summary de la regresion"),
                                                                             verbatimTextOutput("sumRegMul")),
                                                                      
                                                                      column(width = 5, offset = 0.25, br(),
                                                                             h4("Intervalos de confianza"),
                                                                             verbatimTextOutput("ICregMul"))
                                                                    )),
                                                   
                                                   conditionalPanel(condition = 'input.colEstu',
                                                                    fluidRow(
                                                                      column(width = 5, offset = 0.25, br(),
                                                                             h4("Factor de inflaccion de la varianza"),
                                                                             verbatimTextOutput("FIV")),
                                                                      
                                                                      column(width = 5, offset = 0.25, br(),
                                                                             h4("Numero de condicion"),
                                                                             verbatimTextOutput("NC"))
                                                                    ))
                                                   
                                                 ) # Fin mainPanel regresion lineal multiple
                                               ) # Fin sidebarLayout Regresion lineal multiple
                                               
                                      ), # Fin tabPanel Regresion lineal multiple
                                      
                                      tabPanel("Analisis cluster",
                                               sidebarLayout(
                                                 
                                                 sidebarPanel(width = 2,
                                                              
                                                              radioButtons("tip_cluster", "Elegir el tipo de cluester:",
                                                                           c("Cluster k-means" = "c_kmeans", 
                                                                             "Cluster jerarquico" = "c_jerarquico"), 
                                                                           selected = NULL, inline = F),
                                                              
                                                              conditionalPanel(condition = "input.tip_cluster == 'c_kmeans'", 
                                                                               
                                                                               uiOutput("varX_clus"), uiOutput("varY_clus"),
                                                                               
                                                                               checkboxInput("dscale", "Escalar los datos"),
                                                                               
                                                                               checkboxInput("vis_var", "Visualizar la variabilidad de los datos"),
                                                                               
                                                                               numericInput("num_var", "Variabilidad del modelo. Desde 2 clusters hasta...", value = 1),
                                                                               
                                                                               numericInput("num_centros", "Seleccionar el numero de centros:", value = 1),
                                                                               
                                                                               numericInput("nstart", "Selecionar nstart (semillas aleatorias seleccionadas):", value = 1),
                                                                               
                                                                               selectInput("usa_algoritmo", "Algoritmo a usar:",
                                                                                           c("Lloyd" = "Lloyd",
                                                                                             "Forgy" = "Forgy")),
                                                                               
                                                                               actionButton("apli_kmeans", "Aplicar algoritmo")),
                                                              
                                                             
                                                              
                                                              conditionalPanel(condition = "input.tip_cluster == 'c_jerarquico'", 
                                                                               
                                                                               uiOutput("var_Char"),
                                                                          
                                                                               
                                                                               selectInput("usa_distancia", "Distancia a usar:",
                                                                                           c("euclidean" = "euclidean",
                                                                                             "maximum" = "maximum", 
                                                                                             "manhattan" = "manhattan",
                                                                                             "canberra" = "canberra",
                                                                                             "binary" = "binary",
                                                                                             "minkowski" = "minkowski")),
                                                                               
                                                                               selectInput("usa_cluster", "Cluster a aplicar:",
                                                                                           c("ward.D2" = "ward.D2",
                                                                                             "single" = "single", 
                                                                                             "complete" = "complete",
                                                                                             "average" = "average",
                                                                                             "mcquitty" = "mcquitty",
                                                                                             "median" = "median",
                                                                                             "centroid" = "centroid")),
                                                                               
                                                                               
                                                                               helpText("Seleccionar el numero de agrupamientos"),
                                                                               numericInput("usa_agrupamientos", "Seleccionar agrupamientos:", value = NULL, min = 2, max = 15),
                                                                               actionButton("dibujar_agrup", "Dibujar"))
                                                              
                                                 ), # Fin sidebarPanel Analisis cluster
                                                 
                                                 mainPanel(
                                                   
                                                   conditionalPanel(condition = "input.tip_cluster == 'c_jerarquico'",
                                                                    
                                                                    fluidRow(
                                                                      
                                                                      column(width = 12, offset = 2,  br(),
                                                                             h4("Visualizacion de los grupos"),
                                                                             verbatimTextOutput("grupos")),
                                                                      
                                                                     
                                                                      column(width = 12, offset = 2,  br(),
                                                                             h4("Grafico del cluster"),
                                                                             plotOutput("cluster_jer",  "1000px"))
                                                                    )),
                                                   
                                                   conditionalPanel(condition = "input.tip_cluster == 'c_kmeans'",
                                                                    
                                                                    fluidRow(
                                                                      
                                                                      column(width = 12, offset = 1.5,
                                                                             h4("Variabilidad de los datos"),
                                                                             verbatimTextOutput("var_dat"))),
                                                                    
                                                                    br(),
                                                                    
                                                                    fluidRow(
                                                                      
                                                                      column(width = 6, offset = 0.5,
                                                                             h4("Grafico Clusters vs Varianza"),
                                                                             plotlyOutput("grafico_codo")),
                                                                      
                                                                      br(),
                                                                      
                                                                      column(width = 6, offset = 0.5,
                                                                             h4("Cluster k-means"),
                                                                             plotlyOutput("cluster_kmeans")))
                                                                    
                                                   ) # Fin conditionalPanel kmeans
                                                 ) # Fin mainPanel Analisis cluster
                                               ) # Fin del sidebarLayout Analisis cluster
                                      ) # Fin del tabPanel Cluster
                                      
                                      
                                    ) #??? Fin del tabsetPanel Analisis multivariante
                           ) #??? Fin del tabPanel Analisis multivariante
                           
                ) # Fin navbarPage
) # Fin fluidPage



server <- function(input, output) {
  
  datos_2 = data.frame()
  
  normal = eventReactive(input$creaNorm, {
    norm = rnorm(isolate(input$nNorm), isolate(input$media), isolate(input$varianza))
    
  })
  
  binomial = eventReactive(input$creaBinom, {
    binom = rbinom(isolate(input$nBin), isolate(input$tamanno), isolate(input$prob))
    
  })
  
  poiss = eventReactive(input$creaPoiss, {
    pois = rpois(isolate(input$nPois), isolate(input$lambda))
    
  }) 
  
  
  datosN = reactive({
    
    if(input$creaNorm){datos_N = cbind(datos2(), normal()); datos_N}else{NULL}
    
  })
  
  datosB = reactive({
    
    if(input$creaBinom){datos_B = cbind(binomial()); datos_B}else{NULL}
    
  })
  
  datosP = reactive({
    
    if(input$creaPoiss){datos_P = cbind(poiss()); datos_P}else{NULL}
    
  })
  
  
  datos2 = reactive({
    
    datos_2 = cbind(datosN(), datosB(), datosP())
    datos_2 = as.data.frame(datos_2)
  })
  
  datos_pred_NA = eventReactive(input$camb_NA_pred, {
    data.frame(complete(mi.datos(), isolate(input$iteraccion_num)))
  })
  
  
  
  infile = reactive({
    
    infile = input$subirDat
    
  })
  
  datos = reactive({
    
    dat = data.frame()
    
    if(is.null(infile())){dat = datos2()}else{
      
      dat = read.csv2(infile()$datapath, 
                      h = input$header, 
                      sep = input$sep, 
                      dec = input$dec,
                      quote = input$quote,
                      na.strings = "NA")}
    
    if(!is.null(infile()) & input$camb_NA_pred == T){dat = datos_pred_NA()}else{dat}
    
    if(!is.null(infile()) & input$camb_NA_media == T){
      
      nomb = colnames(dat) 
      dat_med = data.frame()
      for(i in 1:nrow(dat)){
        for(j in 1:ncol(dat)){
          if(is.na(dat[i,j]) == T){dat_med[i,j] <- round(mean(na.omit(dat[,j])), 2)
          }else{dat_med[i,j] <- dat[i,j]}
        }
      }
      colnames(dat_med) = nomb; return(dat_med)}else{dat}
    
    if(!is.null(infile()) & input$camb_NA_eliminar == T){dat = na.omit(dat)}else{dat}
    
    if(!is.null(infile()) & input$apli_logBOTON == T){
      
      dat = mutate(dat, logVar1 = log(dat[,isolate(input$varLog1)]), logVar2 = log(dat[,isolate(input$varLog2)]))
      
    }else{dat}
    
    if(!is.null(infile()) & input$apli_scaleBOTON == T){
      
      dat = mutate(dat, scaleVar1 = log(dat[,isolate(input$varScale1)]), scaleVar2 = log(dat[,isolate(input$varScale2)]))
      
    }else{dat}
    
    if(!is.null(infile()) & input$apli_cox == T){
      
      df <- dat
      v  <- names(df[,sapply(df, class) == "numeric"])
      
      for (ii in 1:length(v)){
        asimetria = skewness(df[,v[ii]])
        coef.variacion = sd(df[,v[ii]]) / mean(df[,v[ii]]) 
        
        if ((asimetria < -1 | asimetria > 1) & coef.variacion > 1){
          print(paste("transformando variable: ", v[ii]))
          df$bc_tmp <- BoxCox(df[,v[ii]],BoxCox.lambda(df[,v[ii]]))
          names(df)[names(df)=="bc_tmp"] = paste("BoxCox_", v[ii], sep = "")
        }
      }
      
      df
      
    }else{dat}
    
  }) # Falta meter lo que queda
  
  
  
  output$tabladatos = DT::renderDataTable(
    
    DT::datatable({datos()},
                  
                  filter = "top",
                  selection = "multiple"
                  
    )
  )
  
  
  # Estudio preliminar
  
    # SUMMARY Y CLASS
  
  output$summaryDa = renderPrint({
    
    summary(datos())
    
  }) # Summary de los datos
  output$classDa = renderPrint({
    
    sapply(datos(), class)
    
  }) # Clase de los datos
  
  
    # ESTUDIO NAS
  
  output$diagnostico_NA = renderPlot({
    
    if(input$diag_NA == T){
      
      aggr(datos(), prop = FALSE, 
           numbers = TRUE, border = NA,
           combine = TRUE)
      
    }else{NULL}
    
  }) # Plot matriz diagnostico NAs
  
  output$marginalDiagnostico_NA =renderPlot({
    
    if(input$diag_NA == T){
      
      marginplot(datos()[,c(input$selx,input$sely)])
      
    }else{NULL}
    
  }) # Plot diagnostico individual NAs
  
  output$selecvarXDiag_NA = renderUI({
    conditionalPanel(condition = 'input.diag_NA',
                     selectInput("selx", "Seleccionar variable X:",
                                 choices = names(datos()),
                                 selected = 1))
  }) # Variable x diagnostico individual NAs
  output$selecvarYDiag_NA = renderUI({
    conditionalPanel(condition = 'input.diag_NA',
                     selectInput("sely", "Seleccionar variable Y:",
                                 choices = names(datos()),
                                 selected = 1))
  }) # Variable y diagnostico individual NAs
  
  
  
  output$tabla_NA = renderTable({
    
    if(input$diag_NA == T){
      
      md.pattern(datos())
      
    }else{NULL}
    
  })
  
  mi.datos = reactive({
    mi.datos = mice(isolate(datos()), seed = 1234, printFlag = FALSE)
  }) 
  
  output$selec_varImp = renderUI({
    selectInput("select_var_imp", "Seleccionar variable", 
                choices = names(datos()), selected = NULL)
  })
  
  output$mids = renderPrint({
    
    mice:::print.mids(isolate(mi.datos()))
    
  })
  
  output$val_input = renderPrint({
    isolate(mi.datos())[[6]][input$select_var_imp]
  }) 
  
  
  output$media_text = renderPrint({
    
    sapply(na.omit(datos()), mean)
    
  }) # Visualizador de medias por variable
  
  output$eliminar_text = renderPrint({
    
    count(na.omit(datos()))
    
  }) # Visualizador numero de obs. a eliminar 
  
  
  # Estudio normalidad
    
      # TEST NORMALIDAD
  
  output$var_normalidad = renderUI({
    selectInput("selecVar_normtest", "Seleccionar variable:",
                choices = names(datos()), selected = NULL)
  }) # Variable a estudiar la normalidad
  
  estNorm = reactive({
    
    estNorm = datos()[,input$selecVar_normtest]
    
    estNorm = data.frame(estNorm); colnames(estNorm) = colnames(input$selecVar_normtest)
    estNorm
    
  })
  
  output$test_normPearson = renderPrint({
    pearson.test(estNorm()[,1])
  }) # Test de normalidad Pearson
  
  output$test_normSP = renderPrint({
    shapiro.test(estNorm()[,1])
  }) # Test de normalidad KS
  
  
  output$var_log1 = renderUI({
    selectInput("varLog1", "Seleccionar variable 1:",
                choices = names(datos()), selected = NULL)
  }) # Variable a logaritmizar 1
  output$var_log2 = renderUI({
    selectInput("varLog2", "Seleccionar variable 2:",
                choices = names(datos()), selected = NULL)
  }) # Variable a logaritmizar 2
  
  
  output$tabla_log1 = renderTable({
    dl = data.frame(datos()[,input$varLog1])
    logTran = data.frame(log(dl))
    
    dl = cbind(dl, logTran); colnames(dl) = c(input$varLog1, "logVar1"); head(dl, 10)
    
  }) # Tabla de logaritmizacion 1
  output$tabla_log2 = renderTable({
    dl = data.frame(datos()[, input$varLog2])
    logTran = data.frame(log(dl))
    
    dl = cbind(dl, logTran); colnames(dl) = c(input$varLog2, "logVar2"); head(dl, 10)
    
  }) # Tabla de logaritmizacion 2
  
  
  output$var_scale1 = renderUI({
    selectInput("varScale1", "Seleccionar variable 1:",
                choices = names(datos()), selected = NULL)
  }) # Variable a escalar 1
  output$var_scale2 = renderUI({
    selectInput("varScale2", "Seleccionar variable 2:",
                choices = names(datos()), selected = NULL)
  }) # Variable a escalar 2
  
  output$tabla_scale1 = renderTable({
    dl = data.frame(datos()[, isolate(input$varScale1)])
    scaleTran = data.frame(scale(dl))
    
    dl = cbind(dl, scaleTran); colnames(dl) = c(isolate(input$varScale1), "scaleVar1"); head(dl, 10)
    
  }) # Tabla de escalado 1
  output$tabla_scale2 = renderTable({
    dl = data.frame(datos()[, isolate(input$varScale2)])
    scaleTran = data.frame(scale(dl))
    
    dl = cbind(dl, scaleTran); colnames(dl) = c(isolate(input$varScale2), "scaleVar2"); head(dl, 10)
    
  }) # Tabla de escalado 2
  
  


  
  output$est_media = renderPrint({
    
    medBo = bootstrap(datos()[,input$selecVar_normtest], input$repli, mean)
    mean(medBo$thetastar)
    
  }) # Media bootstrap
  output$est_varianza = renderPrint({
    
    medBo = bootstrap(datos()[,input$selecVar_normtest], input$repli, var)
    mean(medBo$thetastar)
    
  }) # Varianza bootstrap
  
  
  
  icBoot = reactive({
    mean.boot = function(x,ind){
      return(c(mean(x[ind]), var(x[ind])))
    }
    eso = boot(datos()[,input$selecVar_normtest], mean.boot, input$repli)
    eso
  })
  output$ic_est_media = renderPrint({
    boot.ci(icBoot(), index = 1, conf = input$confianza)
  }) # IC para media bootstrap 
  output$ic_est_varianza = renderPrint({
    boot.ci(icBoot(), index = 2, conf = input$confianza)
  }) # IC para varianza bootstrap
  
  
  output$var_ind1 = renderUI({
    selectInput("varInd1", "Seleccionar variable 1:", 
                choices = names(datos()), selected = NULL)
  }) # Variable 1 para independencia
  output$var_ind2 = renderUI({
    selectInput("varInd2", "Seleccionar variable 2:", 
                choices = names(datos()), selected = NULL)
  }) # Variable 2 para independencia
  
  
  output$intervalos_v1 = renderPrint({
    
    r = range(datos()[,input$varInd1])
    cs = nclass.Sturges(datos()[,input$varInd1])
    li = seq(r[1], r[2], length = cs)
    
    intervalos = cut(datos()[,input$varInd1], breaks = li, include.lowest = T)
    
    int = data.frame(table(intervalos)); colnames(int) = c("Intervalos", "Frecuencia")
    int
  })
  output$intervalos_v2 = renderPrint({
    
    r = range(datos()[,input$varInd2])
    cs = nclass.Sturges(datos()[,input$varInd2])
    li = seq(r[1], r[2], length = cs)
    
    intervalos = cut(datos()[,input$varInd2], breaks = li, include.lowest = T)
    
    int = data.frame(table(intervalos)); colnames(int) = c("Intervalos", "Frecuencia")
    int
  })
  
  output$prop_v1 = renderPrint({
    
    r = range(datos()[,input$varInd1])
    cs = nclass.Sturges(datos()[,input$varInd1])
    li = seq(r[1], r[2], length = cs)
    
    intervalos = cut(datos()[,input$varInd1], breaks = li, include.lowest = T)
    
    int = data.frame(table(intervalos)); colnames(int) = c("Intervalos", "Frecuencia")
    proporciones = cbind.data.frame(int$Frecuencia / sum(int$Frecuencia), (int$Frecuencia / sum(int$Frecuencia)*100)); colnames(proporciones) = c("Proporciones", "Porcentaje")
    
    proporciones
    
  })
  output$prop_v2 = renderPrint({
    
    r = range(datos()[,input$varInd2])
    cs = nclass.Sturges(datos()[,input$varInd2])
    li = seq(r[1], r[2], length = cs)
    
    intervalos = cut(datos()[,input$varInd2], breaks = li, include.lowest = T)
    
    int = data.frame(table(intervalos)); colnames(int) = c("Intervalos", "Frecuencia")
    proporciones = cbind.data.frame(int$Frecuencia / sum(int$Frecuencia), (int$Frecuencia / sum(int$Frecuencia)*100)); colnames(proporciones) = c("Proporciones", "Porcentaje")
    
    proporciones
  })
  
  output$plot_v1 = renderPlotly({
    
    r = range(datos()[,input$varInd1])
    cs = nclass.Sturges(datos()[,input$varInd1])
    li = seq(r[1], r[2], length = cs)
    
    intervalos = cut(datos()[,input$varInd1], breaks = li, include.lowest = T)
    
    int = data.frame(table(intervalos)); colnames(int) = c("Intervalos", "Frecuencia")
    
    ggplot(int, aes(Intervalos, Frecuencia)) + 
      geom_bar(stat = 'identity', width = 0.10) +
      ggtitle("Grafico de recuentos") +
      xlab(input$varInd1) + 
      ylab("Recuentos") +
      theme_bw()
    
  }) # Plot recuentos 1
  output$plot_v2 = renderPlotly({
    
    r = range(datos()[,input$varInd2])
    cs = nclass.Sturges(datos()[,input$varInd2])
    li = seq(r[1], r[2], length = cs)
    
    intervalos = cut(datos()[,input$varInd2], breaks = li, include.lowest = T)
    
    int = data.frame(table(intervalos)); colnames(int) = c("Intervalos", "Frecuencia")
    
    ggplot(int, aes(Intervalos, Frecuencia)) + 
      geom_bar(stat = 'identity', width = 0.10) +
      ggtitle("Grafico de recuentos") +
      xlab(input$varInd2) + 
      ylab("Recuentos") +
      theme_bw()
    
  }) # Plot recuentos 2
  
  
  
  output$controles1 = renderUI({
    
    selectInput("selectvarX", "Seleccionar variable X:",
                choices = names(datos()),
                selected = 1)
  }) # Tiene que estar siempre
  
  output$controles2 = renderUI({
    
    selectInput("selectvarY", "Seleccionar variable Y:",
                choices = names(datos()),
                selected = 1)
  }) # Tiene que estar siempre
  
  output$plotdisp = renderPlotly({
    
    p = ggplot(datos(), aes(x = datos()[,input$selectvarX], y = datos()[,input$selectvarY])) + 
      xlab(input$selectvarX) +
      ylab(input$selectvarY)
    
    
    if(input$aplicarlogx == T){
      
      p = ggplot(datos(), aes(x = log(datos()[,input$selectvarX]), y = datos()[,input$selectvarY])) + 
        xlab(paste("log_", input$selectvarX, sep = "")) +
        ylab(input$selectvarY)
      
      p + geom_point(color = "navyblue", size = 1) + 
        theme_bw() 
      
    }else {
      
      p + geom_point(color = "navyblue", size = 1) + 
        theme_bw()
    }
    
    if(input$aplicarlogy == T){
      
      p = ggplot(datos(), aes(x = datos()[,input$selectvarX], y = log(datos()[,input$selectvarY]))) + 
        ylab(paste("log_", input$selectvarY, sep = "")) +
        xlab(input$selectvarY)  
      
      p + geom_point(color = "navyblue", size = 1) + 
        theme_bw()
      
    }else {
      
      p + geom_point(color = "navyblue", size = 1) + 
        theme_bw()
    }
    
    if(input$aplicarlogy == T & input$aplicarlogx == T){
      
      p = ggplot(datos(), aes(x = log(datos()[,input$selectvarX]), y = log(datos()[,input$selectvarY])))+ 
        xlab(paste("log_", input$selectvarX, sep = "")) +
        ylab(paste("log_", input$selectvarY, sep = ""))
      
      p + geom_point(color = "navyblue", size = 1) + 
        theme_bw()
      
    }else {
      
      p + geom_point(color = "navyblue", size = 1) + 
        theme_bw()
    }
    
    if(input$aplicarReg == TRUE){
      
      p + geom_point(color = "navyblue", size = 1) + 
        geom_smooth(method = "lm",se = T, colour = "brown3", fill = 0.32) +
        theme_bw()
      
      
    }else{
      
      p + geom_point(color = "navyblue", size = 1) + 
        theme_bw()
    }
    
  }) # Tiene que estar siempre ############ PROBLEMA AQUI
  
  
  
  
  output$controlesApliReg = renderUI({
    
    
    checkboxInput("aplicarReg",
                  "Aplicar regresion")
    
    
  }) # Tiene que estar siempre
  
  
  output$controlesResidLS = renderUI({
    
    conditionalPanel(
      condition = "input.tipReg == 'ls'",
      checkboxInput("visResid",
                    "Visualizar residuos"))
    
  }) # Solo en RLS
  
  
  output$smLS = renderUI({
    
    conditionalPanel(condition = "input.tipReg == 'ls' && 'input.aplicarReg'",
                     br(), br(),
                     h4("Regresion lineal simple"),
                     verbatimTextOutput("summaryregLS"))
    
  }) 
  
  datRLS = reactive({
    
    dat = cbind.data.frame(datos()[,input$selectvarY], datos()[,input$selectvarX])
    colnames(dat) = c(input$selectvarY, input$selectvarX)
    if(input$aplicarlogx == T){
      
      colnames(dat) = c(input$selectvarY, paste("log_", input$selectvarX, sep = ""))
      
    }else{colnames(dat) = c(input$selectvarY, input$selectvarX)}
    if(input$aplicarlogy == T){
      
      colnames(dat) = c(paste("log_", input$selectvarY, sep = ""), input$selectvarX)
      
    }else{colnames(dat) = c(input$selectvarY, input$selectvarX)}
    if(input$aplicarlogx == T & input$aplicarlogy == T){
      
      colnames(dat) = c(paste("log_", input$selectvarY, sep = ""), paste("log_", input$selectvarX, sep = ""))
      
    }else{colnames(dat) = c(input$selectvarY, input$selectvarX)}
    
    dat
  
    })
  
  regRLS = reactive({
    
    if(input$aplicarReg == TRUE){
      
      reg = lm(datRLS()[,1] ~ ., data = datRLS())
      
    }else{
      
      return(NULL)
      
    }
    
    if(input$aplicarlogx == T){
      
      reg = lm(datRLS()[,1] ~ log(datRLS()[,2]), data = datRLS())
      
    }else{
      
      reg = lm(datRLS()[,1] ~ datRLS()[,2], data = datRLS())
      
    }
    
    if(input$aplicarlogy == T){
      
      reg = lm(log(datRLS()[,1]) ~ datRLS()[,2], data = datRLS())
      
    }else{
      
      reg = lm(datRLS()[,1] ~ datRLS()[,2], data = datRLS())
      
    }
    
    if(input$aplicarlogy == T & input$aplicarlogx == T){
      
      reg = lm(log(datRLS()[,1]) ~ log(datRLS()[,2]), data = datRLS())
      
    }else{
      
      reg = lm(datRLS()[,1] ~ datRLS()[,2], data = datRLS())
      
    }
    
  })
  
  output$summaryregLS = renderPrint({
    
    summary(regRLS())
    
  }) # Solo en RLS
  
  output$smLOE = renderUI({
    
    conditionalPanel(condition = "input.tipReg == 'loess' && 'input.aplicarReg'",
                     br(), br(),
                     h4("Regresion lineal simple"),
                     verbatimTextOutput("summaryregLOE"))
    
  }) # LOESS conditionalPanel sm
  
  output$summaryregLOE = renderPrint({
    
    if(input$aplicarReg == TRUE){
      
      reg = loess(datos()[,input$selectvarY] ~ datos()[,input$selectvarX], span = input$span)
      summary(reg)
      
    }else{
      
      return(NULL)
      
    }
    
    if(input$aplicarlogx == T){
      
      reg = loess(datos()[,input$selectvarY] ~ log(datos()[,input$selectvarX]), span = input$span)
      summary(reg)
      
    }else{
      
      reg = loess(datos()[,input$selectvarY] ~ datos()[,input$selectvarX], span = input$span)
      summary(reg)
      
    }
    
    if(input$aplicarlogy == T){
      
      reg = loess(log(datos()[,input$selectvarY]) ~ datos()[,input$selectvarX], span = input$span)
      summary(reg)
      
    }else{
      
      reg = loess(datos()[,input$selectvarY] ~ datos()[,input$selectvarX], span = input$span)
      summary(reg)
      
    }
    
    if(input$aplicarlogy == T & input$aplicarlogx == T){
      
      reg = loess(log(datos()[,input$selectvarY]) ~ log(datos()[,input$selectvarX]), span = input$span)
      summary(reg)
      
    }else{
      
      reg = loess(datos()[,input$selectvarY] ~ datos()[,input$selectvarX], span = input$span)
      summary(reg)
      
    }
    
    
  }) # LOESS verbatimtextOutput summary
  
  
  output$plResidLS = renderUI({
    
    conditionalPanel(condition = "input.tipReg == 'ls' && 'input.aplicarReg'",
                     br(), br(),
                     h4("Variable X vs residuos"),
                     plotlyOutput("plotResidLS", width = "500px"))
    
  })
  output$plotResidLS = renderPlotly({
    
    if(input$visResid){
      
      reg = lm(datos()[,input$selectvarY] ~ datos()[,input$selectvarX], data = datos())
      residuos = data.frame(residuals(reg)); residuos = data.frame(residuos[,1]);
      
      p = ggplot(data = datos(), aes(datos()[,input$selectvarX], residuos[,1])) + 
        xlab(input$selectvarX) + 
        ylab("Residuos")
      
      pp = p + geom_point() + geom_smooth(method = "loess", color = "brown3") +
        geom_hline(yintercept = 0) +
        theme_bw()
      
      
    }else{return(NULL)}
    
    if(input$visResid & input$aplicarlogx){
      
      reg = lm(datos()[,input$selectvarY] ~ log(datos()[,input$selectvarX]), data = datos())
      residuos = data.frame(residuals(reg)); residuos = data.frame(residuos[,1]);
      
      p = ggplot(data = datos(), aes(datos()[,input$selectvarX], residuos[,1])) + 
        xlab(input$selectvarX) + 
        ylab("Residuos")
      
      pp = p + geom_point() + geom_smooth(method = "loess", color = "brown3") +
        geom_hline(yintercept = 0) +
        theme_bw()
      
    }else{
      
      reg = lm(datos()[,input$selectvarY] ~ datos()[,input$selectvarX], data = datos())
      residuos = data.frame(residuals(reg)); residuos = data.frame(residuos[,1]);
      
      p = ggplot(data = datos(), aes(datos()[,input$selectvarX], residuos[,1])) + 
        xlab(input$selectvarX) + 
        ylab("Residuos")
      
      pp = p + geom_point() + geom_smooth(method = "loess", color = "brown3") +
        geom_hline(yintercept = 0) +
        theme_bw()
    }
    
    if(input$visResid &input$aplicarlogy){
      
      reg = lm(log(datos()[,input$selectvarY]) ~ datos()[,input$selectvarX], data = datos())
      residuos = data.frame(residuals(reg)); residuos = data.frame(residuos[,1]);
      
      p = ggplot(data = datos(), aes(datos()[,input$selectvarX], residuos[,1])) + 
        xlab(input$selectvarX) + 
        ylab("Residuos")
      
      pp = p + geom_point() + geom_smooth(method = "loess", color = "brown3") +
        geom_hline(yintercept = 0) +
        theme_bw()
      
    }else{
      
      reg = lm(datos()[,input$selectvarY] ~ datos()[,input$selectvarX], data = datos())
      residuos = data.frame(residuals(reg)); residuos = data.frame(residuos[,1]);
      
      p = ggplot(data = datos(), aes(datos()[,input$selectvarX], residuos[,1])) + 
        xlab(input$selectvarX) + 
        ylab("Residuos")
      
      pp = p + geom_point() + geom_smooth(method = "loess", color = "brown3") +
        geom_hline(yintercept = 0) +
        theme_bw()
      
    }
    
    if(input$visResid & input$aplicarlogy & input$aplicarlogy){
      
      reg = lm(log(datos()[,input$selectvarY]) ~ log(datos()[,input$selectvarX]), data = datos())
      residuos = data.frame(residuals(reg)); residuos = data.frame(residuos[,1]);
      
      p = ggplot(data = datos(), aes(datos()[,input$selectvarX], residuos[,1])) + 
        xlab(input$selectvarX) + 
        ylab("Residuos")
      
      pp = p + geom_point() + geom_smooth(method = "loess", color = "brown3") +
        geom_hline(yintercept = 0) +
        theme_bw()
      
    }else{
      
      reg = lm(datos()[,input$selectvarY] ~ datos()[,input$selectvarX], data = datos())
      residuos = data.frame(residuals(reg)); residuos = data.frame(residuos[,1]);
      
      p = ggplot(data = datos(), aes(datos()[,input$selectvarX], residuos[,1])) + 
        xlab(input$selectvarX) + 
        ylab("Residuos")
      
      pp = p + geom_point() + geom_smooth(method = "loess", color = "brown3") +
        geom_hline(yintercept = 0) +
        theme_bw()
      
    }
    
  }) # Solo en RLS
  
  output$plResidLOE = renderUI({
    
    conditionalPanel(condition = "input.tipReg == 'loess' && 'input.aplicarReg'",
                     br(), br(),
                     h4("Variable X vs residuos"),
                     plotlyOutput("plotResidLOE", width = "500px"))
    
  }) # LOESS conditionalPanel plResid
  
  output$hisResidLOE = renderUI({
    conditionalPanel(condition = "input.tipReg == 'loess' && 'input.aplicarReg'",
                     br(), br(),
                     h4("Distribucion de los residuos"),
                     plotlyOutput("histResidLOE", width = "500px"))
    
    
  }) # LOESS conditionalPanel hisResid
  
  output$hisResidLS = renderUI({
    conditionalPanel(condition = "input.tipReg == 'ls' && 'input.aplicarReg'",
                     br(), br(),
                     h4("Distribucion de los residuos"),
                     plotlyOutput("histResidLS", width = "500px"))
    
    
  }) 
  output$histResidLS = renderPlotly({
    
    if(input$visResid){
      
      reg = lm(datos()[,input$selectvarY] ~ datos()[,input$selectvarX], data = datos())
      residuos = data.frame(residuals(reg)); residuos = data.frame(residuos[,1]);
      cs = nclass.Sturges(residuos[,1])
      
      bar = seq(min(residuos[,1]), max(residuos[,1]), length.out = cs+1)
      
      p = ggplot(residuos, aes(residuos[,1])) + xlab("Residuos") + ylab("Densidad")
      
      pp = p + geom_histogram(aes(y = ..density..), 
                              breaks = bar, 
                              fill = '#75AADB',
                              colour = "black", 
                              alpha = 0.32) +
        geom_density(kernel = "gaussian", 
                     col = "brown3",
                     linetype = 1,
                     size = 0.75) +
        
        theme_bw()
      
      
    }else{return(NULL)}
    
    if(input$visResid & input$aplicarlogx){
      
      reg = lm(datos()[,input$selectvarY] ~ log(datos()[,input$selectvarX]), data = datos())
      residuos = data.frame(residuals(reg)); residuos = data.frame(residuos[,1]);
      
      cs = nclass.Sturges(residuos[,1]) 
      bar = seq(min(residuos[,1]), max(residuos[,1]), length.out = cs+1)
      
      p = ggplot(residuos, aes(residuos[,1])) + xlab("Residuos") + ylab("Densidad")
      
      pp = p + geom_histogram(aes(y = ..density..), 
                              breaks = bar, 
                              fill = '#75AADB',
                              colour = "black", 
                              alpha = 0.32) +
        geom_density(kernel = "gaussian", 
                     col = "brown3",
                     linetype = 1,
                     size = 0.75) +
        
        theme_bw()
      
    }else{
      
      reg = lm(datos()[,input$selectvarY] ~ datos()[,input$selectvarX], data = datos())
      residuos = data.frame(residuals(reg)); residuos = data.frame(residuos[,1]);
      cs = nclass.Sturges(residuos[,1])
      
      bar = seq(min(residuos[,1]), max(residuos[,1]), length.out = cs+1)
      
      p = ggplot(residuos, aes(residuos[,1])) + xlab("Residuos") + ylab("Densidad")
      
      pp = p + geom_histogram(aes(y = ..density..), 
                              breaks = bar, 
                              fill = '#75AADB',
                              colour = "black", 
                              alpha = 0.32) +
        geom_density(kernel = "gaussian", 
                     col = "brown3",
                     linetype = 1,
                     size = 0.75) +
        
        theme_bw()
    }
    
    if(input$visResid &input$aplicarlogy){
      
      reg = lm(log(datos()[,input$selectvarY]) ~ datos()[,input$selectvarX], data = datos())
      residuos = data.frame(residuals(reg)); residuos = data.frame(residuos[,1]);
      
      cs = nclass.Sturges(residuos[,1]) 
      bar = seq(min(residuos[,1]), max(residuos[,1]), length.out = cs+1)
      
      p = ggplot(residuos, aes(residuos[,1])) + xlab("Residuos") + ylab("Densidad")
      
      pp = p + geom_histogram(aes(y = ..density..), 
                              breaks = bar, 
                              fill = '#75AADB',
                              colour = "black", 
                              alpha = 0.32) +
        geom_density(kernel = "gaussian", 
                     col = "brown3",
                     linetype = 1,
                     size = 0.75) +
        
        theme_bw()
      
    }else{
      
      reg = lm(datos()[,input$selectvarY] ~ datos()[,input$selectvarX], data = datos())
      residuos = data.frame(residuals(reg)); residuos = data.frame(residuos[,1]);
      cs = nclass.Sturges(residuos[,1])
      
      bar = seq(min(residuos[,1]), max(residuos[,1]), length.out = cs+1)
      
      p = ggplot(residuos, aes(residuos[,1])) + xlab("Residuos") + ylab("Densidad")
      
      pp = p + geom_histogram(aes(y = ..density..), 
                              breaks = bar, 
                              fill = '#75AADB',
                              colour = "black", 
                              alpha = 0.32) +
        geom_density(kernel = "gaussian", 
                     col = "brown3",
                     linetype = 1,
                     size = 0.75) +
        
        theme_bw()
      
    }
    
    if(input$visResid & input$aplicarlogy & input$aplicarlogy){
      
      reg = lm(log(datos()[,input$selectvarY]) ~ log(datos()[,input$selectvarX]), data = datos())
      residuos = data.frame(residuals(reg)); residuos = data.frame(residuos[,1]);
      
      cs = nclass.Sturges(residuos[,1]) 
      bar = seq(min(residuos[,1]), max(residuos[,1]), length.out = cs+1)
      
      p = ggplot(residuos, aes(residuos[,1])) + xlab("Residuos") + ylab("Densidad")
      
      pp = p + geom_histogram(aes(y = ..density..), 
                              breaks = bar, 
                              fill = '#75AADB',
                              colour = "black", 
                              alpha = 0.32) +
        geom_density(kernel = "gaussian", 
                     col = "brown3",
                     linetype = 1,
                     size = 0.75) +
        
        theme_bw()
      
    }else{
      
      reg = lm(datos()[,input$selectvarY] ~ datos()[,input$selectvarX], data = datos())
      residuos = data.frame(residuals(reg)); residuos = data.frame(residuos[,1]);
      cs = nclass.Sturges(residuos[,1])
      
      bar = seq(min(residuos[,1]), max(residuos[,1]), length.out = cs+1)
      
      p = ggplot(residuos, aes(residuos[,1])) + xlab("Residuos") + ylab("Densidad")
      
      pp = p + geom_histogram(aes(y = ..density..), 
                              breaks = bar, 
                              fill = '#75AADB',
                              colour = "black", 
                              alpha = 0.32) +
        geom_density(kernel = "gaussian", 
                     col = "brown3",
                     linetype = 1,
                     size = 0.75) +
        
        theme_bw()
      
    }
    
  }) # Solo en RLS
  
  
  
  
  
  output$controles3 = renderUI({
    
    radioButtons("varesc", "Seleccion de variables:",
                 choices = names(datos()),
                 selected = NULL)
    
  })
  
  output$trazaQuan = renderUI({
    
    checkboxInput("aplicarQuan",
                  "Aplicar linea de cuantiles y media")
    
  })
  
  output$plotdens = renderPlotly({
    
    if(input$aplicarQuan){
      
      if(class(datos()[,input$varesc]) == "numeric"){
        
        x = ggplot(datos(), aes(get(input$varesc))) + xlab(input$varesc) + ylab("Densidad")
        
        x + geom_density(fill = '#75AADB', alpha = 0.32)  + 
          geom_vline(aes(xintercept = mean(datos()[,input$varesc])), col = "red", size = 0.75) +  
          geom_vline(aes(xintercept = quantile(datos()[,input$varesc])[2]), linetype = 2, col = "springgreen4", size = 0.75) +
          geom_vline(aes(xintercept = quantile(datos()[,input$varesc])[4]), linetype = 2, col = "springgreen4", size = 0.75) +
          geom_vline(aes(xintercept = quantile(datos()[,input$varesc])[3]), linetype = 2, col = "springgreen4", size = 0.75) +
          theme_bw()}else{NULL}
      
    }else{
      
      if(class(datos()[,input$varesc]) == "numeric"){
        x = ggplot(datos(), aes(get(input$varesc))) + xlab(input$varesc) + ylab("Densidad")
        x + geom_density(fill = '#75AADB', alpha = 0.32)  + theme_bw()}else{NULL}
    }
    
  })
  
  output$controles4 = renderUI({
    
    radioButtons("varescHist", "Seleccion de variables:",
                 choices = names(datos()),
                 selected = NULL)
    
  })
  
  output$controles5 = renderUI({
    
    sliderInput("barras", 
                label = h4("Seleccionar numero de barras:"),
                min = 1, max = 50, value = 30)
    
    
  })
  
  output$controles6 = renderUI({
    
    checkboxInput("trazLinea",
                  "Trazar linea")
    
    
  })
  
  output$histograma = renderPlotly({
    
    bar = seq(min(datos()[,input$varescHist]), max(datos()[,input$varescHist]), length.out = input$barras + 1)
    
    
    if(input$trazLinea){
      
      ggplot(datos(), aes(get(input$varescHist))) + 
        
        geom_histogram(position = "identity", 
                       breaks = bar,   
                       fill = '#75AADB',
                       colour = "black", 
                       alpha = 0.5) + 
        
        theme_bw() + 
        
        geom_freqpoly(breaks = bar,
                      colour = "brown3", 
                      size = 0.75)
    }else{
      
      ggplot(datos(), aes(get(input$varescHist))) + 
        
        geom_histogram(position = "identity", 
                       breaks = bar, 
                       fill = '#75AADB',
                       colour = "black", 
                       alpha = 0.5) + 
        
        theme_bw()
      
      
    }
    
    
  })
  
  output$var1 = renderUI({
    
    selectInput("selectvar1", "Seleccionar variable X:",
                choices = names(datos()),
                selected = 1)
  })
  
  output$var2 = renderUI({
    
    selectInput("selectvar2", "Seleccionar variable Y:",
                choices = names(datos()),
                selected = 1)
  })
  
  
  output$cortest = renderPrint({
    
    cor.test(datos()[,input$selectvar1],datos()[,input$selectvar2])    
    
  })
  
  output$ttest = renderPrint({
    
    t.test(datos()[,input$selectvar1],datos()[,input$selectvar2])    
    
  })
  
  output$vartest = renderPrint({
    
    var.test(datos()[,input$selectvar1],datos()[,input$selectvar2])    
    
  })
  
  
  
  # Analisis cluster

      # Cluster jerarquico

  datos_noNum = reactive({
    
    datos.noNum = datos()[,sapply(datos(), class) != "numeric"]
    datos.noNum
    
  }) # Datos con variables no numericas
  
  output$var_Char = renderUI({
    selectInput("varChar", "Seleccionar nombre de filas:",
                choices = names(datos_noNum()),
                selected = 1)
  })
  
  datos_cj = reactive({
    
    datosCJ = datos()[,sapply(datos(), class) == "numeric"]
    datosCJ; rownames(datosCJ) = c(datos()[,1]); datosCJ
    
  }) # Datos con variables numericas
  
  
  
  distancia = reactive({
    
    distancia = dist(datos_cj(), method = input$usa_distancia)
    
  })
  
  output$grupos = renderPrint({
    
    fit = hclust(distancia(), method = input$usa_cluster)
    grupo = cutree(fit, k = input$usa_agrupamientos) 
    grupo = data.frame(grupo) 
    
    if(input$dibujar_agrup == T){t(grupo)}else{print("A la espera de seleccionar agrupamientos")}
    
  }) # Visualizador matriz de distancias
  
  output$cluster_jer = renderPlot({
    
    fit = hclust(distancia(), method = input$usa_cluster)
    plot(fit, cex = 0.7) 
    
    if(input$dibujar_agrup == T){plot(fit,  cex = 0.7); rect.hclust(fit, k = input$usa_agrupamientos, border="red")}else{plot(fit,  cex = 0.7)}
    
  }) # Cluster jerarquico
  
  
      # Cluster kmeans
  
  output$varX_clus = renderUI({
    selectInput("varXclus", "Seleccionar variable X:",
                choices = names(datos_cj()),
                selected = 1)
  })
  output$varY_clus = renderUI({
    selectInput("varYclus", "Seleccionar variable Y:",
                choices = names(datos_cj()),
                selected = 1)
  })
  
  
  datos_sc = eventReactive(input$dscale, {
    data.frame(scale(datos_cj()))
  })
  
  variabilidad = reactive({
    
    wss = (nrow(datos_sc()) - 1)*sum(apply(datos_sc(), 2, var))
    for (i in 2:input$num_var) wss[i] <- sum(kmeans(datos_sc(), centers = i)$withinss)
    wss
    
  })
  
  output$var_dat = renderPrint({
    variabilidad()
  })
  
  output$grafico_codo = renderPlotly({
    
    wss = cbind.data.frame(Clusters = c(1:input$num_var), Varianza = variabilidad())
    ggplot(wss, aes(x = Clusters, y = Varianza, group = 1)) +
      geom_point(stat = 'summary', fun.y = sum, col = "steelblue4", size = 1.25) +
      stat_summary(fun.y = sum, geom = "line", col = "steelblue4", size = 0.5) +
      theme_bw()
  })
  
  km = eventReactive(input$apli_kmeans, {
    
    if(input$dscale == T){
      km2 = kmeans(datos_sc(), centers = input$num_centros, nstart = input$nstart,  algorithm = input$usa_algoritmo)}
    else{km2 = kmeans(datos(), centers = input$num_centros, nstart = input$nstart,  algorithm = input$usa_algoritmo)}
    
    km2$cluster = as.factor(km2$cluster) 
    km = km2$cluster; km
    
  })
  
  output$cluster_kmeans = renderPlotly({
    
    if(input$apli_kmeans == T){
      p = ggplot(datos(), aes(x = input$varXclus, y = input$varYclus, color = km())) 
      p + geom_point() + theme_bw()}else{NULL}
    
  }) # Hay que revisarlo
  
  
  # REGRESION LINEAL MULTIPLE Y CORRELACION
  
        # CORRELACION
  
  output$vxCorr = renderUI({
    selectInput("vx_Corr", "Seleccionar variable X:",
                choices = names(datos_cj()),
                selected = 1)
  }) 
  output$vyCorr = renderUI({
    selectInput("vy_Corr", "Seleccionar variable Y:",
                choices = names(datos_cj()),
                selected = 1)
  })
  
  output$corrTestPear = renderPrint({
    
    cor.test(datos_cj()[,input$vx_Corr],datos()[,input$vy_Corr])    
    
  })
  output$corrPlot = renderPlot({
    corrplot(cor(datos_cj()), method = "number")
  })
  
  output$rm_dep = renderUI({
    selectInput("rmDep", "Seleccionar variable dependiente:",
                choices = names(datos_cj()),
                selected = 1)
  }) # Variable y RLM
  
  output$rm_Vars = renderUI({
    checkboxGroupInput("rmVars", "Seleccionar variables independientes:", 
                       choices = names(datos_cj()), selected = NULL)
  }) # Variables x RLM
  
  datReg = reactive({
    
    dep = datos_cj()[,input$rmDep]; colnames(dep) = colnames(datos_cj()[,input$rmDep])
    ind = datos_cj()[,input$rmVars]; colnames(ind) = colnames(datos_cj()[,input$rmVars])
    
    dReg = cbind.data.frame(dep, ind)
    
  })
  
  regMul = reactive({
    lm(dep ~ ., datReg())
  })
  
  output$sumRegMul = renderPrint({
    
    summary(regMul())
    
  })
  output$ICregMul = renderPrint({
    
    confint(regMul())
    
  })
  
  output$FIV = renderPrint({
    
    FIV_ <- function(X){
      
      observaciones = dim(X)[1]
      variables = dim(X)[2]
      
      fiv = array(0,variables)
      
      for (i in 1:variables) {
        
        reg_aux = lm(X[,i] ~ X[,-i])
        R2 = as.numeric(summary(reg_aux)[8])
        fiv[i] = 1/(1-R2)
        
      }
      return(fiv)
    }
    
    FIV_(as.matrix(datReg()))
    
  })
  
  output$NC = renderPrint({
    
    NC_ <- function(X){
      
      XX = crossprod(X)
      autovalores = eigen(XX)[[1]]
      auto_min = min(autovalores)
      auto_max = max(autovalores)
      nc = sqrt(auto_max/auto_min)
      return(nc)
      
    }
    
    
    
    longitud_unidad <- function(X){
      
      observaciones = dim(X)[1]
      variables = dim(X)[2]
      
      Xlu = array(,c(observaciones, variables))
      for (i in 1:variables) {
        for (j in 1:observaciones) {
          Xlu[j,i] = X[j,i]/sqrt(crossprod(X[,i]))
        }
      }
      return(Xlu)
    }
    
    observaciones = dim(datReg())[1]
    cte = rep(1, observaciones) 
    matriz = cbind(cte,as.matrix(datReg())) 
    Xlu = longitud_unidad(matriz)
    NC_(Xlu)
    
  })
  
  
}

shinyApp(ui = ui, server = server)