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

ui <- fluidPage(theme = shinytheme("cosmo"),
                
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
                                                 
                                                 mainPanel(br(),
                                                           
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
                                                              
                                                              conditionalPanel(condition = 'input.diag_NA',
                                                                               
                                                                               radioButtons("limpia_NA", "Sustituir los NA por:",
                                                                                            inline = T,
                                                                                            select = NULL, 
                                                                                            choices = c("Media" = "media_NA",
                                                                                                        "Predicciones" = "pred_NA",
                                                                                                        "Eliminar" = "eliminar_NA"))
                                                                               
                                                              ),
                                                              
                                                              fluidRow(h5("Distr. normal"),
                                                                       
                                                                       column(width = 12,
                                                                              sliderInput("nNorm", width = '450px',
                                                                                          label = h5("Muestra"),
                                                                                          min = 1, max = 1000, value = 500, step = 1))
                                                                       
                                                                       
                                                              ), # fin fluidRow Dist.Normal (muestra)
                                                              
                                                              fluidRow(
                                                                
                                                                column(width = 5, offset = 0.1,
                                                                       numericInput("media", width = '100px',
                                                                                    label = h5("Media"), 
                                                                                    value = 0)),
                                                                
                                                                column(width = 5, offset = 0.1,
                                                                       numericInput("varianza", width = '100px', 
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
                                                                
                                                                column(width = 5, offset = 0.1,
                                                                       numericInput("tamanno", width = '100px', 
                                                                                    label = h5("Tamanno"), 
                                                                                    value = 10,
                                                                                    max = 1000, min = 0)),
                                                                
                                                                column(width = 5, offset = 0.1,
                                                                       numericInput("prob", width = '100px', 
                                                                                    label = h5("Probabilidad"), 
                                                                                    value = 0.5, step = 0.1,
                                                                                    max = 1, min = 0)),
                                                                
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
                                                                
                                                                column(width = 5, offset = 0.1,
                                                                       numericInput("lambda", width = '100px',
                                                                                    label = h5("Lambda"), 
                                                                                    value = 1)),
                                                                
                                                                column(width = 4, offset = 0.1,
                                                                       h5("Accion"),
                                                                       actionButton("creaPoiss", "Generar", width = '75px'))
                                                                
                                                              ) # fin fluidRow Dist.Poisson (lambda y generar)
                                                              
                                                              
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
                                                   
                                                   conditionalPanel(condition = 'input.diag_NA',
                                                                    
                                                                    fluidRow(
                                                                      
                                                                      h4("Diagnostico NA"),
                                                                      
                                                                      br(),
                                                                      
                                                                      column(width = 5, offset = 0.25,
                                                                             plotOutput("diagnostico_NA", width = "500px")),
                                                                      
                                                                      
                                                                      column(width = 5, offset = 0.25,
                                                                             plotOutput("marginalDiagnostico_NA", width = "500px")),
                                                                      
                                                                      column(width = 2, offset = 0.25,
                                                                             uiOutput("selecvarXDiag_NA"),
                                                                             uiOutput("selecvarYDiag_NA"))
                                                                    ),
                                                                    
                                                                    br(),
                                                                    
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
                                                                    
                                                   )
                                                   
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
                                                                               p("Pulsar el boton para annadir variables."), 
                                                                               actionButton("apli_logBOTON", "Aplicar")), 
                                                              
                                                              hr(),
                                                              
                                                              br(),
                                                              
                                                              checkboxInput("scale_apli", "Aplicar escalado", value = F),
                                                              
                                                              conditionalPanel(condition = 'input.scale_apli',
                                                                               helpText("Seleccionar variables a escalar."),
                                                                               uiOutput("var_scale1")),
                                                              
                                                              conditionalPanel(condition = 'input.scale_apli',
                                                                               p("Pulsar el boton para annadir variable."), 
                                                                               actionButton("apli_scaleBOTON", "Aplicar")),
                                                              
                                                              hr(),
                                                              
                                                              br(), 
                                                              
                                                              checkboxInput("boxcox", "Transformaciones Box Cox", value = F),
                                                              
                                                              conditionalPanel(condition = 'input.boxcox', 
                                                                               helpText("Al aplicar la transformacion se crearan nuevas variables para todas
                                                                                        aquellas que necesiten la transformacion"),
                                                                               
                                                                               actionButton("apli_cox", "Aplicar")),
                                                              
                                                              
                                                              hr(),
                                                              
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
                                                     
                                                     conditionalPanel(condition = 'input.scale_apli',
                                                                      column(width = 5, offset = 0.5,
                                                                             br(),
                                                                             h4("Variable 1 con el escalado aplicado"), 
                                                                             tableOutput("tabla_scale1")))
                                                     
                                                     # Aqui tendra que ir la tabla del escalado.
                                                     
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
                                      
                                      tabPanel("Intervalos y recuentos",
                                               
                                               sidebarLayout(
                                                 
                                                 sidebarPanel(width = 2,
                                                              
                                                              helpText("Seleccionar variables para agrupar en intervalos."),
                                                              
                                                              uiOutput("var_ind1"), 
                                                              
                                                              uiOutput("var_ind2"),
                                                              
                                                              checkboxInput("prop", "Mostrar proporciones muestrales"), br(),
                                                              
                                                              helpText("Notese que se emplea la metodologia bootstrap para estimar las proporciones poblacionales"),
                                                              
                                                              checkboxInput("plot_recuentos", "Mostrar grafico de los recuentos")
                                                              
                                                              
                                                 ), # Fin sidebarPanel Estudio de independencia
                                                 
                                                 mainPanel(
                                                   
                                                   fluidRow(
                                                     column(width = 6, offset = 0.25,
                                                            br(),
                                                            h4("Intervalos y recuentos variable 1"),
                                                            verbatimTextOutput("intervalos_v1")),
                                                     column(width = 6, offset = 0.25,
                                                            br(),
                                                            h4("Intervalos y recuentos variable 2"),
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
                                               
                                      ), # Estudio de independencia
                                      
                                      tabPanel("Diferencia de medias y varianzas",
                                               
                                               sidebarLayout(
                                                 
                                                 sidebarPanel(width = 2,
                                                              
                                                              checkboxInput("dif_med", "Diferencia de medias"),
                                                              
                                                              checkboxInput("dif_var", "Diferencia de varianzas"),
                                                              
                                                              conditionalPanel(condition = "input.dif_med || input.dif_var",
                                                                               
                                                                               uiOutput("selec_var_dif1"),
                                                                               uiOutput("selec_var_dif2"),
                                                                               
                                                                               hr(), br()
                                                                               
                                                              ),
                                                              
                                                              checkboxInput("dif_med_boo", "Diferencia medias con bootstrap"), 
                                                              
                                                              checkboxInput("dif_var_boo", "Diferencia varianzas con bootstrap"), 
                                                              
                                                              conditionalPanel(condition = "input.dif_med_boo || input.dif_var_boo",
                                                                               
                                                                               
                                                                               numericInput("repliBoo", "Replicas bootstrap", min = 500,
                                                                                            max = 5000, value = 1000, step = 100),
                                                                               
                                                                               numericInput("confBoo", "Confianza del intervalo", min = 0.05, 
                                                                                            max = 0.95, value = 0.95, step = 0.05),
                                                                               
                                                                               hr(), br(),
                                                                               
                                                                               checkboxInput("vis_distrBoo_med", "Mostrar distribucion diferencia de medias"),
                                                                               
                                                                               checkboxInput("vis_distrBoo_var", "Mostrar distribucion diferencia varianzas")
                                                                               
                                                              )
                                                              
                                                              
                                                 ), # Fin sidebarPanel Diferencia de medias y varianzas
                                                 
                                                 mainPanel(
                                                   
                                                   conditionalPanel(condition = "input.dif_med",
                                                                    
                                                                    fluidRow(
                                                                      column(width = 4, offset = 0.25,
                                                                             br(),
                                                                             h4("Media variable 1"),
                                                                             verbatimTextOutput("med_v1")),
                                                                      
                                                                      column(width = 4, offset = 0.25,
                                                                             br(),
                                                                             h4("Media variable 2"),
                                                                             verbatimTextOutput("med_v2")),
                                                                      
                                                                      column(width = 4, offset = 0.25,
                                                                             br(),
                                                                             h4("Diferencia de medias"),
                                                                             verbatimTextOutput("dif_me1me2"))
                                                                      
                                                                    )
                                                                    
                                                   ),
                                                   
                                                   conditionalPanel(condition = "input.dif_var",
                                                                    
                                                                    fluidRow(
                                                                      column(width = 4, offset = 0.25,
                                                                             br(),
                                                                             h4("Varianza variable 1"),
                                                                             verbatimTextOutput("var_v1")),
                                                                      
                                                                      column(width = 4, offset = 0.25,
                                                                             br(),
                                                                             h4("Varianza variable 2"),
                                                                             verbatimTextOutput("var_v2")),
                                                                      
                                                                      column(width = 4, offset = 0.25,
                                                                             br(),
                                                                             h4("Diferencia de Varianza"),
                                                                             verbatimTextOutput("dif_var1var2"))
                                                                      
                                                                    )
                                                                    
                                                   ),
                                                   
                                                   conditionalPanel(condition = "input.dif_med_boo",
                                                                    
                                                                    fluidRow(
                                                                      column(width = 5, offset = 0.25,
                                                                             br(),
                                                                             h4("Estimacion de la diferencia"),
                                                                             verbatimTextOutput("est_med_boo")),
                                                                      
                                                                      column(width = 5, offset = 0.25,
                                                                             br(),
                                                                             h4("Intervalos de confianza"),
                                                                             verbatimTextOutput("ic_med_boo"))
                                                                      
                                                                    )
                                                                    
                                                   ),
                                                   
                                                   conditionalPanel(condition = "input.dif_var_boo",
                                                                    
                                                                    fluidRow(
                                                                      column(width = 5, offset = 0.25,
                                                                             br(),
                                                                             h4("Estimacion de la diferencia"),
                                                                             verbatimTextOutput("est_var_boo")),
                                                                      
                                                                      column(width = 5, offset = 0.25,
                                                                             br(),
                                                                             h4("Intervalos de confianza"),
                                                                             verbatimTextOutput("ic_var_boo"))
                                                                      
                                                                    )
                                                                    
                                                   ), 
                                                   
                                                   conditionalPanel(condition = "input.vis_distrBoo_med",
                                                                    
                                                                    fluidRow(
                                                                      column(width = 4, offset = 0.25,
                                                                             br(),
                                                                             h4("Densidad e histograma de la diferncia de medias"),
                                                                             plotlyOutput("plotdifmed")),
                                                                      
                                                                      column(width = 4, offset = 0.25,
                                                                             br(),
                                                                             h4("QQplot de la diferncia de medias"),
                                                                             plotlyOutput("qqplotdifmed")),
                                                                      
                                                                      column(width = 4, offset = 0.25,
                                                                             br(),
                                                                             h4("Test de normalidad de la diferncia de medias"),
                                                                             verbatimTextOutput("normdifmed"))
                                                                      
                                                                    )
                                                                    
                                                   ),
                                                   
                                                   conditionalPanel(condition = "input.vis_distrBoo_var",
                                                                    
                                                                    fluidRow(
                                                                      column(width = 4, offset = 0.25,
                                                                             br(),
                                                                             h4("Densidad e histograma de la diferncia de varianzas"),
                                                                             plotlyOutput("plotdifvar")),
                                                                      
                                                                      column(width = 4, offset = 0.25,
                                                                             br(),
                                                                             h4("QQplot de la diferncia de varianzas"),
                                                                             plotlyOutput("qqplotdifvar")),
                                                                      
                                                                      column(width = 4, offset = 0.25,
                                                                             br(),
                                                                             h4("Test de normalidad de la diferncia de varianzas"),
                                                                             verbatimTextOutput("normdifvar"))
                                                                      
                                                                    )
                                                                    
                                                   )
                                                   
                                                   
                                                 ) # Fin del mainPanel Diferencia de medias y varianzas
                                                 
                                               ) # Fin del sidebarLayour Diferencia de medias y varianzas
                                               
                                      ) # Fin del tabPanel Diferencia de medias y varianzas
                                      
                                      
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
                                                                          c("Lineal simple" = "ls"),
                                                                          selected = NULL),
                                                              
                                                              checkboxInput("aplicarlogx",
                                                                            "Aplicar logaritmo a X"),
                                                              
                                                              checkboxInput("aplicarlogy",
                                                                            "Aplicar logaritmo a Y"),
                                                              
                                                              
                                                              uiOutput("controlesApliReg"),
                                                              
                                                              
                                                              uiOutput("controlesResidLS")
                                                              
                                                              
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
                           
                           
                           tabPanel("Analisis multivariante",
                                    
                                    tabsetPanel(
                                      
                                      tabPanel("Regresion lineal multiple",
                                               
                                               sidebarLayout(
                                                 sidebarPanel(width = 2,
                                                              
                                                              checkboxInput("corr", "Matriz y test de correlaciones"),
                                                              
                                                              uiOutput("rm_dep"), uiOutput("rm_Vars"), hr(),
                                                              
                                                              checkboxInput("regMul", "Resultados de la regresion"), hr(),
                                                              
                                                              checkboxInput("colEstu", "Estudio de la colinealidad"), hr(),
                                                              
                                                              checkboxInput("resEst", "Estudio de los residuos")
                                                              
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
                                                                    )),
                                                   
                                                   conditionalPanel(condition = 'input.resEst',
                                                                    fluidRow(
                                                                      column(width = 5, offset = 0.25, br(),
                                                                             h4("Grafico de los residuos"),
                                                                             plotlyOutput("RLMresid")),
                                                                      
                                                                      column(width = 5, offset = 0.25, br(),
                                                                             h4("Test de normalidad de los residuos"),
                                                                             verbatimTextOutput("RLMnorm"))
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
                                                                             plotOutput("cluster_jer",  "1200px"))
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
                                                                             plotOutput("cluster_kmeans")))
                                                                    
                                                   ) # Fin conditionalPanel kmeans
                                                 ) # Fin mainPanel Analisis cluster
                                               ) # Fin del sidebarLayout Analisis cluster
                                      ) # Fin del tabPanel Cluster
                                      
                                      
                                    ) # Fin del tabsetPanel Analisis multivariante
                           ) # Fin del tabPanel Analisis multivariante
                           
                ) # Fin navbarPage
                ) # Fin fluidPage



server <- function(input, output) {
  
  
  ### GENERADORES DE FUNCIONES ###
  
  
  datos_2 = NULL
  normal = eventReactive(input$creaNorm, {
    
    if(input$creaNorm > 0){
      
      norm = rnorm(isolate(input$nNorm), isolate(input$media), isolate(input$varianza))
      
      if(is.null(datos_2) || (nrow(datos_2) != length(norm))){
        
        datos_2 <<- cbind(norm)
        
      }else{
        
        datos_2 <<- cbind(datos_2, norm)
        
      }
      
    }
    
    datos_2 = datos_2
    return(datos_2)
    
  })
  
  datos_3 <- NULL
  binomial = eventReactive(input$creaBinom, {
    
    if(input$creaBinom > 0){ 
      
      binom = rbinom(isolate(input$nBin), isolate(input$tamanno), isolate(input$prob))
      
      if (is.null(datos_3) || (nrow(datos_3) != length(binom))){
        
        datos_3 <<- cbind(binom)
        
      }else{ 
        
        datos_3 <<- cbind(datos_3, binom)
        
      } 
      
    }
    
    datos_3 = datos_3
    return(datos_3)
    
  })
  
  datos_4 = NULL
  poiss = eventReactive(input$creaPoiss, {
    
    if(input$creaPoiss > 0){ 
      
      pois = rpois(isolate(input$nPois), isolate(input$lambda))
      
      if (is.null(datos_4) || (nrow(datos_4) != length(pois))){
        
        datos_4 <<- cbind(pois)
        
      }else{ 
        
        datos_4 <<- cbind(datos_4, pois)
        
      } 
      
    }
    
    datos_4 = datos_4
    return(datos_4)
    
  }) 
  
  datos2 = reactive({
    
    datos = NULL
    
    if(input$creaNorm > 0){ datos = cbind(datos, normal()) }else{ datos = datos }
    if(input$creaBinom > 0){ datos = cbind(datos, binomial()) }else{ datos = datos }
    if(input$creaPoiss > 0){ datos = cbind(datos, poiss()) }else{ datos = datos }
    
    datos = data.frame(datos)
    
  })
  
  
  
  
  ### GENERADOR CAMBIO NAs ###  
  
  datos_pred_NA = eventReactive(input$camb_NA_pred, {
    
    data.frame(complete(mi.datos(), isolate(input$iteraccion_num)))
    
  })
  
  
  
  ### LECTURA Y VISUALIZACION DE DATOS ### 
  
  infile = reactive({
    
    infile = input$subirDat
    
  })
  
  
  dat2 = NULL
  datos = reactive({
    
    dat = data.frame()
    
    if(is.null(infile())){ dat = datos2() }else{
      
      dat = read.csv2(infile()$datapath, 
                      h = input$header, 
                      sep = input$sep, 
                      dec = input$dec,
                      quote = input$quote,
                      na.strings = "NA")}
    
    if(!is.null(infile()) & input$camb_NA_pred == T){ dat = datos_pred_NA() }else{dat}
    
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
    
    if(!is.null(infile()) & input$camb_NA_eliminar == T){ dat = na.omit(dat) }else{dat}
    
    if(!is.null(infile()) &  input$log_apli == T & input$apli_logBOTON > 0){
      
      d2 = NULL
      d2 = data.frame(log(dat[,input$varLog1]))
      colnames(d2) = paste("log_", isolate(input$varLog1), sep =  "")
      
      if(is.null(dat2)){
        
        dat2 <<- cbind(d2)
        
      }else{
        
        if(input$apli_logBOTON > 1) dat2 <<- cbind(dat2, d2)
        
      }
      
      dat = cbind(dat, dat2)  
      return(dat)
      
    }else{dat}
    
    if(!is.null(infile()) & input$scale_apli == T & input$apli_scaleBOTON > 0){
      
      d3 = NULL
      d3 = data.frame(scale(dat[,input$varScale1]))
      colnames(d3) = paste("sc_", isolate(input$varScale1), sep =  "")
      
      if(is.null(dat2)){
        
        dat2 <<- cbind(d3)
        
      }else{
        
        if(input$apli_scaleBOTON > 1) dat2 <<- cbind(dat2, d3)
        
      }
      
      dat = cbind(dat, dat2)  
      return(dat)
      
    }else{dat}
    
    if(!is.null(infile()) & input$apli_cox == T){
      
      df <- dat
      v  <- names(df[,sapply(df, class) == "numeric"])
      
      for (ii in 1:length(v)){
        asimetria = skewness(df[,v[ii]])
        coef.variacion = sd(df[,v[ii]]) / mean(df[,v[ii]]) 
        
        if ((asimetria < -1 | asimetria > 1) & coef.variacion > 1){
          df$bc_tmp <- BoxCox(df[,v[ii]],BoxCox.lambda(df[,v[ii]]))
          names(df)[names(df)=="bc_tmp"] = paste("BoxCox_", v[ii], sep = "")
        }
      }
      
      df
      
    }else{dat}
    
  }) 
  
  
  
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
      
    }
    
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
                choices = names(datos_cj()), selected = NULL)
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
    
  }) # Test de normalidad SP
  
  
  output$var_log1 = renderUI({
    
    selectInput("varLog1", "Seleccionar variable:",
                choices = names(isolate(datos_cj())), selected = NULL)
    
  }) # Variable a logaritmizar 
  output$tabla_log1 = renderTable({
    
    d2 = data.frame(datos()[,colnames(datos()) == input$varLog1])
    d2 = cbind(d2, log(d2))
    colnames(d2) = c(input$varLog1, paste("log_", input$varLog1, sep =  ""))
    head(d2, 10)
    
  }) # Tabla de logaritmizacion 
  
  
  output$var_scale1 = renderUI({
    
    selectInput("varScale1", "Seleccionar variable:",
                choices = names(isolate(datos_cj())), selected = NULL)
    
  }) # Variable a escalar 
  output$tabla_scale1 = renderTable({
    
    d2 = data.frame(datos()[,colnames(datos()) == input$varScale1])
    d2 = cbind(d2, scale(d2))
    colnames(d2) = c(input$varScale1, paste("sc_", input$varScale1, sep =  ""))
    head(d2, 10)
    
  }) # Tabla de escalado 
  
  
  
  ### ESTIMACIONES BOOTSTRAP ###
  
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
    
    bb = boot(datos()[,input$selecVar_normtest], mean.boot, input$repli)
    return(bb)
    
  }) # Contiene la funcion que calcula los IC de la med. o la var.
  
  output$ic_est_media = renderPrint({
    boot.ci(icBoot(), index = 1, conf = input$confianza)
  }) # IC para media bootstrap 
  output$ic_est_varianza = renderPrint({
    
    boot.ci(icBoot(), index = 2, conf = input$confianza)
    
  }) # IC para varianza bootstrap
  
  
  ### INTERVALOS Y RECUENTOS ### 
  
  output$var_ind1 = renderUI({
    selectInput("varInd1", "Seleccionar variable 1:", 
                choices = names(datos_cj()), selected = NULL)
  }) # Variable 1 
  output$var_ind2 = renderUI({
    selectInput("varInd2", "Seleccionar variable 2:", 
                choices = names(datos_cj()), selected = NULL)
  }) # Variable 2 
  
  intV1 = reactive({
    
    r = range(datos()[,input$varInd1])
    cs = nclass.Sturges(datos()[,input$varInd1])
    li = seq(r[1], r[2], length = cs)
    
    intervalos = cut(datos()[,input$varInd1], breaks = li, include.lowest = T)
    
    int = data.frame(table(intervalos)); colnames(int) = c("Intervalos", "Frecuencia")
    return(int)
    
  }) # Calcula los intervalos y sus frecuencias de la variable 1
  output$intervalos_v1 = renderPrint({
    
    intV1()
    
  })
  
  intV2 = reactive({
    
    r = range(datos()[,input$varInd2])
    cs = nclass.Sturges(datos()[,input$varInd2])
    li = seq(r[1], r[2], length = cs)
    
    intervalos = cut(datos()[,input$varInd2], breaks = li, include.lowest = T)
    
    int = data.frame(table(intervalos)); colnames(int) = c("Intervalos", "Frecuencia")
    return(int)
    
  }) # Calcula los intervalos y sus frecuencias de la variable 2
  output$intervalos_v2 = renderPrint({
    
    intV2()
    
  })
  
  output$prop_v1 = renderPrint({
    
    proporciones = cbind.data.frame(intV1()$Frecuencia / sum(intV1()$Frecuencia), (intV1()$Frecuencia / sum(intV1()$Frecuencia)*100)); colnames(proporciones) = c("Proporciones", "Porcentaje")
    
    return(proporciones)
    
  }) 
  output$prop_v2 = renderPrint({
    
    proporciones = cbind.data.frame(intV2()$Frecuencia / sum(intV2()$Frecuencia), (intV2()$Frecuencia / sum(intV2()$Frecuencia)*100)); colnames(proporciones) = c("Proporciones", "Porcentaje")
    
    return(proporciones)
    
  })
  
  output$plot_v1 = renderPlotly({
    
    ggplot(intV1(), aes(Intervalos, Frecuencia)) + 
      geom_bar(stat = 'identity', width = 0.10) +
      ggtitle("Grafico de recuentos") +
      xlab(input$varInd1) + 
      ylab("Recuentos") +
      theme_bw()
    
  }) # Plot recuentos 1
  output$plot_v2 = renderPlotly({
    
    ggplot(intV2(), aes(Intervalos, Frecuencia)) + 
      geom_bar(stat = 'identity', width = 0.10) +
      ggtitle("Grafico de recuentos") +
      xlab(input$varInd2) + 
      ylab("Recuentos") +
      theme_bw()
    
  }) # Plot recuentos 2
  
  
  ### DIFERENCIA DE MEDIAS Y VARIANZAS ###
  
  output$selec_var_dif1 = renderUI({
    
    selectInput("varDif1", "Seleccionar variable 2:", 
                choices = names(datos_cj()), selected = NULL)
    
  })
  output$selec_var_dif2 = renderUI({
    
    selectInput("varDif2", "Seleccionar variable 2:", 
                choices = names(datos_cj()), selected = NULL)
    
  })
  
  ### Medias y diferencias estimador estandar
  
  output$med_v1 = renderPrint({
    
    mean(datos()[,input$varDif1])
    
  })
  
  output$med_v2 = renderPrint({
    
    mean(datos()[,input$varDif2])
    
  })
  
  output$dif_me1me2 = renderPrint({
    
    mean(datos()[,input$varDif1]) -  mean(datos()[,input$varDif2])
    
  })
  
  ### Varianzas y diferencias estimador estandar
  
  output$var_v1 = renderPrint({
    
    var(datos()[,input$varDif1])
    
  })
  
  output$var_v2 = renderPrint({
    
    var(datos()[,input$varDif2])
    
  })
  
  output$dif_var1var2 = renderPrint({
    
    var(datos()[,input$varDif1]) - var(datos()[,input$varDif2])
    
  })
  
  ### Bootstrapping para diferencia de estimadores ###
  
  bootdif_med = reactive({
    
    diferencia_medias <- function(data, i) {
      
      pseudomuestra <- data[i,]
      
      mean_1 <- mean(pseudomuestra[, input$varDif1])
      mean_2 <- mean(pseudomuestra[, input$varDif2])
      
      return(mean_1 - mean_2)
    }
    
    
    boot_distribution <- boot(data = datos(), statistic = diferencia_medias, R = input$repliBoo)
    return(boot_distribution)
    
  })
  
  bootdif_var = reactive({
    
    diferencia_varianzas <- function(data, i) {
      
      pseudomuestra <- data[i,]
      
      var_1 <- var(pseudomuestra[, input$varDif1])
      var_2 <- var(pseudomuestra[, input$varDif2])
      
      return(var_1 - var_2)
    }
    
    
    boot_distribution <- boot(data = datos(), statistic = diferencia_varianzas, R = input$repliBoo)
    return(boot_distribution)
    
  })
  
  output$est_med_boo = renderPrint({
    
    bootdif_med()
    
  })
  output$est_var_boo = renderPrint({
    
    bootdif_var()
    
  })
  
  output$ic_med_boo = renderPrint({
    
    boot.ci(bootdif_med())
    
  })
  output$ic_var_boo = renderPrint({
    
    boot.ci(bootdif_var())
    
  })
  
  output$plotdifmed = renderPlotly({
    
    bootdif_med = data.frame(Estadisticos = bootdif_med()$t)
    
    ggplot(data = bootdif_med, aes(Estadisticos)) +
      geom_histogram(aes(y = ..density..), alpha = 0.4, colour = "white") + 
      geom_line(aes(y = ..density..), stat = 'density', colour = "red") +
      geom_vline(xintercept = mean(bootdif_med$Estadisticos)) +
      geom_vline(xintercept = quantile(bootdif_med$Estadisticos)[2], colour = "blue") +
      geom_vline(xintercept = quantile(bootdif_med$Estadisticos)[4], colour = "blue") +
      labs(x = "Diferencia de medias") +
      theme_bw()
    
    
  })
  output$qqplotdifmed = renderPlotly({
    
    bootdif_med = data.frame(Estadisticos = bootdif_med()$t)
    
    ggplot(bootdif_med, aes(sample = Estadisticos)) + 
      stat_qq(position = "identity", col = "brown3") + 
      stat_qq_line(size = 1) + 
      ylab("Teorica") + xlab("Muestra") +
      theme_bw()
    
    
  })
  output$normdifmed = renderPrint({
    
    bootdif_med = data.frame(Estadisticos = bootdif_med()$t)
    
    shapiro.test(bootdif_med$Estadisticos)
    
  }) 
  
  ### ESTUDIO DIFERENCIA VARIANZAS ###
  
  output$plotdifvar = renderPlotly({
    
    bootdif_var = data.frame(Estadisticos = bootdif_var()$t)
    
    ggplot(data = bootdif_var, aes(Estadisticos)) +
      geom_histogram(aes(y = ..density..), alpha = 0.4, colour = "white") + 
      geom_line(aes(y = ..density..), stat = 'density', colour = "red") +
      geom_vline(xintercept = mean(bootdif_var$Estadisticos)) +
      geom_vline(xintercept = quantile(bootdif_var$Estadisticos)[2], colour = "blue") +
      geom_vline(xintercept = quantile(bootdif_var$Estadisticos)[4], colour = "blue") +
      labs(x = "Diferencia de varianzas") +
      theme_bw()
    
    
  }) # Distribucion diferencia de varianzas
  output$qqplotdifvar = renderPlotly({
    
    bootdif_var = data.frame(Estadisticos = bootdif_var()$t)
    
    ggplot(bootdif_var, aes(sample = Estadisticos)) + 
      stat_qq(position = "identity", col = "brown3") + 
      stat_qq_line(size = 1) + 
      ylab("Teorica") + xlab("Muestra") +
      theme_bw()
    
    
  }) # QQ-plot
  output$normdifvar = renderPrint({
    
    bootdif_var = data.frame(Estadisticos = bootdif_var()$t)
    
    shapiro.test(bootdif_var$Estadisticos)
    
  }) # SWtest para la distrinucion de la diferencia de varianzas
  
  
  ### GRAFICA DE DISPERSION Y REGRESION ###
  
  output$controles1 = renderUI({
    
    selectInput("selectvarX", "Seleccionar variable X:",
                choices = names(isolate(datos())),
                selected = 1)
    
  }) 
  output$controles2 = renderUI({
    
    selectInput("selectvarY", "Seleccionar variable Y:",
                choices = names(isolate(datos())),
                selected = 1)
    
  }) 
  
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
    
  }) # Plot de dispersion
  
  
  output$controlesApliReg = renderUI({
    
    checkboxInput("aplicarReg",
                  "Aplicar regresion")
    
    
  }) 
  
  output$controlesResidLS = renderUI({
    
    conditionalPanel(
      condition = 'input.aplicarReg',
      checkboxInput("visResid",
                    "Visualizar residuos"))
    
  })
  
  
  output$smLS = renderUI({
    
    conditionalPanel(condition = 'input.aplicarReg',
                     br(), br(),
                     h4("Regresion lineal simple"),
                     verbatimTextOutput("summaryregLS"))
    
  }) 
  
  datRLS = reactive({
    
    dat = cbind.data.frame(datos()[,input$selectvarY], datos()[,input$selectvarX])
    colnames(dat) = c(input$selectvarY, input$selectvarX)
    
    if(input$aplicarlogx == T){
      
      colnames(dat) = c(input$selectvarY, paste("log_", input$selectvarX, sep = ""))
      
    }else{
      
      colnames(dat) = c(input$selectvarY, input$selectvarX)
      
    }
    
    if(input$aplicarlogy == T){
      
      colnames(dat) = c(paste("log_", input$selectvarY, sep = ""), input$selectvarX)
      
    }else{
      
      colnames(dat) = c(input$selectvarY, input$selectvarX)
      
    }
    
    if(input$aplicarlogx == T & input$aplicarlogy == T){
      
      colnames(dat) = c(paste("log_", input$selectvarY, sep = ""), paste("log_", input$selectvarX, sep = ""))
      
    }else{
      
      colnames(dat) = c(input$selectvarY, input$selectvarX)
      
    }
    
    return(dat)
    
  }) # Selecciona los datos para la regresion
  regRLS = reactive({
    
    if(input$aplicarReg == T){
      
      reg = lm(datos()[,input$selectvarY] ~ datos()[,input$selectvarX], data = datos())
      
      if(input$aplicarlogx == T){
        
        reg = lm(datos()[,input$selectvarY] ~ log(datos()[,input$selectvarX]), data = datos())
        
      }else{
        
        reg
        
      }
      
      if(input$aplicarlogy == T){
        
        reg = lm(log(datos()[,input$selectvarY]) ~ datos()[,input$selectvarX], data = datos())
        
      }else{
        
        reg 
        
      }
      
      if(input$aplicarlogx == T && input$aplicarlogy == T){
        
        reg = lm(log(datos()[,input$selectvarY]) ~ log(datos()[,input$selectvarX]), data = datos())
        
      }else{
        
        reg
        
      }
      
    }else{
      
      return(NULL)
      
    }
    
  }) # Realiza las regresiones
  
  output$summaryregLS = renderPrint({
    
    summary(regRLS())
    
  }) # Solo en RLS
  
  output$plResidLS = renderUI({
    
    conditionalPanel(condition = 'input.visResid',
                     br(), br(),
                     h4("Variable X vs residuos"),
                     plotlyOutput("plotResidLS", width = "500px"))
    
  }) # plotlyOutput para residuos
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
      
      p = ggplot(data = datos(), aes(log(datos()[,input$selectvarX]), residuos[,1])) + 
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
      
      p = ggplot(data = datos(), aes(log(datos()[,input$selectvarX]), residuos[,1])) + 
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
    
  }) # plotlyOutput para residuos
  
  
  output$hisResidLS = renderUI({
    conditionalPanel(condition = 'input.visResid',
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
                              colour = "white", 
                              alpha = 0.4) + 
        geom_line(aes(y = ..density..), stat = 'density', colour = "brown3") + 
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
                              colour = "white", 
                              alpha = 0.4) +
        geom_line(aes(y = ..density..), stat = 'density', colour = "brown3") + 
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
                              colour = "white", 
                              alpha = 0.4) +
        geom_line(aes(y = ..density..), stat = 'density', colour = "brown3") + 
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
                              colour = "white", 
                              alpha = 0.4) +
        geom_line(aes(y = ..density..), stat = 'density', colour = "brown3") + 
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
                              colour = "white", 
                              alpha = 0.4) +
        geom_line(aes(y = ..density..), stat = 'density', colour = "brown3") + 
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
                              colour = "white", 
                              alpha = 0.4) +
        geom_line(aes(y = ..density..), stat = 'density', colour = "brown3") + 
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
                              colour = "white", 
                              alpha = 0.4) +
        geom_line(aes(y = ..density..), stat = 'density', colour = "brown3") + 
        theme_bw()     
      
    }
    
  }) 
  
  
  
  
  
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
    
    x = ggplot(datos(), aes(get(input$varesc))) + xlab(input$varesc) 
    
    if(class(datos()[,input$varesc]) == "numeric"){
      
      x + geom_density(fill = '#75AADB', alpha = 0.32)  + ylab("Densidad") + theme_bw()
      
      if(input$aplicarQuan > 0){
        
        x + geom_density(fill = '#75AADB', alpha = 0.32)  + 
          geom_vline(aes(xintercept = mean(datos()[,input$varesc])), col = "red", size = 0.75) +  
          geom_vline(aes(xintercept = quantile(datos()[,input$varesc])[2]), linetype = 2, col = "springgreen4", size = 0.75) +
          geom_vline(aes(xintercept = quantile(datos()[,input$varesc])[4]), linetype = 2, col = "springgreen4", size = 0.75) +
          geom_vline(aes(xintercept = quantile(datos()[,input$varesc])[3]), linetype = 2, col = "springgreen4", size = 0.75) +
          ylab("Densidad") +
          theme_bw()
        
      }else{
        
        x + geom_density(fill = '#75AADB', alpha = 0.32)  + ylab("Densidad") + theme_bw()
        
      }
      
    }else{
      
      x + stat_count(stat = 'identity', width = 0.10, fill = '#75AADB', alpha = 0.75) + ylab("Distribucion") + theme_bw()
      
    }
    
  })
  
  output$controles4 = renderUI({
    
    radioButtons("varescHist", "Seleccion de variables:",
                 choices = names(datos()),
                 selected = NULL)
    
  })
  
  bar = reactive({
    
    cs = nclass.Sturges(datos()[,input$varescHist])
    
    return(cs)
    
  })
  output$controles5 = renderUI({
    
    sliderInput("barras", 
                label = h4("Seleccionar numero de barras:"),
                min = 1, max = 50, value = bar())
    
    
  })
  output$controles6 = renderUI({
    
    checkboxInput("trazLinea",
                  "Trazar linea")
    
    
  })
  
  output$histograma = renderPlotly({
    
    cs = nclass.Sturges(datos()[,input$varescHist])
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
                      size = 0.75) + 
        
        xlab(input$varescHist) + ylab("Recuento")
      
    }else{
      
      ggplot(datos(), aes(get(input$varescHist))) + 
        
        geom_histogram(position = "identity", 
                       breaks = bar, 
                       fill = '#75AADB',
                       colour = "black", 
                       alpha = 0.5) + 
        
        theme_bw() + 
        
        xlab(input$varescHist) + ylab("Recuento")
      
      
    }
    
    
  })
  
  
  
  
  ### ANALISIS CLUSTER ###
  
  ### Cluster jerarquico ###
  
  datos_noNum = reactive({
    
    datos.noNu = datos()[,sapply(datos(), class) != "numeric"]
    
    nombres = c(names(datos()[,sapply(datos(), class) != "numeric"]))
    
    colnames(datos.noNu) = nombres
    
    return(datos.noNu)
    
  }) # Datos con variables no numericas
  
  output$var_Char = renderUI({
    selectInput("varChar", "Seleccionar nombre de filas:",
                choices = names(datos_noNum()),
                selected = 1)
  })
  
  datos_cj = reactive({
    
    datosCJ = datos()[,sapply(datos(), class) == "numeric"]
    
    nombres = c(names(datos()[,sapply(datos(), class) == "numeric"]))
    
    colnames(datosCJ) = nombres
    
    return(datosCJ)
    
  }) # Datos con variables numericas
  
  
  
  distancia = reactive({
    
    distancia = dist(datos_cj(), method = input$usa_distancia)
    
  })
  
  output$grupos = renderPrint({
    
    fit = hclust(distancia(), method = input$usa_cluster)
    grupo = cutree(fit, k = input$usa_agrupamientos) 
    grupo = data.frame(grupo) 
    
    if(input$dibujar_agrup != T){
      
      print("A la espera de seleccionar agrupamientos")
      
    }else{
      
      t(grupo)
      
    }
    
  }) # Visualizador matriz de distancias
  
  output$cluster_jer = renderPlot({
    
    fit = hclust(distancia(), method = input$usa_cluster)
    plot(fit, labels = datos()[,input$varChar], cex = 1.2) 
    
    if(input$dibujar_agrup == T){
      
      plot(fit, labels = datos()[,input$varChar], cex = 1.2); 
      rect.hclust(fit, k = input$usa_agrupamientos, border = "red")
      
    }else{
      
      plot(fit, labels = datos()[,input$varChar], cex = 1.2) 
      
    }
    
  }) # Cluster jerarquico
  
  
  ### Cluster kmeans ###
  
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
    
    if(input$dscale == T){
      
      wss = (nrow(datos_sc()) - 1)*sum(apply(datos_sc(), 2, var))
      for (i in 2:input$num_var) wss[i] <- sum(kmeans(datos_sc(), centers = i)$withinss)
      wss
      
    }else{
      
      wss = (nrow(datos_cj()) - 1)*sum(apply(datos_cj(), 2, var))
      for (i in 2:input$num_var) wss[i] <- sum(kmeans(datos_cj(), centers = i)$withinss)
      wss
      
    }
    
    
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
      
      km2 = kmeans(datos_sc(), centers = input$num_centros, nstart = input$nstart,  
                   algorithm = input$usa_algoritmo)
      
      return(km2)
      
    }else{
      
      km2 = kmeans(datos_cj(), centers = input$num_centros, nstart = input$nstart, 
                   algorithm = input$usa_algoritmo)
      
      return(km2)
      
    }
    
  })
  
  output$cluster_kmeans = renderPlot({
    
    if(input$apli_kmeans > 0){
      
      if(input$dscale == T){
        
        plot(datos_sc()[,input$varXclus], datos_sc()[,input$varYclus],
             col = km()$cluster,
             xlab = input$varXclus,
             ylab = input$varYclus,
             pch = 20, cex = 3)
        
      }else{
        
        plot(datos_cj()[,input$varXclus], datos_cj()[,input$varYclus],
             col = km()$cluster,
             xlab = input$varXclus,
             ylab = input$varYclus,
             pch = 20, cex = 3)
        
      }
      
    }
    
  }) 
  
  
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
  
  residRLM = reactive({
    
    residuos = data.frame(resid(regMul())); colnames(residuos) = c("Residuos")
    residuos
    
  }) # Residuos RLM
  
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
    
    FIV_(as.matrix(datReg()[,-1]))
    
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
    
    observaciones = dim(datReg()[,-1])[1]
    cte = rep(1, observaciones) 
    matriz = cbind(cte,as.matrix(datReg()[,-1])) 
    Xlu = longitud_unidad(matriz)
    NC_(Xlu)
    
  })
  
  output$RLMresid = renderPlotly({
    
    cs = nclass.Sturges(residRLM()[,1])
    bar = seq(min(residRLM()[,1]), max(residRLM()[,1]), length.out = cs + 1)
    
    p = ggplot(residRLM(), aes(residRLM()[,1])) + xlab("Residuos") + ylab("Densidad")
    
    p + geom_histogram(aes(y = ..density..), 
                       breaks = bar, 
                       fill = '#75AADB',
                       colour = "white", 
                       alpha = 0.4) +
      
      geom_line(aes(y = ..density..), stat = 'density', colour = "brown3") + 
      theme_bw()          
    
    
    
  })
  
  output$RLMnorm = renderPrint({
    
    shapiro.test(residRLM()[,1])
    
  })
  
}

shinyApp(ui = ui, server = server)