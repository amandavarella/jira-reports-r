library(shiny)

shinyUI(fluidPage(
  fluidRow(
    column(4,
           titlePanel("Jira Statistics")
    ),
    column(1, offset=11,
           actionButton("sair","Sair")
    )
  ),

  fluidRow(tabsetPanel(id="tabset",
                       tabPanel("Login",
                                fluidRow(
                                  column(4),
                                  column(4, offset=3.5,
                                         br(),
                                         br(),
                                         br(),
                                         wellPanel(
                                           br(),
                                           br(),
                                           textInput("chave", "Chave", ""),
                                           passwordInput("senha", "Senha", ""),
                                           actionButton("okAut", "Ok"),
                                           br(),
                                           br()
                                         )
                                  )
                                )
                       ),

                       tabPanel("Projeto",
                                fluidRow(
                                  column(4),
                                  column(4, offset=3.5,
                                         br(),
                                         br(),
                                         br(),
                                         wellPanel(
                                           br(),
                                           selectInput("selProj", label = h5("Projeto:"),
                                                       choices = c(
                                                                     "Selecione" = "SEL",
                                                                     "ATLAS" = "ATLAS",
                                                                     "PNI" = "PNI"
                                                                     )
                                                                ),
                                           br(),
                                           br()
                                         )
                                  )
                                )
                       ),

                       tabPanel("Rendimento",
                                fluidPage(
                                  br(),
                                  fluidRow(
                                    column(3,
                                           wellPanel(
                                             h5("Exibir:"),
                                             br(),
                                             checkboxInput("porTipo", "por Tipo de pendência:", FALSE),
                                             br()
                                           )

                                    ),
                                    column(9,
                                           plotOutput("plotThroughput", width="100%", height="600px")
                                    )
                                  )
                                )
                       ),

                       tabPanel("Lead Time",
                                fluidPage(
                                  br(),
                                  fixedRow(
                                    column(3,
                                           wellPanel(
                                             dateRangeInput("periodo", "Período:",
                                                            start=NULL, end=Sys.Date(),
                                                            format="mm/yyyy", language="pt-BR", separator="a"),
                                             actionButton("okLT", "Ok"),
                                             br()
                                           ),
                                           br(),
                                           br(),
                                           wellPanel(
                                             fluidRow(
                                               column(6,
                                                      h5("Min:"),
                                                      verbatimTextOutput("min"),
                                                      h5("1º Quantil:"),
                                                      verbatimTextOutput("pquantil")
                                               ),
                                               column(6,
                                                      h5("Max:"),
                                                      verbatimTextOutput("max"),
                                                      h5("3º Quantil:"),
                                                      verbatimTextOutput("tquantil")
                                               )
                                             )
                                           )
                                    ),
                                    column(9,
                                           plotOutput("plotLT", width="100%", height="600px")
                                    )
                                  )
                                )
                       ),

                       tabPanel("Scatter Plot",
                                plotOutput("plotSP", width="100%", height="600px"),
                                br(),
                                DT::dataTableOutput("tableSP")
                       ),

                       tabPanel("Histograma",
                                plotOutput("plotHist", width="100%", height="600px"),
                                br(),
                                DT::dataTableOutput("tableHist")
                       ),

                       tabPanel("Weibull",
                                plotOutput("plotWeibull", width="100%", height="600px"),
                                br(),
                                DT::dataTableOutput("tableWeibullParam"),
                                br(),
                                DT::dataTableOutput("tableWeibullValues")
                       ),
                       
                       tabPanel("PI Panel Epics",
                                plotOutput("piPanelEpics", width="100%", height="600px")
                       ),
                       
                       tabPanel("PI Panel Story",
                                plotOutput("piPanelStory", width="100%", height="600px")
                       ),
                       
                       tabPanel("Monte Carlo",
                                plotOutput("plotMonteCarlo", width="100%", height="600px")
                       )
                       
                       
  ))
))
