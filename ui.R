#Load libraries
library(shiny)
library(plotly)
library(shinydashboard)
library(data.table)
library(dplyr)
library(DT)
library(lme4)
library(doBy)
library(ggplot2)
library(stringr)
library(stringi)
library(reshape2)
library(doBy)
library(lmerTest)
library(lsr)
library(qrencoder)

#UI setup
shinyUI(fluidPage(theme = "bootstrap.css", 
                  shinyjs::useShinyjs(),
                  navbarPage("HYPOTHESIS TESTING - Simon Gonzalez, James Grama & Catherine Travis - Australian National University",
                             tabPanel("Intro",
                                      fluidRow(
                                        tabBox(
                                          title = "",
                                          id = "tabset0", width = 1000,
                                          tabPanel(tagList(shiny::icon("refresh"), "Data Flowchart"),
                                                   tags$div(img(src = "p.png", width = "650px"))),
                                          tabPanel(tagList(shiny::icon("file-text-o"), "Abstract"),
                                                   
                                                   tags$head(tags$style("#title1{color: black;
                                 font:35px serif;
                                 }"
                                                   )
                                                   ),
                                                   tags$head(tags$style("#text1{color: black;
                                 font:23px serif;
                                                                        }"
                                                   )
                                                   ),
                                                   htmlOutput('title1'),
                                                   htmlOutput('text1')
                                                   
                                          )
                                        ))
                             ),
                             tabPanel("Data Summary and Input",
                                      dashboardPage(
                                        dashboardHeader(title = h5('Sydney Speaks 1970s Data')),
                                        dashboardSidebar(
                                          
                                          sidebarMenu(
                                            helpText('NOTE: Use single space separation'),
                                            helpText(' and type variable names as they'),
                                            helpText('appear in the data'),
                                            menuItem("Model Input", tabName = "formInput", icon = icon("building"),
                                                     textInput("response_variable", label = h5("Dependent Variable"), value = "F1 F2"),
                                                     textInput("fixed_variable", label = h5("Fixed Variable"), value = "sex community class pre_place fol_place sex:community sex:class sex:class:community"),
                                                     textInput("random_intercept", label = h5("Random Intercept"), value = "1|speaker"),
                                                     hr(),
                                                     textInput("sig_factor_test", label = h5("Is this significant?"), value = "sex:class:community")
                                            ),
                                            menuItem("Data Variables", tabName = "dataVars", icon = icon("database"),
                                                     textInput("split_parameter", label = h5("Split Parameter"), value = "vowel"),
                                                     textInput("binary_in", label = h5("Binary Variable"), value = "stress 1"),
                                                     textInput("categorical_in", label = h5("Categorical Variable"), value = "point 2"),
                                                     textInput("discrete_in", label = h5("Discrete Variable"), value = "word_count 3"),
                                                     textInput("continuous_in", label = h5("Continuous Variable"), value = "(formant_sd 3)")
                                            ),
                                            
                                            
                                            plotOutput('qrreader', width = 100, height = 100)
                                          )
                                          
                                        ),
                                        dashboardBody(
                                          
                                          fluidRow(
                                            box(
                                              title = "Statistical Model", status = "primary", solidHeader = TRUE,
                                              collapsible = TRUE, width = 750,
                                              verbatimTextOutput("frm")
                                            )),
                                          fluidRow(
                                            tabBox(
                                              title = tagList(shiny::icon("toggle-down"), "Input Data Summary"),
                                              id = "tabset1", width = 1000,
                                              tabPanel(tagList(shiny::icon("sort-numeric-asc"), "Numeric Columns"),
                                                       DT::dataTableOutput('sum_table_num')
                                              ),
                                              tabPanel(tagList(shiny::icon("sort-alpha-asc"), "Non-numeric columns"),
                                                       DT::dataTableOutput('sum_table_nonnum')
                                              )
                                            ))
                                        )
                                      )
                             ),
                             tabPanel("Visualisation",
                                      dashboardPage(
                                        dashboardHeader(title = h5('Sydney Speaks 1970s Data')),
                                        dashboardSidebar(
                                          
                                          sidebarMenu(
                                            uiOutput('plt_nmber_rslts'),
                                            checkboxGroupInput("plot_type", label = h5("Plot..."), 
                                                               choices = list("Tokens" = 1, "Means" = 2, "Ellipses" = 3, 'Densities' = 4),
                                                               selected = 1),
                                            uiOutput('unfiltered_dt'),
                                            plotOutput('qrreader2', width = 100, height = 100)
                                          )
                                          
                                        ),
                                        dashboardBody(
                                          fluidRow(
                                            tabBox(
                                              title = tagList(shiny::icon("server"), "Results"),
                                              id = "tabset2", width = 1000,
                                              tabPanel(tagList(shiny::icon("table"), "Table"),
                                                       uiOutput('no_sig'),
                                                       DT::dataTableOutput('results_table')
                                              ),
                                              tabPanel(tagList(shiny::icon("area-chart"), "Plot"),
                                                       plotlyOutput('vowel_space_results')
                                              )
                                            ))
                                          
                                          
                                        )
                                      )
                             )
                  )
))
