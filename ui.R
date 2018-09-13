

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(shinyjs)
library(DT)
source("helper_functions.R")
major<-getActivePrograms()

ui<-
fluidPage(includeCSS("theme.CSS"),shinyjs::useShinyjs(),
dashboardPage(skin="yellow",title="Funding Formula App",#58355E(right) #422c46(left)
dashboardHeader(title="ELAC",titleWidth="300px"),
dashboardSidebar(disable=T ),
dashboardBody(
              
                  column(12,div(id="time_frame",
                       box(solidHeader = T, width=12,title="Time Frame",status="info",
                                           column(3,selectInput(inputId = "year",label="Academic Year",
                                                                choices=c(paste0(as.integer(format(Sys.Date(), "%Y")):(as.integer(format(Sys.Date(), "%Y"))-5),
                                                                                 "-",(as.integer(format(Sys.Date(), "%Y"))+1):(as.integer(format(Sys.Date(), "%Y"))-4))))),
                                           column(3,checkboxGroupInput(inputId = "semester","Semester (OPTIONAL)",
                                                                 choices=c("Summer","Fall","Winter","Spring")))
                  ))),
                tabsetPanel(
                       tabPanel(title="Base and Supplemental Allocation",br(),
                                column(12,downloadButton(outputId="downloadExcel",label="Excel")),br(),br(),br(),
                                column(4, div(id="department_box",box(solidHeader = T,status="info",width=12,
                                                                      title= "Department",selectInput(inputId = "department",label=NULL,choices=c(" "))))),
                                column(8,div(id="discipline_box",box(solidHeader = T,status="info",width=6,title="Discipline",selectInput(inputId = "discipline", label=NULL,choices=c(" "))))),
                                column(offset=11,width=1,actionButton("submit","Submit")),
                                column(4,uiOutput("value_box_sem_year_base")),
                                column(8,uiOutput("value_box_dep_dis")),
                                tabBox(
                                        id = "tabs",width=12,
                                        tabPanel(title="FTES Overview",value="ftes_glance_tab",
                                                
                                                 plotlyOutput("FTES_glance",height=500),
                                                 DT::dataTableOutput("table_ftes_glance")),
                                        tabPanel(title="FTES Breakdown",value="ftes_overview_tab",
                                                 plotlyOutput("FTES_overview",height=500),
                                                 DT::dataTableOutput("table_ftes_overview")),
                                        tabPanel(title="Need-Based",value="supp_overview_tab",
                                        plotlyOutput("supp_overview",height=500),
                                        DT::dataTableOutput("table_supp")))
                                       
                                ),#end of FTES tab
                       tabPanel(title="Success Allocation",br(),
                                column(12,downloadButton(outputId="downloadExcel_success",label="Excel")),br(),br(),br(),
                                column(12,div(id="program_box",box(solidHeader = T,status="info",width=4,title="Program",selectInput(inputId = "program",label=NULL,choices=c("All Programs",levels(major$Major)) )))),
                                column(offset=11,width=1,actionButton("submit_success","Submit")),
                                column(4,uiOutput("value_box_sem_year_success")),
                                column(8,uiOutput("value_box_prog")),
                                tabBox(id="success_tabs",width=12,
                                       tabPanel(title="Success Allocation", value="success_alocation_tab",
                                                plotlyOutput("success_allocation_graph",height=500),
                                                DT::dataTableOutput("table_success"))
                                       ))#end of Success Allocation Tab
                )
                     
    )#dashboardBody
              ))#dashboardPage ,fluidpage
