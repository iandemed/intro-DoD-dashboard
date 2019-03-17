#### global.R ####

source("packrat/init.R")

# Load relevant libraries
library(dplyr)
library(reader)
library(stringr)
library(ggplot2)
library(plotly)
library(DT)
library(choroplethr)
library(choroplethrMaps)

# Load series of functions used throughout
# the app
source("helper_functions.R")


#### ui.R ####
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(
    title = "Visualizing the Suicide Health Epidemic",
    titleWidth = 500
  ),
  
  dashboardSidebar(disable = TRUE),
  # dashboardSidebar(
  #   width = 300,
  #   sidebarMenu(
  #     id = "tabs",
  #     menuItem("Recent Results", icon = icon("th"), tabName = "recentresults"),
  #     downloadButton("downloadHistogram", list("Download"), HTML("&nbsp;&nbsp;&nbsp;", icon("sign-in")))
  #   )
  # ),
  
  # --- Landing Page --- #
  dashboardBody(style = "baackground:white",
                # tabItems(
                # tabItem("recentresults",
                fluidPage(style = "padding:0px",
                          tags$h2("Demographics for Deaths of Despair", style = "margin-top:0px"),
                          
                          fluidRow(
                            box(width = 12, collapsible = TRUE, status = "primary", solidHeader = TRUE,
                                title = strong("Age Distribution"),
                                tabsetPanel(
                                  tabPanel(strong("Total"), style = "min-height:450px",
                                           column(6,br(),plotlyOutput("DoD_total_plot1")),
                                           column(6,br(),plotlyOutput("DoD_total_plot2"))),
                                  tabPanel(strong("Firearms"), style = "min-height:450px",
                                           column(6,br(), plotlyOutput("DoD_firearms_plot1")),
                                           column(6,br(), plotlyOutput("DoD_firearms_plot2"))),
                                  tabPanel(strong("Poisoning"), style = "min-height:450px",
                                           column(6,br(), plotlyOutput("DoD_poisoning_plot1")),
                                           column(6,br(), plotlyOutput("DoD_poisoning_plot2"))),
                                  tabPanel(strong("Self-harm by Jumping"), style = "min-height:450px",
                                           column(6,br(), plotlyOutput("DoD_jumping_plot1")),
                                           column(6,br(), plotlyOutput("DoD_jumping_plot2"))),
                                  tabPanel(strong("Suffocation/Drowning"), style = "min-height:450px",
                                           column(6,br(), plotlyOutput("DoD_suffocation_plot1")),
                                           column(6,br(), plotlyOutput("DoD_suffocation_plot2")))
                                )
                            )
                          ),
                          
                          fluidRow(
                            box(width = 12, collapsible = TRUE, status = "primary", solidHeader = TRUE,
                                title = strong("Geography of Deaths of Despair"),
                                column(4, collapsible = TRUE,h3("Seeking Help"),
                                       p("If you or someone you know is suicidal or in emotional distress please reach out to one of the resources below: "),
                                       p("Suicide Hotline: 1-800-273-8255"),
                                       p(a("Lifeline Chat", href = "https://suicidepreventionlifeline.org/chat/")),
                                       p(a("Locate a Crisis Center", href = "https://suicidepreventionlifeline.org/our-crisis-centers/")),
                                       p(),
                                       h3("Download Charts"),
                                       downloadButton("downloadHistogram", label = "Histograms", HTML("&nbsp;&nbsp;&nbsp;", icon("sign-in"))),
                                       downloadButton("downloadMap", label = "Map", HTML("&nbsp;&nbsp;&nbsp;", icon("sign-in")))
                                ),
                                column(8, br(), br(),
                                       plotOutput("choropleth"),
                                       br()
                                )
                            )
                          )
                )
                # )
                # )
  )
)


### server.R ###

server <- function(input, output, session){
  
  # Load all necessary data in ot the enviornment
  death_lst <- get_data_from_db()
  
  mort_dat <- read.csv("mort.csv", sep=",")
  map_dat <- mort_dat %>% filter(Category == "Self-harm and interpersonal violence") %>% 
    select("FIPS","Mortality.Rate..2014.") %>%
    # We only want county FIPS codes, any FIPS code less than 1000
    # will correspond to states
    filter(FIPS > 1000) %>%
    # county_choropleth requires that data.frames have columns
    # named region and value in order to graph the data correctly
    rename(region = FIPS , value = "Mortality.Rate..2014.")
  
  
  # --- Total deaths of despair --- #
  DoD_total_plot1_info <- reactive({
    get_plot_data("Total", "M", death_lst)
  })
  
  output$DoD_total_plot1 <- renderPlotly({
    plot_info <- DoD_total_plot1_info()
    p <- update_histogram_plot(plotly_obj = plotly_config(plot_ly()), plot_info, plot_info$Sex)
    configLandingPanel(p, xtitle = "Age", ytitle = "Count")
  })
  
  DoD_total_plot2_info <- reactive({
    get_plot_data("Total", "F", death_lst)
  })
  
  output$DoD_total_plot2 <- renderPlotly({
    plot_info <- DoD_total_plot2_info()
    p <- update_histogram_plot(plotly_obj = plotly_config(plot_ly()), plot_info, plot_info$Sex)
    configLandingPanel(p, xtitle = "Age", ytitle = "Count")
  })
  
  
  # --- Firearm related deaths of despair --- #
  DoD_firearms_plot1_info <- reactive({
    get_plot_data("Firearms", "M", death_lst)
  })
  
  output$DoD_firearms_plot1 <- renderPlotly({
    plot_info <- DoD_firearms_plot1_info()
    p <- update_histogram_plot(plotly_obj = plotly_config(plot_ly()), plot_info, plot_info$Sex, 1)
    configLandingPanel(p, xtitle = "Age", ytitle = "Count")
  })
  
  DoD_firearms_plot2_info <- reactive({
    get_plot_data("Firearms", "F", death_lst)
  })
  
  output$DoD_firearms_plot2 <- renderPlotly({
    plot_info <- DoD_firearms_plot2_info()
    p <- update_histogram_plot(plotly_obj = plotly_config(plot_ly()), plot_info, plot_info$Sex, 1)
    configLandingPanel(p, xtitle = "Age", ytitle = "Count")
  })
  
  
  # --- Deaths of despair from poisoning --- #
  DoD_poisoning_plot1_info <- reactive({
    get_plot_data("Poisoning", "M", death_lst)
  })
  
  output$DoD_poisoning_plot1 <- renderPlotly({
    plot_info <- DoD_poisoning_plot1_info()
    p <- update_histogram_plot(plotly_obj = plotly_config(plot_ly()), plot_info, plot_info$Sex, 1)
    configLandingPanel(p, xtitle = "Age", ytitle = "Count")
  })
  
  DoD_poisoning_plot2_info <- reactive({
    get_plot_data("Poisoning", "F", death_lst)
  })
  
  output$DoD_poisoning_plot2 <- renderPlotly({
    plot_info <- DoD_poisoning_plot2_info()
    p <- update_histogram_plot(plotly_obj = plotly_config(plot_ly()), plot_info, plot_info$Sex, 1)
    configLandingPanel(p, xtitle = "Age", ytitle = "Count")
  })
  
  
  #--- Deaths of despair from jumping --- #
  DoD_jumping_plot1_info <- reactive({
    get_plot_data("Self-harm by Jumping", "M", death_lst)
  })
  
  output$DoD_jumping_plot1 <- renderPlotly({
    plot_info <- DoD_jumping_plot1_info()
    p <- update_histogram_plot(plotly_obj = plotly_config(plot_ly()), plot_info, plot_info$Sex, 1)
    configLandingPanel(p, xtitle = "Age", ytitle = "Count")
  })
  
  DoD_jumping_plot2_info <- reactive({
    get_plot_data("Self-harm by Jumping", "F", death_lst)
  })
  
  output$DoD_jumping_plot2 <- renderPlotly({
    plot_info <- DoD_jumping_plot2_info()
    p <- update_histogram_plot(plotly_obj = plotly_config(plot_ly()), plot_info, plot_info$Sex, 1)
    configLandingPanel(p, xtitle = "Age", ytitle = "Count")
  })
  
  
  # --- Deaths of despair from suffocation/drowning --- #
  DoD_suffocation_plot1_info <- reactive({
    get_plot_data("Suffocation or Drowning", "M", death_lst)
  })
  
  output$DoD_suffocation_plot1 <- renderPlotly({
    plot_info <- DoD_suffocation_plot1_info()
    p <- update_histogram_plot(plotly_obj = plotly_config(plot_ly()), plot_info, plot_info$Sex, 1)
    configLandingPanel(p, xtitle = "Age", ytitle = "Count")
  })
  
  DoD_suffocation_plot2_info <- reactive({
    get_plot_data("Suffocation or Drowning", "F", death_lst)
  })
  
  output$DoD_suffocation_plot2 <- renderPlotly({
    plot_info <- DoD_suffocation_plot2_info()
    p <- update_histogram_plot(plotly_obj = plotly_config(plot_ly()), plot_info, plot_info$Sex, 1)
    configLandingPanel(p, xtitle = "Age", ytitle = "Count")
  })
  
  # --- Geogrpahy of Suicides and interpersonal violence --- #
  output$choropleth <- renderPlot({
    
    county_choropleth(map_dat,legend = "Deaths per 100,000")
    
  })
  
  
  
  output$downloadHistogram <- downloadHandler(
    filename = "DoD_histogram.pdf",
    content = function(file) {
      
      # Display a message letting the user know the
      # graph is being created
      showModal(modalDialog(
        title = "Running Histogram charts",
        icon("spinner", class = "fa-spin fa-2x"),
        footer = NULL,
        size = "s",
        style = "text-align:center"
      ))
      
      
      create_histogram_pdf(death_lst, file_name = "DoD_hist.pdf")
      
      # Stops the spinner from displaying
      removeModal(session)
      
      # Downloads from a web-page
      file.copy(from = "DoD_hist.pdf", file)
    })
  
  output$downloadMap <- downloadHandler(
    filename = "DoD_map.pdf",
    content = function(file) {
      
      showModal(modalDialog(
        title = "Creating map",
        icon("spinner", class = "fa-spin fa-2x"),
        footer = NULL,
        size = "s",
        style = "text-align:center"
      ))
      
      
      map <- county_choropleth(map_dat,legend = "Deaths per 100,000", title = "Deaths of Despair and Interpersonal Violence")
      
      # Opens up pdf graphic device --- allows us to save our charts
      # to the specified file
      pdf("DoD_map.pdf", height = 8.5, width = 11)
      
      plot(map)
      gph <- recordPlot()
      replayPlot(gph)
      
      # Turns off pdf graphic device
      dev.off()
      # Turns off all open graphic devices
      graphics.off()
      
      
      
      removeModal(session)
      
      file.copy(from = "DoD_map.pdf", file)
      
    })
  
}

shinyApp(ui, server)
