# app.R

library(shiny)
library(vroom)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)
library(reactable)
library(shinydashboard)
library(leaflet)
library(rsconnect)


## Load data


conditions <- read_csv("conditions.csv")
patients <- read_csv("patients.csv")
observations <- read_csv("observations.csv")


## Define patients with COVID-19


covid_patient_ids <- conditions %>%
  filter(grepl("COVID-19", DESCRIPTION)) %>%
  pull(PATIENT)


covid_conditions <- conditions %>%
  filter(PATIENT %in% covid_patient_ids) 



## Define covid date


covid_dates <- conditions %>%
  filter(grepl("COVID-19", DESCRIPTION)) %>%
  rename(covid_date = START) %>%
  mutate(covid_date = as.Date(covid_date)) %>%
  group_by(PATIENT) %>%
  summarise(covid_date = min(covid_date, na.rm = TRUE), .groups = "drop")

covid_dates_uni <- covid_dates %>%
  group_by(PATIENT) %>%
  slice_min(covid_date, with_ties = FALSE) %>%
  ungroup()


covid_patients <- patients %>% filter(Id %in% covid_patient_ids) %>%
  left_join(covid_dates_uni, by = c("Id" = "PATIENT")) 
head(covid_patients)


deceased_patients <- patients %>%
  filter(Id %in% covid_patient_ids, !is.na(DEATHDATE)) %>%
  select(Id, DEATHDATE)


deceased_patients <- patients %>%
  filter(Id %in% covid_patient_ids, !is.na(DEATHDATE)) %>%
  select(Id, DEATHDATE)

covid_lab_values <- observations %>%
  filter(PATIENT %in% covid_patient_ids) %>%
  filter(CODE %in% c("731-0", "26881-3")) %>%
  select(DATE, PATIENT, CODE, DESCRIPTION, VALUE, UNITS) %>%
  left_join(
    covid_dates %>%
      group_by(PATIENT) %>%
      slice_min(covid_date, with_ties = FALSE) %>%
      ungroup(),
    by = "PATIENT"
  ) %>%
  mutate(
    DATE = as.Date(DATE),
    covid_date = as.Date(covid_date),
    Days = as.integer(DATE - covid_date)
  ) %>%
  left_join(
    deceased_patients %>%
      distinct(Id, .keep_all = TRUE),
    by = c("PATIENT" = "Id")
  ) %>%
  mutate(survivor = is.na(DEATHDATE))



# Summarise patient counts by city
city_summary <- covid_patients |>
  filter(!is.na(CITY), CITY != "") |>
  group_by(CITY) |>
  summarise(
    patient_count = n(),
    avg_lat = mean(LAT, na.rm = TRUE),
    avg_lon = mean(LON, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(patient_count))

# UI
ui <- dashboardPage(
  dashboardHeader(title = "COVID Patient Dashboard | Thieu Nguyen", titleWidth = 500),
  
  dashboardSidebar(width = 150,
                   sidebarMenu(
                     menuItem("Patient Count", tabName = "patients", icon = icon("dashboard")),
                     menuItem("Lab Values", icon = icon("th"), tabName = "labs",
                              badgeLabel = "new", badgeColor = "green")
                   )
  ),
  
  dashboardBody(
    tabItems(
      # Patient count tab
      tabItem(tabName = "patients",
              fluidRow(
                box(
                  width = 12,
                  status = "warning",
                  sliderInput(
                    inputId = "top_n",
                    label = "Select number of top cities to display:",
                    min = 1,
                    max = 20,
                    value = 5
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Top Cities by Patient Count",
                  width = 6,
                  status = "primary",
                  solidHeader = TRUE,
                  plotOutput("bar_chart", height = "400px")
                ),
                box(
                  title = "Patient Distribution Map",
                  width = 6,
                  status = "success",
                  solidHeader = TRUE,
                  leafletOutput("map", height = "400px")
                )
              )
      ),
      
      # Lab values tab
      tabItem(tabName = "labs",
              fluidRow(
                box(
                  title = "Lab Values by Days from COVID Diagnosis",
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,
                  plotOutput("lab_plot", height = "500px")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive top N cities data
  top_n_cities <- reactive({
    city_summary |>
      slice_head(n = input$top_n)
  })
  
  # Bar chart output
  output$bar_chart <- renderPlot({
    data <- top_n_cities()
    
    ggplot(data, aes(x = reorder(CITY, patient_count), y = patient_count)) +
      geom_col(fill = "#2c7fb8") +
      geom_text(
        aes(label = patient_count),
        hjust = 1.1,
        color = "white",
        size = 4
      ) +
      coord_flip() +
      labs(x = "City", y = "Patient Count") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 10))
  })
  
  # Map output
  output$map <- renderLeaflet({
    leaflet(city_summary) |>
      addTiles() |>
      addCircleMarkers(
        lng = ~avg_lon,
        lat = ~avg_lat,
        label = ~paste0(CITY, ": ", patient_count, " patients"),
        radius = ~sqrt(patient_count) * 0.7,
        fillOpacity = 0.6,
        color = "#e6550d",
        stroke = FALSE
      )
  })
  

  
  # Lab values dual boxplot
  output$lab_plot <- renderPlot({
    ggplot(covid_lab_values, aes(x = factor(Days), y = as.numeric(VALUE), fill = survivor)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.7, position = position_dodge()) +
      facet_wrap(~DESCRIPTION, scales = "free_y") +
      scale_fill_manual(
        values = c("FALSE" = "#e41a1c", "TRUE" = "#377eb8"),
        labels = c("Deceased", "Survivor")
      ) +
      labs(
        title = "Lab Values by Days from COVID Diagnosis",
        x = "Days Since COVID Diagnosis",
        y = "Lab Value",
        fill = "Survival Status"
      ) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 8))
  })
}

# Run the app
shinyApp(ui, server)

