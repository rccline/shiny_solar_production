library(shiny)
library(suncalc)
library(tidyverse)
library(lubridate)

# Define the UI
ui <- fluidPage(
  titlePanel("Solar Position Heat Map"),
  sidebarLayout(
    sidebarPanel(
      numericInput("longitude", "Longitude:", value = -96.7026),
      numericInput("latitude", "Latitude:", value = 40.8136),
      textInput("location", "Location Name:", value = "Lincoln, NE"),
      actionButton("recalculate", "Recalculate"),
      tags$div(
        style = "margin-top: 20px;",
        "Instructions: Enter Longitude and Latitude, and rename the Location.",
        br(),
        "Created by Robert C. Cline", 
        br(),
        "Golden Triangle Land Services, Inc., Sidney, NE",
        br(),
        "email: rccline@fastmail.fm"
      )
    ),
    mainPanel(
      plotOutput("heatmap", height = "800px")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Function to generate the solar position table
  generate_solar_position_table <- function(latitude, longitude, year) {
    # ... (same code as before) ...
  }
  
  # Reactive expression to recalculate the solar position table
  solar_position_table <- reactive({
    req(input$latitude, input$longitude)
    year <- 2024
    generate_solar_position_table(input$latitude, input$longitude, year)
  })
  
  # Trigger the reactive expression when inputs change or the button is clicked
  observeEvent(input$recalculate, {
    solar_position_table()
  })
  
  # Render the heat map plot
  output$heatmap <- renderPlot({
    tilt_angles <- seq(0, 90, by = 2)
    azimuths <- seq(0, 350, by = 10)
    plot_data <- expand.grid(Tilt = tilt_angles, Azimuth = azimuths)
    
    max_production <- max(solar_position_table()$Max_Elevation)
    
    plot_data$Percentage <- round(100 * (90 - abs(plot_data$Tilt - optimal_tilt(input$latitude))) * (180 - abs(plot_data$Azimuth - 180)) / (90 * 180), 0)
    
    ggplot(plot_data, aes(x = Azimuth, y = Tilt, fill = Percentage, label = Percentage)) +
      geom_tile(color = "white", size = 0.5) +
      geom_text(color = "black", size = 3) +
      scale_fill_gradient(low = "red", high = "green", limits = c(0, 100), na.value = "grey50",
                          breaks = c(0, 60, 70, 100),
                          labels = c("0%", "60%", "70%", "100%"),
                          name = "Percent of Max\nAnnual Production") +
      scale_x_continuous(expand = c(0, 0), breaks = seq(0, 360, by = 30), labels = function(x) ifelse(x %in% c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330), x, "")) +
      scale_y_continuous(expand = c(0, 0), breaks = seq(0, 90, by = 2)) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0.5, size = 10)
      ) +
      labs(
        title = "Impact of Azimuth and Tilt on Solar Production, Percent of Maximum",
        subtitle = paste0("Modeling a Fixed Tilt System in ", input$location),
        x = "Azimuth (Direction Solar Array is Facing 0 = North 90 = East 180 = South 270 = West)",
        y = "Tilt Angle (degrees)",
        caption = "Azimuth: 0 = North, 90 = East, 180 = South, 270 = West"
      )
  })
  
  # Function to calculate the optimal tilt angle based on latitude
  optimal_tilt <- function(latitude) {
    abs(latitude) * 0.9 + 29
  }
}

# Run the Shiny app
shinyApp(ui = ui, server = server)