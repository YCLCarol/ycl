library(shiny)
library(plotly)
library(sportyR)
library(tidyverse)
library(baseballr)

baseball <- read.csv(file.path("data", "baseball2024.csv"))



# Replace empty or NA values in `events` with values from `description`
baseball <- baseball %>%
  mutate(events = if_else(is.na(events) | events == "", description, events))

# Calculate location_x and location_y for plotting
baseball <- baseball %>%
  mutate(location_x = 2.5 * (hc_x - 125.42),
         location_y = 2.5 * (198.27 - hc_y))

# Add a column for result categories
baseball <- baseball %>%
  mutate(result = case_when(
    events == "field_out" ~ "Out",
    events == "force_out" ~ "Out",
    events == "sac_fly" ~ "Out",
    events == "double_play" ~ "Out",
    events == "fielders_choice" ~ "Out",
    events == "fielders_choice_out" ~ "Out",
    events == "grounded_into_double_play" ~ "Out",
    events == "field_error" ~ "Error",
    events == "single" ~ "Single",
    events == "double" ~ "Double",
    events == "triple" ~ "Triple",
    events == "home_run" ~ "Home Run"
  ))

# Define the UI
ui <- fluidPage(
  titlePanel("MLB Visualization: Strike Zone and Spray Chart"),
  
  tabsetPanel(
    # Tab for Strike Zone
    tabPanel(
      "Strike Zone",
      sidebarLayout(
        sidebarPanel(
          selectizeInput("sz_player", "Select Player:", 
                         choices = NULL, 
                         options = list(
                           placeholder = 'Type or select a player',
                           searchField = 'text'
                         )),
          selectInput("sz_zone", "Select Zone:", 
                      choices = NULL),
          selectInput("sz_pitch_name", "Select Pitch Name:", 
                      choices = NULL)
        ),
        mainPanel(
          plotlyOutput("strikeZonePlot", height = "800px")
        )
      )
    ),
    # Tab for Spray Chart
    tabPanel(
      "Spray Chart",
      sidebarLayout(
        sidebarPanel(
          selectInput("select_result", 
                      h3("Select Result"), 
                      choices = c("Out", "Error", "Single", "Double", "Triple", "Home Run"), 
                      selected = "Out"),
          
          selectInput("select_player", 
                      h3("Player Name"),
                      choices = unique(baseball$player_name), 
                      selected = unique(baseball$player_name)[1])
        ),
        mainPanel(
          plotOutput(outputId = "chart1", height = "800px")
        )
      )
    )
  )
)

# Define the Server
server <- function(input, output, session) {
  ## Strike Zone Tab
  updateSelectizeInput(session, "sz_player", 
                       choices = unique(baseball$player_name))
  
  # Reactive filtered dataset for strike zone
  filtered_data_sz <- reactive({
    req(input$sz_player)
    
    data <- baseball %>% 
      filter(player_name == input$sz_player)
    
    if (!is.null(input$sz_zone) && input$sz_zone != "All Zones") {
      data <- data %>% filter(zone == input$sz_zone)
    }
    if (!is.null(input$sz_pitch_name) && input$sz_pitch_name != "All Pitches") {
      data <- data %>% filter(pitch_name == input$sz_pitch_name)
    }
    
    data
  })
  
  observeEvent(input$sz_player, {
    player_data <- baseball %>% filter(player_name == input$sz_player)
    
    updateSelectInput(session, "sz_pitch_name",
                      choices = c("All Pitches" = "All Pitches", 
                                  unique(player_data$pitch_name)),
                      selected = "All Pitches")
    
    sorted_zones <- sort(unique(player_data$zone), na.last = TRUE)
    updateSelectInput(session, "sz_zone",
                      choices = c("All Zones" = "All Zones", 
                                  sorted_zones),
                      selected = "All Zones")
  })
  
  output$strikeZonePlot <- renderPlotly({
    req(input$sz_player)
    
    plot_data <- filtered_data_sz()
    
    subtitle_text <- paste(
      if(is.null(input$sz_pitch_name) || input$sz_pitch_name == "All Pitches") "All Pitches" 
      else input$sz_pitch_name,
      "-",
      if(is.null(input$sz_zone) || input$sz_zone == "All Zones") "All Zones" 
      else paste("Zone", input$sz_zone)
    )
    
    strike_zone_plot <- ggplot() +
      geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.5, ymax = 3.5),
                fill = NA, color = "red", alpha = 0.2, size = 1.5) +
      geom_point(data = plot_data, 
                 aes(x = plate_x, y = plate_z, 
                     color = events,
                     text = paste(
                       "Event:", events, "<br>",
                       "Description:", description, "<br>",
                       "Velocity:", launch_speed, "mph<br>",
                       "Launch Angle:", launch_angle, "°<br>"
                     )),
                 size = 4, alpha = 0.7) +
      labs(
        title = paste("Strike Zone for", input$sz_player),
        subtitle = subtitle_text,
        x = "Horizontal Position (ft from center)",
        y = "Vertical Position (ft)",
        color = "Event"
      ) +
      theme_minimal() +
      coord_fixed(ratio = 1) +
      scale_x_continuous(limits = c(-2.5, 2.5)) +
      scale_y_continuous(limits = c(0, 5))
    
    ggplotly(strike_zone_plot, tooltip = "text", width = 800, height = 800)
  })
  
  ## Spray Chart Tab
  filtered_data <- reactive({
    baseball %>%
      filter(player_name == input$select_player, result == input$select_result)
  })
  
  output$chart1 <- renderPlot({
    req(filtered_data())
    data <- filtered_data()
    
    baseball_field <- geom_baseball(league = "MLB", display_range = "outfield")
    
    baseball_field +
      geom_point(data = data, aes(x = location_x, y = location_y, color = result,
                                ),
                 size = 3, alpha = 0.7) +
      scale_color_manual(values = c("Out" = "blue", 
                                    "Error" = "orange", 
                                    "Single" = "green", 
                                    "Double" = "purple", 
                                    "Triple" = "pink", 
                                    "Home Run" = "red")) +
      ggtitle(paste("Spray Chart for", input$select_player, "(", input$select_result, ")")) +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, hjust = 0.5))
  })
}

# Run the app
shinyApp(ui = ui, server = server)

