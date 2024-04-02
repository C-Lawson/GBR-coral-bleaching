library(plotly)
library(shiny)

df <- gbr_shape_dhw |>
  as.data.frame() |>
  dplyr::select(-id, -geometry) |>
  pivot_longer(-gbr_name, names_to="year", values_to="dhw_max") |>
  mutate(year = as.numeric(str_extract_all(year, "\\d+"))) |>
  arrange(gbr_name, year)


ui <- fluidPage(
  titlePanel("Dynamic Temperature Plot by gbr_name"),
  selectInput("gbr_name", "Select gbr_name:", choices = unique(df$gbr_name)),
  checkboxInput("show_line", "Show Line", TRUE),
  checkboxInput("show_points", "Show Points", TRUE),
  checkboxInput("show_trajectory", "Show Trajectory", TRUE),
  checkboxInput("show_error", "Show Error", TRUE),
  plotlyOutput("tempPlot")
)

server <- function(input, output) {

  output$tempPlot <- renderPlotly({
    plot <- ggplot(df[df$gbr_name == input$gbr_name, ], aes(x = year, y = dhw_max)) +
      theme_bw() +
      labs(title = paste("DHW Trends for ", input$gbr_name),
           x = "Year", y = "DHW")

    # Conditionally add line
    if(input$show_line) {
      plot <- plot + geom_line(show.legend=FALSE)
    }

    # Conditionally add points
    if(input$show_points) {
      plot <- plot + geom_point(shape=21, size=2, show.legend=FALSE)
    }

    # Conditionally add trajectory with or without error
    if(input$show_trajectory) {
      error_option <- ifelse(input$show_error, TRUE, FALSE)
      plot <- plot + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr", k = 10), se = error_option, show.legend = FALSE)
    }

    ggplotly(plot)



  })
}

shinyApp(ui = ui, server = server)


