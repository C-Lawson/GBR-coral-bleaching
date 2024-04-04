
# rsconnect::deployApp("/Users/rof011/GBR-coral-bleaching/apps/timeseries/", appPrimaryDoc="reeftimeseries.R", appName="DHWtimeseries", appTitle="DHWtimeseries")


library(plotly)
library(tidyverse)
library(sf)
library(shiny)
library(shinyWidgets)


df <- readRDS("gbr_shape_dhw.RDS") |>
  as.data.frame() |>
  mutate(reef_id = paste0(as.character(gbr_name), " (", id,")")) |>
  dplyr::select(-id, -gbr_name, -geometry) |>
  pivot_longer(-reef_id, names_to="year", values_to="dhw_max") |>
  mutate(year = as.numeric(str_extract_all(year, "\\d+"))) |>
  arrange(reef_id, year) |>
  mutate(reef_id=as.factor(reef_id))


ui <- fluidPage(

  tags$head(
    tags$style(HTML("
      .container-fluid {
        max-width: 1200px;
      }
    "))
  ),


  #titlePanel("Dynamic Temperature Plot by gbr_name"),
  tags$div(style = "margin-top: 100px;"), # Adjust the margin-top value as needed
  # Use sidebarLayout with sidebarPanel for inputs and mainPanel for plot
  sidebarLayout(
    sidebarPanel(
      tags$div(style = "margin-top: 2px;"), # Adjust the margin-top value as needed
      selectizeInput("reef_id", "Select reef(s):", choices = NULL,
                     options = list(maxOptions = 4000, server = TRUE), multiple = TRUE),
      tags$p(""),
      tags$p(""),
      materialSwitch("show_points", "Show Points", TRUE, status="danger"),
      materialSwitch("show_line", "Show Line", TRUE, status="danger"),
      materialSwitch("show_trajectory", "Show Trajectory", TRUE, status="danger"),
      materialSwitch("show_error", "Show Error", TRUE, status="danger"),
      width=3
    ),
    mainPanel(
      plotlyOutput("tempPlot"),
      width=8
    )
  )
)


server <- function(input, output, session) {

  observe({
    initial_value <- "Heron Reef (23-052)"
    updateSelectizeInput(session, "reef_id",
                         choices = unique(df$reef_id),
                         selected = initial_value,
                         server = TRUE)
  })



  output$tempPlot <- renderPlotly({

    df <- df |>
      filter_at(vars(reef_id), any_vars(. %in% unique(input$reef_id))) |>
      droplevels() |>
    mutate(reef_id=as.character(reef_id))

    if(nrow(df)>0){
    plot <- ggplot(data=df, aes(x = year, y = dhw_max)) +
      theme_bw() +
      scale_x_continuous(breaks = seq(1986,2024,2), limits=c(1986, 2024)) +
      scale_y_continuous(breaks = seq(0,18,2), limits=c(0, 18), oob = scales::squish) +
      theme(plot.title = element_text(size = 12),
            legend.position = 'none',
            text=element_text(family="Arial"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      geom_vline(xintercept = c(1998, 2002, 2016, 2017, 2020, 2022, 2024),
                 color="darkred", alpha=0.1, linewidth=2)# +
      #scale_fill_distiller(palette = "Reds", direction = 1)


    # Conditionally add legend
   # if(length(unique(input$reef_id)>1)){
      plot <- plot + labs(title = "Yearly maximum Degree Heating Week (DHW) trends", x="\nYear", y="max DHW")
    # } else if (length(unique(input$reef_id)==1)){
    #   plot <- plot + labs(title = paste0("Max yearly Degree Heating Week trends for ", unique(input$reef_id)),
    #                       x = "Year", y = "DHW")
    # }

#
    # Conditionally add points
    if(isTRUE(input$show_points)) {
    plot <- plot + geom_point(data=df, aes(x = year, y = dhw_max, fill=reef_id),
                               shape=21, color="black", size=2, stroke=0.25, show.legend=FALSE)
    }

    # Conditionally add line
    if(isTRUE(input$show_line)) {
      plot <- plot + geom_line(data=df, aes(x = year, y = dhw_max, color=reef_id))
    }

    # Conditionally add trajectory without error
    if(isTRUE(input$show_trajectory) & !isTRUE(input$show_error)) {
      plot <- plot +
        geom_smooth(data=df, aes(color=reef_id, x = year, y = dhw_max),
                     method = "gam", formula = y ~ s(x, bs = "cr", k = 10), linewidth=0.4, #color="darkred",
                    alpha=0.2, se = FALSE, show.legend = FALSE)
    }

    # Conditionally add error without trajectory
    if(!isTRUE(input$show_trajectory) & isTRUE(input$show_error)) {
      plot <- plot +
        geom_smooth(data=df, aes(fill=reef_id, x = year, y = dhw_max),
                    method = "gam", formula = y ~ s(x, bs = "cr", k = 10), color=NA, #fill="coral2",
                    alpha=0.2, se = TRUE, show.legend = FALSE)
    }

    # Conditionally add trajectory with error
    if(isTRUE(input$show_trajectory) & isTRUE(input$show_error)) {
      plot <- plot +
        geom_smooth(data=df, aes(fill=reef_id, x = year, y = dhw_max),
                    method = "gam", formula = y ~ s(x, bs = "cr", k = 10), color=NA, #fill="coral2",
                    alpha=0.2, se = TRUE, show.legend = FALSE) +
        geom_smooth(data=df, aes(color=reef_id, x = year, y = dhw_max),
                    method = "gam", formula = y ~ s(x, bs = "cr", k = 10), linewidth=0.4, #color="darkred",
                    alpha=0.2, se = FALSE, show.legend = FALSE)
    } else {
      plot
    }

    } else {

      df2 <- data.frame(year=0, dhw_max=0)
      plot <- ggplot(data=df2, aes(x = year, y = dhw_max)) +
        theme_bw() +
        scale_x_continuous(breaks = seq(1986,2024,2), limits=c(1986, 2024)) +
        scale_y_continuous(breaks = seq(0,18,2), limits=c(0, 18)) +
        theme(plot.title = element_text(size = 12),
              legend.position = 'none',
              text=element_text(family="Arial"),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        labs(title = "Yearly maximum Degree Heating Week (DHW) trends", x="\nYear", y="max DHW")
        geom_text(aes(x=2005, y=8, label="add reef"), size=18, alpha=0.2, color="aquamarine3")

    }

      ggplotly(plot, tooltip=c("reef_id", "year", "dhw_max")) |>
      style(hoveron = "color")


  })
}

shinyApp(ui = ui, server = server)


