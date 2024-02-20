## @knitr shiny

library(shiny)
library(patchwork)
library(tidyverse)
library(imputeTS)
library(forecast)
library(stats)
library(sarima)
library(lubridate)

## To run the app locally, uncomment the following two lines and
## comment the next two `source(...)` function calls:
# source(here::here("scripts/functions_simulation.R"))
# source(here::here("scripts/functions_visualization.R"))

## To deploy on server, comment the above `source(...)`
## function calls and uncomment the following:
source("functions_simulation.R")
source("functions_visualization.R")

options(warn = -1)

ui <- fluidPage(
  tags$style(
    type = "text/css",
    "
               .shiny-input-container { overflow: hidden; }
               .shiny-input-spinner { display: none; }
               .shiny-slider-input { display: inline-block; width: 70%; vertical-align: middle; }
               .slider-label { display: inline-block; width: 20%; text-align: right; margin-right: 10px; vertical-align: middle; }
               "
  ),

  titlePanel("Simulating SARMA processes with time-dependent mean structures"),

  sidebarLayout(
    sidebarPanel(
      style = "height: 90vh; overflow-y: auto;",
      # Make the sidebar responsive

      actionButton("go", "Update the plots"),
      # N, seed, mean
      hr(style = "border-top: 1px solid #000000;"),

      fluidRow(
        column(
          3,
          numericInput(
            "fixed_n",
            "N (x100):",
            value = 1.5,
            width = '120px',
            step = 1
          )
        ),
        column(
          3,
          numericInput("fixed_sigma2", "sigma2:", value = 1, width = '60px')
        ),
        column(
          3,
          numericInput(
            "fixed_seed",
            "Seed:",
            value = 0,
            step = 1,
            width = '60px'
          )
        ),
        column(3, numericInput(
          "fixed_c", "c:", value = 0, width = '60px'
        ))
      ),
      hr(style = "border-top: 1px solid #000000;"),

      # phi, Phi
      fluidRow(
        column(
          6,
          tags$div(class = "slider-label", "ϕ:"),
          sliderInput(
            "fixed_ar",
            NULL,
            min = -0.9,
            max = 0.9,
            value = 0.45,
            step = 0.15
          )
        ),
        column(
          6,
          tags$div(class = "slider-label", "θ:"),
          sliderInput(
            "fixed_ma",
            NULL,
            min = -0.9,
            max = 0.9,
            value = 0.5,
            step = 0.15
          )
        )
      ),

      # theta, Theta
      fluidRow(
        column(
          6,
          tags$div(class = "slider-label", "Φ:"),
          sliderInput(
            "fixed_sar",
            NULL,
            min = -0.9,
            max = 0.9,
            value = 0.2,
            step = 0.15
          )
        ),
        column(
          6,
          tags$div(class = "slider-label", "Θ:"),
          sliderInput(
            "fixed_sma",
            NULL,
            min = -0.9,
            max = 0.9,
            value = 0.1,
            step = 0.1
          )
        )
      ),
      hr(style = "border-top: 1px solid #000000;"),

      # Days
      fluidRow(
        column(3, numericInput(
          "Mon", "Mon:", value = 1, width = '60px'
        )),
        column(3, numericInput(
          "Tue", "Tue:", value = 0, width = '60px'
        )),
        column(3, numericInput(
          "Wed", "Wed:", value = 2, width = '60px'
        )),
        column(3, numericInput(
          "Thu", "Thu:", value = -2, width = '60px'
        ))
      ),

      fluidRow(
        column(3, numericInput(
          "Fri", "Fri:", value = 3, width = '60px'
        )),
        column(3, numericInput(
          "Sat", "Sat:", value = 1, width = '60px'
        )),
        column(3, numericInput(
          "Sun", "Sun:", value = -1, width = '60px'
        )),
        column(3, numericInput(
          "Mult", "Scale:", value = 0.5, width = '60px'
        )),
        column(3) # Empty column to align the days correctly
      ),

      # Amplitude and phase shift
      fluidRow(
        column(
          4,
          numericInput("fixed_amp", "Amplitude:", value = 1.5, width = '100px')
        ),
        column(
          4,
          tags$div(class = "slider-label", "φ:"),
          sliderInput(
            "fixed_peak_shift",
            NULL,
            min = 0,
            max = 7,
            step = 0.5,
            value = 2
          )
        ),
        # Weekend effect
        column(
          4,
          numericInput(
            "fixed_wee",
            "Weekend effect:",
            value = 2,
            width = '100px'
          )
        )
      ),
      width = 4
    ),

    # Plot
    mainPanel(plotOutput(
      "myImage", height = "150px", width = "350px"
    ),
    width = 8)
  )
)


server <- function(input, output) {
  p <- eventReactive(input$go, {
    plot_sim_rows(
      fixed_c = input$fixed_c,
      fixed_dowe = c(
        input$Mon,
        input$Tue,
        input$Wed,
        input$Thu,
        input$Fri,
        input$Sat,
        input$Sun
      ) * input$Mult,
      fixed_amp = input$fixed_amp,
      fixed_peak_shift = input$fixed_peak_shift,
      fixed_wee = input$fixed_wee,
      fixed_sigma2 = input$fixed_sigma2,
      fixed_ma = input$fixed_ma,
      fixed_ar = input$fixed_ar,
      fixed_sar = input$fixed_sar,
      fixed_sma = input$fixed_sma,
      fixed_n = input$fixed_n * 100,
      fixed_seed = input$fixed_seed,
      prefix = NULL,
      for_shiny = TRUE,
      # prefix = NULL,
      file_format = "svg"
    )

  },
  ignoreNULL = FALSE)


  output$myImage <- renderPlot({
    print(p())
  }, width = 40 * 28.35 / 1.4,
  height = (4 * 7 + 0.5) * 28.35 / 2) # Convert cm to pixels if needed

}


shinyApp(ui = ui, server = server)
