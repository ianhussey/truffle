# app.R

# ---- Packages ----
# devtools::install_github("ianhussey/truffle")
library(shiny)
library(truffle)
library(lavaan)
library(dplyr)
library(tidyr)
library(tidyselect)
library(latent2likert)
library(ggplot2)
library(missMethods)
library(readr)
library(zip)

# ---- UI ----
ui <- fluidPage(
  title = "truffle",
  titlePanel(
    title = span(
      img(src = "logo.png", height = 90),
      "truffle: simple data simulation"
    )
  ),
  sidebarLayout(
    sidebarPanel(
      textInput(
        "project_identifier",
        "Project identifier",
        value = "student 1"
      ),
      numericInput("seed", "Seed", value = 42, min = 0, step = 1),
      selectInput(
        "design",
        "Design",
        choices = c("Cross sectional study", "Between 2 groups experiment"),
        selected = "Between 2 groups experiment"
      ),
      numericInput("n", "Total N", value = 200, min = 1, step = 1),
      numericInput(
        "outcomes",
        "Number of scales",
        value = 3,
        min = 1,
        step = 1
      ),
      numericInput("items", "Items per scale", value = 10, min = 1, step = 1),
      numericInput(
        "response_options",
        "Number of response options (1-to-N)",
        value = 7,
        min = 2,
        step = 1
      ),
      numericInput(
        "alpha",
        "Approximate Cronbach's alpha",
        value = 0.70,
        min = 0.05,
        max = 0.999,
        step = 0.01
      ),
      numericInput(
        "r",
        "Approximate r among scales",
        value = 0.5,
        min = -0.99,
        max = 0.99,
        step = 0.01
      ),
      numericInput(
        "d",
        "Approximate Cohen's d between groups (experiments only)",
        value = 0.5,
        step = 0.01
      ),
      actionButton("run", "Run simulation"),
      #br(), br(),
      downloadButton("download_zip", "Download ZIP")
    ),
    mainPanel(
      h4("Simulation specifications"),
      tableOutput("spec_tbl"),
      tags$hr(),
      h4("Data (clean)"),
      tableOutput("clean_head"),
      tags$hr(),
      h4("Data (added untidiess)"),
      tableOutput("untidy_head")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  # storage
  vals <- reactiveValues(
    specs = NULL,
    data_clean = NULL,
    data_untidy = NULL
  )

  make_specs <- reactive({
    tibble(
      project_identifier = input$project_identifier,
      seed = input$seed,
      design = input$design,
      n = input$n,
      outcomes = input$outcomes,
      items = input$items,
      response_options = input$response_options,
      alpha = input$alpha,
      r = input$r,
      d = input$d
    )
  })

  observeEvent(input$run, {
    req(input$outcomes >= 1, input$items >= 1, input$response_options >= 2)

    specs <- make_specs()
    vals$specs <- specs

    outcomes <- specs$outcomes
    factors <- paste0("X", seq_len(outcomes), "_latent")
    prefixes <- paste0("X", seq_len(outcomes), "_item")

    if (identical(specs$design, "Between 2 groups experiment")) {
      data_clean <-
        truffle_likert(
          study_design = "factorial_between2",
          n_per_condition = specs$n * 2,
          factors = factors,
          prefixes = prefixes,
          n_items = rep(specs$items, outcomes),
          n_levels = specs$response_options,
          alpha = rep(specs$alpha, outcomes),
          r_among_outcomes = specs$r,
          approx_d_between_groups = specs$d,
          seed = specs$seed
        ) |>
        truffle_demographics()
    } else if (identical(specs$design, "Cross sectional study")) {
      data_clean <-
        truffle_likert(
          study_design = "crosssectional",
          n_per_condition = specs$n,
          factors = factors,
          prefixes = prefixes,
          n_items = rep(specs$items, outcomes),
          n_levels = specs$response_options,
          alpha = rep(specs$alpha, outcomes),
          r_among_outcomes = specs$r,
          seed = specs$seed
        ) |>
        truffle_demographics()
    } else {
      validate(need(FALSE, "Unknown design"))
    }

    # dirt pipeline
    data_untidy <- data_clean |>
      dirt_impossible_values(prop = .04, replacement_value = 8) |>
      mutate(completion_time = truffle_reaction_times(n = n())) |>
      dirt_numbers(cols = "completion_time", unit = " seconds") |>
      dirt_dates(col = "date") |>
      dirt_missingness(prop = .05) |>
      dirt_untidy(col = "block_trial") |>
      dirt_duplicates(prop = 0.05) |>
      relocate(date, .after = "id") |>
      relocate(completion_time, .after = "id") |>
      relocate(block_trial, .after = "id") |>
      dirt_colnames()

    vals$data_clean <- data_clean
    vals$data_untidy <- data_untidy
  })

  # previews
  output$spec_tbl <- renderTable({
    req(vals$specs)
    vals$specs
  })

  output$clean_head <- renderTable({
    req(vals$data_clean)
    head(vals$data_clean, 10)
  })

  output$untidy_head <- renderTable({
    req(vals$data_untidy)
    head(vals$data_untidy, 10)
  })

  # download zip
  output$download_zip <- downloadHandler(
    filename = function() {
      id <- gsub("[^A-Za-z0-9_-]", "_", input$project_identifier %||% "project")
      paste0(id, ".zip")
    },
    content = function(file) {
      req(vals$specs, vals$data_clean, vals$data_untidy)

      tmpdir <- tempfile("truffle_export_")
      dir.create(tmpdir)

      # write CSVs
      readr::write_csv(
        vals$specs,
        file.path(tmpdir, "simulation_specifications.csv")
      )
      readr::write_csv(vals$data_clean, file.path(tmpdir, "data_clean.csv"))
      readr::write_csv(vals$data_untidy, file.path(tmpdir, "data_untidy.csv"))

      # zip
      zip::zipr(
        zipfile = file,
        files = list.files(tmpdir, full.names = TRUE),
        include_directories = FALSE
      )
    }
  )
}

shinyApp(ui, server)
