### UI

# Spinner type
options("spinner.type" = 8)

## Define UI
ui <- fluidPage(
  titlePanel("TS exploration"),
  fluidRow(
    column(
      3,
      selectInput(
        "upc_id", "UPC ID",
        c("", products$upc_id |> sort()),
        selected = ""
      )
    ),
    column(3, uiOutput("store_id"))
  ),
  tabsetPanel(
    tabPanel("Products Data", DT::dataTableOutput("products"), hr()),
    tabPanel("Store Data", DT::dataTableOutput("store"), hr()),
    # tabPanel(
    #   "Correlations",
    #   fluidRow(hr(), column(10, withSpinner(plotOutput("corr", height = "800px")), offset = 1)),
    #   hr()
    # ),
    tabPanel(
      "TS Plot",
      hr(),
      fluidRow(
        column(4, selectInput(
          "column",
          "Feature",
          colnames(
            transactions_final |>
              as_tibble() |>
              select(-upc_id, -store_id, -week, -week_end_date)
          ),
          selected = "units"
        ), offset = 1)
      ),
      withSpinner(plotlyOutput("plot")),
      # RMSE
      fluidRow(column(1), htmlOutput("rmse")),
      div(
        style = "text-align: right; margin-right: 10px;",
        em("Deselect model in legend to hide it in plot")
      ),
      hr()
    ),
    tabPanel(
      "Forecast",
      sidebarLayout(
        sidebarPanel(
          tags$h3("Settings", style = "font-weight:bold;"),
          # Fixed sidebar with checkboxes and select input
          checkboxInput("feature", label = "Feature", value = 0),
          checkboxInput("display", label = "Display", value = 0),
          checkboxInput("tpr_only", label = "TPR", value = 0),
          width = 2,
        ),
        mainPanel(
          # Main panel with the plot wrapped in a spinner
          withSpinner(plotlyOutput("forecast")),
          div(
            style = "text-align: right; margin-right: 10px;",
            em("Deselect model in legend to hide it in plot")
          ),
          width = 10
        )
      ),
      hr()
    ),
    tabPanel(
      "Residuals",
      fluidRow(column(10, withSpinner(plotlyOutput("res_acf")), offset = 1)),
      hr(),
      fluidRow(column(10, withSpinner(plotlyOutput("res_dist")), offset = 1)),
      hr()
    )
  )
)
