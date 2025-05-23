# Stylized UI

# Spinner type
options("spinner.type" = 8)

## Define UI
ui <- fluidPage(
  title = "Sales Forecast Dashboard",
  tags$head(
    tags$style(HTML("
      body {
        font-family: Arial, sans-serif;
        background-color: #008080;
        margin-top: 20px;
        margin-bottom: 20px;
      }

      .title-panel {
        font-family: Consolas;
        color: #E0FFFF;
        font-weight: bold;
        text-align: center;
        margin-bottom: 10px;
      }

      .tab-content {
        background-color: #fff;
        padding: 5px;
        border-top-right-radius: 8px;
        border-bottom-left-radius: 8px;
        border-bottom-right-radius: 8px;
        box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);
        max-width: 100%;
        overflow: hidden;
      }

      .nav-tabs > li > a {
        font-size: 14px;
        color: #008080;
        padding: 10px 15px;
        background-color: #f8f9fa;
        border-radius: 5px 5px 0 0;
        transition: all 0.3s ease;
      }

      .nav-tabs > li > a:hover {
        color: #ffffff;
        background-color: #20B2AA;
        border: none;
      }

      .nav-tabs > li.active > a {
        color: #ffffff;
        background-color: #20B2AA;
        border: none;
      }

      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        color: #ffffff;
        background-color: #20B2AA;
        border: none;
      }

      .checkbox {
        margin-bottom: 10px;
      }

      .form-group {
        margin-bottom: 15px;
      }

      hr {
        border: 0;
        border-top: 1px solid #ddd;
      }

      #ts-plot-content, #forecast-content {
        max-height: 600px;
        overflow-y: auto;
      }

    "))
  ),

  # Filters Row
  fluidRow(
    column(
      3,
      selectInput(
        "store_id", "Select Store ID:",
        store$store_id |> sort(),
        width = "100%"
      )
    ),
    column(
      3,
      selectInput(
        "upc_id", "UPC ID",
        NULL,
        width = "100%"
      ),
      offset = 1
    ),
    column(
      3,
      # Title
      titlePanel(tags$div("Sales Dashboard", class = "title-panel")),
      offset = 2
    )
  ),

  # Tab Panels
  tabsetPanel(
    id = "tabs",
    # Products Data Tab
    tabPanel(
      "Products Data",
      div(
        class = "tab-content",
        DT::dataTableOutput("products"),
        hr()
      )
    ),

    # Store Data Tab
    tabPanel(
      "Store Data",
      div(
        class = "tab-content",
        DT::dataTableOutput("store"),
        hr()
      )
    ),

    # TS Plot Tab
    tabPanel(
      "TS Plot",
      div(
        id = "ts-plot-content",
        class = "tab-content",
        fluidRow(
          column(
            3,
            selectInput(
              "column",
              "Choose Feature:",
              colnames(
                transactions_final |>
                  as_tibble() |>
                  select(-upc_id, -store_id, -week, -week_end_date)
              ),
              selected = "units"
            ),
            offset = 1
          )
        ),
        withSpinner(plotlyOutput("plot", height = "370px")),
        fluidRow(
          column(12, htmlOutput("rmse"))
        ),
        div(
          style = "text-align: right; margin-right: 10px;",
          em("Tip: Deselect model in the legend to hide it in the plot")
        ),
        hr()
      )
    ),

    # Forecast Tab
    tabPanel(
      "Forecast",
      div(
        id = "forecast-content",
        class = "tab-content",
        sidebarLayout(
          sidebarPanel(
            tags$h3("Settings", style = "font-weight: bold;"),
            checkboxInput("feature", "Show Feature", value = 0),
            checkboxInput("display", "Display Details", value = 0),
            checkboxInput("tpr_only", "Show TPR", value = 0),
            width = 2
          ),
          mainPanel(
            withSpinner(plotlyOutput("forecast", height = "370px")),
            div(
              style = "text-align: right; margin-right: 10px;",
              em("Tip: Deselect model in the legend to hide it in the plot")
            ),
            width = 10
          )
        ),
        hr()
      )
    ),

    # Residuals Tab
    tabPanel(
      "Residuals",
      div(
        class = "tab-content",
        fluidRow(
          column(5, withSpinner(plotlyOutput("res_acf"))),
          column(7, withSpinner(plotlyOutput("res_dist")))
        ),
        hr()
      )
    )
  )
)
