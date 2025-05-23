# SERVER LOGIC

## Server Logic
server <- function(input, output, session) {
  # Observe change in store_id and update upc_id options accordingly
  observeEvent(input$store_id, {
    # Filter UPC IDs based on selected store_id
    filtered_upc_ids <- transactions_final %>%
      filter(store_id == input$store_id) %>%
      distinct(upc_id) %>%
      pull(upc_id) %>%
      sort()

    # Update the upc_id selectInput choices
    updateSelectInput(
      session, "upc_id",
      choices = filtered_upc_ids
    )
  })

  # Filter dataset based on input
  ts <- function(data) {
    reactive({
      req(input$upc_id, input$store_id)
      data |>
        filter(
          upc_id == as.numeric(input$upc_id),
          store_id == as.numeric(input$store_id)
        )
    })
  }
  rtransactions_final <- ts(transactions_final)
  rfc_train <- ts(fc_train)
  rfc_arima <- ts(fc_arima)
  rfc_stl <- ts(fc_stl)
  rres <- ts(res)

  # Render Products and Store data tables
  output$products <- DT::renderDataTable(products)
  output$store <- DT::renderDataTable(store)

  # Render TS plot with train/test forecasts
  output$plot <- renderPlotly({
    tryCatch(
      {
        if (input$column != "units") stop()
        p <- rtransactions_final() |>
          ggplot() +
          # Historic data plot
          geom_line(aes(week, units)) +
          # Train/test forecasts
          geom_line(aes(week, p_units, color = .model), data = rfc_train()) +
          # CI 95% and 80%
          geom_ribbon(
            aes(week, ymin = low95, ymax = up95, fill = .model),
            alpha = 0.3, show.legend = FALSE,
            data = rfc_train()
          ) +
          geom_ribbon(
            aes(week, ymin = low80, ymax = up80, fill = .model),
            alpha = 0.3, show.legend = FALSE,
            data = rfc_train()
          ) +
          # Scale, labels and theme
          scale_x_yearweek(breaks = "1 month") +
          labs(color = "Train/Test Forecast Model:", x = "Week", y = "Units sold") +
          theme(axis.text.x = element_text(angle = 45))
        plt <- ggplotly(p)

        # Modify legend manually
        plt$x$data[[2]]$name <- plt$x$data[[2]]$legendgroup <-
          plt$x$data[[4]]$name <- plt$x$data[[4]]$legendgroup <-
          plt$x$data[[6]]$name <- plt$x$data[[6]]$legendgroup <- "ARIMA"
        plt$x$data[[3]]$name <- plt$x$data[[3]]$legendgroup <-
          plt$x$data[[5]]$name <- plt$x$data[[5]]$legendgroup <-
          plt$x$data[[7]]$name <- plt$x$data[[7]]$legendgroup <- "STL"
        plt
      },
      error = function(e) {
        # If error is arised, render historic data plot
        p <- rtransactions_final() |>
          ggplot() +
          geom_line(aes(week, !!sym(input$column))) +
          scale_x_yearweek(breaks = "1 month") +
          labs(x = "Week", y = input$column) +
          theme(axis.text.x = element_text(angle = 45))
        ggplotly(p)
      }
    )
  })

  # RMSE display
  output$rmse <- renderUI({
    tryCatch(
      {
        rmse <- rfc_train() |>
          left_join(rtransactions_final(), by = c("week", "upc_id", "store_id")) |>
          as_tibble() |>
          group_by(.model) |>
          summarise(rmse = sqrt(mean((units - p_units)^2))) |>
          pivot_wider(names_from = .model, values_from = rmse)
        HTML(glue("<b>ARIMA RMSE</b>: {round(rmse$arima, 2)}&nbsp;&nbsp;&nbsp;<b>ETS RMSE</b>: {round(rmse$stl, 2)}"))
      },
      error = function(e) {
        # If error is arised, retrun nothing
        ""
      }
    )
  })

  # Render Forecast plot
  output$forecast <- renderPlotly({
    tryCatch(
      {
        v_feature <- as.numeric(input$feature)
        v_display <- as.numeric(input$display)
        v_tpr_only <- as.numeric(input$tpr_only)

        # Check if input is a valid combination
        if (v_tpr_only == 1 && (v_display == 1 || v_feature == 1)) {
          stop("Not valid combination.")
        }

        # Filter fable for input
        f_rfc_arima <- rfc_arima() |>
          filter(
            feature == v_feature,
            display == v_display,
            tpr_only == v_tpr_only
          )

        p <- rtransactions_final() |>
          tail(10) |>
          ggplot() +
          # Historic data plot
          geom_line(aes(week, units)) +
          # ARIMA forecast plot
          geom_line(aes(week, p_units, color = .model), data = f_rfc_arima) +
          # CI 95% and 80%
          geom_ribbon(
            aes(week, ymin = low95, ymax = up95, fill = .model),
            alpha = 0.3, show.legend = FALSE,
            data = f_rfc_arima
          ) +
          geom_ribbon(
            aes(week, ymin = low80, ymax = up80, fill = .model),
            alpha = 0.3, show.legend = FALSE,
            data = f_rfc_arima
          ) +
          # STL forecast plot
          geom_line(aes(week, p_units, color = forcats::as_factor(.model)), data = rfc_stl()) +
          # CI 95% and 80%
          geom_ribbon(
            aes(week, ymin = low95, ymax = up95, fill = .model),
            alpha = 0.3, show.legend = FALSE,
            data = rfc_stl()
          ) +
          geom_ribbon(
            aes(week, ymin = low80, ymax = up80, fill = .model),
            alpha = 0.3, show.legend = FALSE,
            data = rfc_stl()
          ) +
          # Scale, labs and theme
          scale_x_yearweek(breaks = "1 week") +
          labs(color = "Forecast Model:", x = "Week", y = "Units sold") +
          theme(axis.text.x = element_text(angle = 45))
        plt <- ggplotly(p)

        # Modify legend manually
        plt$x$data[[2]]$name <- plt$x$data[[2]]$legendgroup <-
          plt$x$data[[3]]$name <- plt$x$data[[3]]$legendgroup <-
          plt$x$data[[4]]$name <- plt$x$data[[4]]$legendgroup <- "ARIMA"
        plt$x$data[[5]]$name <- plt$x$data[[5]]$legendgroup <-
          plt$x$data[[6]]$name <- plt$x$data[[6]]$legendgroup <-
          plt$x$data[[7]]$name <- plt$x$data[[7]]$legendgroup <- "STL"
        plt
      },
      error = function(e) {
        # If error is arised, render historic data plot
        p <- rtransactions_final() |>
          autoplot(units) +
          scale_x_yearweek(breaks = "1 month") +
          labs(x = "Week", y = "Units sold") +
          theme(axis.text.x = element_text(angle = 45))
        ggplotly(p)
      }
    )
  })

  # Residuals autocorrelation plot
  output$res_acf <- renderPlotly({
    tryCatch(
      {
        rres() |>
          ACF(.innov, lag_max = 55) |>
          autoplot() +
          scale_x_continuous(breaks = c(13, 26, 39, 52)) +
          labs(x = "Lag [weeks]", y = "ACF")
      },
      error = function(e) {
        # If error is arised, return void plot
        ggplot() +
          theme_void() +
          labs(title = "Not valid TS.")
      }
    ) |>
      ggplotly()
  })

  # Residuals distribution plot
  output$res_dist <- renderPlotly({
    tryCatch(
      {
        rres() |>
          ggplot() +
          geom_histogram(aes(x = .innov, fill = .model), bins = 50) +
          facet_wrap(~.model, scales = "free") +
          scale_x_continuous(breaks = scales::extended_breaks(10)) +
          labs(x = "Residuals", y = "Count") +
          theme(legend.position = "none")
      },
      error = function(e) {
        # If error is arised, return void plot
        ggplot() +
          theme_void() +
          labs(title = "Not valid TS.")
      }
    ) |>
      ggplotly()
  })
}
