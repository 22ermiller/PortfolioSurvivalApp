server <- function(input, output, session) {
  observeEvent(TRUE, {
    showModal(
      modalDialog(
        title = "Welcome!",
        card_body(
          HTML(
            "
  <p>This tool helps you estimate the probability that your financial plan will succeed using simulations based on an Economic Scenario Generator (ESG). <i>The model assumes retirement at age 60.</i></p>
  <p>To get started, please provide:</p>
  <ol>
    <li><strong>Your anticipated savings at retirement</strong></li>
    <li><strong>Your expected monthly expenses</strong></li>
    <li><strong>The percentage of your savings to invest in annuities</strong> (the remainder is assumed to be invested in the stock market)</li>
  </ol>
  "
          ),
          fluidRow(
            column(
              6,
              currencyInput(
                "nest_egg1",
                "Nest Egg:",
                value = NA,
                format = "dollar"
              )
            ),
            column(
              6,
              currencyInput(
                "expenses1",
                "Monthly Expenses:",
                value = NA,
                format = "dollar"
              )
            )
          ),
          pickerInput(
            "annuity_prop1",
            label = "% of Portfolio in Annuities",
            choices = seq(0, 100, by = 10),
            selected = NULL,
            options = list(
              size = 5
            )
          )
        ),
        footer = actionButton(
          "see_results",
          "See Results",
          class = "btn-success"
        )
      )
    )
  })

  # Close modal when "See Results" is clicked
  observeEvent(input$see_results, {
    removeModal()
  })

  observeEvent(input$see_results, {
    shinyjs::show("main_content") # only reveal after button click
  })

  observeEvent(input$see_results, {
    updateCurrencyInput(
      session = session,
      inputId = "nest_egg",
      value = input$nest_egg1
    )
    updateCurrencyInput(
      session = session,
      inputId = "expenses",
      value = input$expenses1,
    )
    updatePickerInput(
      session = session,
      inputId = "annuity_prop",
      selected = input$annuity_prop1,
    )
  })

  # Logic for simulations --------------------------------------------------

  wr <- reactive({
    round((input$expenses * 12) / input$nest_egg, 3)
  })

  filtered_df <- reactive({
    sim_df %>%
      filter(near(withdrawal_rate, wr(), tol = .0001)) %>%
      filter(near(
        annuity_prop,
        (as.numeric(input$annuity_prop) * .01),
        tol = .01
      ))
  })

  observeEvent(input$annuity_prop, {
    print(paste0("Results:"))
    print(paste0("WR:", wr()))
    print(paste0("%:", as.numeric(input$annuity_prop) / 100))
    print(paste0(filtered_df()))
  })

  output$success_prob <- renderText({
    paste0(filtered_df()$success_rate * 100, "%")
  })

  success_prob_plotServer("prob_plot", sim_df, wr, reactive(input$annuity_prop))

  quantile_barServer("bar_plot", filtered_df, reactive(input$nest_egg))
}
