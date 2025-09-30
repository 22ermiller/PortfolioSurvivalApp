server <- function(input, output, session) {
  observeEvent(TRUE, {
    showModal(
      modalDialog(
        title = "Welcome!",
        card_body(
          HTML(
            "
  <p>This tool helps you estimate the likelihood that your financial plan will succeed using simulations based on an Economic Scenario Generator (ESG).</p>
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
                "nest_egg",
                "Nest Egg:",
                value = NA,
                format = "dollar"
              )
            ),
            column(
              6,
              currencyInput(
                "expenses",
                "Monthly Expenses:",
                value = NA,
                format = "dollar"
              )
            )
          ),
          pickerInput(
            "annuity_prop",
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

  # Logic for simulations --------------------------------------------------

  withdrawal_rate <- reactive({
    round((input$expenses * 12) / input$nest_egg, 4)
  })

  success_rate <- reactive({
    sim_df %>%
      filter(near(withdrawal_rate, withdrawal_rate(), tol = .0001)) %>%
      filter(near(
        annuity_prop,
        (as.numeric(input$annuity_prop) * .01),
        tol = .01
      )) %>%
      pull(success_rate)
  })

  observeEvent(input$annuity_prop, {
    print(paste0("Results:"))
    print(paste0(withdrawal_rate()))
    print(paste0(success_rate()))
    print(paste0(as.numeric(input$annuity_prop) * .01))
  })
}
