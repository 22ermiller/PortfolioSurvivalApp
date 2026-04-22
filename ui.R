# Setup -------------------------------------------------------------------

# for theming in ggplots
#thematic_shiny(font = "auto", bg = "#FFFFFf", fg = "#2F2D2E")

ui <- page_sidebar(
  useShinyjs(),
  # Theming -----------------------------------------------------------------

  theme = bs_theme(
    version = 5,
    font_scale = NULL,
    preset = "journal",
    primary = "#2B6CB0"
  ),

  title = strong("Portfolio Survival Simulation"),

  sidebar = sidebar(
    currencyInput(
      "nest_egg",
      "Nest Egg:",
      value = NA,
      format = "dollar"
    ),
    currencyInput(
      "expenses",
      "Monthly Expenses:",
      value = NA,
      format = "dollar"
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

  hidden(
    div(
      id = "main_content",
      h3("Simulation Results:"),
      fluidRow(
        column(
          width = 6,
          value_box(
            "Probability of Success",
            value = textOutput("success_prob"),
            showcase = bsicons::bs_icon("piggy-bank-fill")
          )
        ),
        column(
          width = 6,
          card(success_prob_plotUI("prob_plot"))
        )
      ),
      fluidRow(
        column(
          width = 6,

          card(quantile_barUI("bar_plot"))
        )
      )
    )
  )
)
