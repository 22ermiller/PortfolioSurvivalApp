# Setup -------------------------------------------------------------------

# for theming in ggplots
#thematic_shiny(font = "auto", bg = "#FFFFFf", fg = "#2F2D2E")

ui <- page_sidebar(
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
      "annuity_prop2",
      label = "% of Portfolio in Annuities",
      choices = seq(0, 100, by = 10),
      selected = NULL,
      options = list(
        size = 5
      )
    )
  ),

  h3("Simulation Results:")
)
