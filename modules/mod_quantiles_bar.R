quantile_barUI <- function(id) {
  ns <- NS(id)
  tagList(
    girafeOutput(ns("plt"))
  )
}

quantile_barServer <- function(id, df, nest_egg) {
  moduleServer(id, function(input, output, session) {
    p05 <- reactive(df()$value_at_death_p05 * nest_egg() / 100)
    p50 <- reactive(df()$value_at_death_p50 * nest_egg() / 100)
    p95 <- reactive(df()$value_at_death_p95 * nest_egg() / 100)

    p1 <- reactive({
      ggplot() +
        geom_rect(
          aes(xmin = p05(), xmax = p95(), ymin = -1, ymax = 1),
          fill = "#2B6CB0",
          color = "#14304eff",
          alpha = .6
        ) +
        geom_segment(
          aes(x = p50(), xend = p50(), y = -1.5, yend = 1.5),
          color = "#2B6CB0",
          linewidth = 1
        ) +
        labs(x = "Amount", y = NULL) +
        scale_x_continuous(labels = scales::label_currency())
    })

    output$plt <- renderGirafe(
      girafe(ggobj = p1())
    )
  })
}
