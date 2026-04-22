success_prob_plotUI <- function(id) {
  ns <- NS(id)
  tagList(
    ggiraph::girafeOutput(ns("plt"))
  )
}

success_prob_plotServer <- function(id, df, wr, annuity_prop) {
  moduleServer(id, function(input, output, session) {
    single_point <- reactive({
      df %>%
        filter(near(withdrawal_rate, wr(), tol = .0001)) %>%
        filter(near(
          annuity_prop,
          (as.numeric(annuity_prop()) * .01),
          tol = .01
        ))
    })

    p1 <- reactive({
      ggplot(
        data = df %>%
          mutate(
            tooltip = paste0(
              "Annuity %: ",
              scales::percent(as.numeric(annuity_prop))
            )
          )
      ) +
        geom_line_interactive(aes(
          x = withdrawal_rate,
          y = success_rate,
          color = factor(annuity_prop),
          alpha = factor(annuity_prop) == as.numeric(annuity_prop()) * .01,
          tooltip = tooltip,
          data_id = factor(annuity_prop)
        )) +
        geom_point(
          data = single_point(),
          aes(
            x = withdrawal_rate,
            y = success_rate,
            color = factor(annuity_prop) #,
            #tooltip = success_rate
          )
        ) +
        labs(x = "Withdrawal Rate", y = "Success Rate", color = "Annuity %") +
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::percent) +
        scale_color_discrete(labels = function(x) {
          paste0(as.numeric(x) * 100, "%")
        }) +
        scale_alpha_manual(
          values = c("TRUE" = 1, "FALSE" = 0.2),
          guide = "none"
        )
    })

    output$plt <- renderGirafe({
      girafe(
        ggobj = p1(),
        options = list(
          opts_hover(css = "opacity:1;stroke-width:1;", nearest_distance = 20)
        )
      )
    })
  })
}
