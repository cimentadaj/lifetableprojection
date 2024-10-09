handle_transitions <- function(input) {
  observeEvent(input$forward_step, {
    hide("landing_page")
    show("step_adjustment")
  })

  observeEvent(input$back_to_diagnostics, {
    hide("step_adjustment")
    show("landing_page")
  })

  observeEvent(input$forward_to_lifetable, {
    hide("step_adjustment")
    show("step_input")
  })

  observeEvent(input$back_to_adjustment, {
    hide("step_input")
    show("step_adjustment")
  })
}
