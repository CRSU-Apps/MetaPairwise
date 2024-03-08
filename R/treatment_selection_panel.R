treatment_selection_panel_ui <- function(id) {
  ns = NS(id)
  div(
    style = "display: flex; justify-content: space-between; margin: 0 15px",
    selectInput(
      inputId = ns("reference"),
      label = "Select Reference",
      choices = NULL,
      selectize = FALSE
    ),
    div(
      style = "display: inline-block; align-self: center;",
      actionButton(
        inputId = ns("swap_treatments"),
        label = NULL,
        icon = icon(name = "arrow-right-arrow-left")
      )
    ),
    selectInput(
      inputId = ns("intervention"),
      label = "Select Intervention",
      choices = NULL,
      selectize = FALSE
    )
  )
}

treatment_selection_panel_server <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Update available reference treatments when the data changes
      observe({
        updateSelectInput(inputId = "reference", choices = data()$levels, selected = data()$levels[1])
      }) %>% bindEvent(data())
      
      # Update available interventions when treatment changes
      observe({
        items <- data()$levels
        items <- items[items != input$reference]
        
        selected <- input$intervention
        if (is.null(selected) || !selected %in% items) {
          selected <- items[1]
        }
        updateSelectInput(inputId = "intervention", choices = items, selected = selected)
        
        # Allow the intervention selection to be updated given the new choices.
        intervention_observer$resume()
      }) %>% bindEvent(input$reference)
      
      # Used to swap treatments
      new_intervention <- reactiveVal()
      
      # Swap treatments when button clicked
      observe({
        new_reference <- input$intervention
        # Prevent the comparator selection from updating until after the choices have been updated.
        intervention_observer$suspend()
        new_intervention(input$reference)
        updateSelectInput(inputId = "reference", selected = new_reference)
      }) %>% bindEvent(input$swap_treatments)
      
      # Swap treatments when button clicked
      intervention_observer <- observe({
        updateSelectInput(inputId = "intervention", selected = new_intervention())
        new_intervention(NULL)
      }) %>% bindEvent(new_intervention(), ignoreNULL = TRUE)
      
      return(
        list(
          reference = reactive({ input$reference }),
          intervention = reactive({ input$intervention })
        )
      )
    }
  )
}