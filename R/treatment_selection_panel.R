treatment_selection_panel_ui <- function(id) {
  ns = NS(id)
  div(
    style = "display: flex; justify-content: space-between; margin: 0 15px",
    selectInput(
      inputId = ns("intervention"),
      label = "Select Intervention",
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
      inputId = ns("reference"),
      label = "Select Reference",
      choices = NULL,
      selectize = FALSE
    )
  )
}

treatment_selection_panel_server <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Update available treatments when the data changes
      observe({
        updateSelectInput(inputId = "intervention", choices = data()$levels, selected = data()$levels[1])
      }) %>% bindEvent(data())
      
      # Update available comparators when treatment changes
      observe({
        items <- data()$levels
        items <- items[items != input$intervention]
        
        selected <- input$reference
        if (is.null(selected) || !selected %in% items) {
          selected <- items[1]
        }
        updateSelectInput(inputId = "reference", choices = items, selected = selected)
        
        # Allow the comparator selection to be updated given the new choices.
        reference_observer$resume()
      }) %>% bindEvent(input$intervention)
      
      # Used to swap treatments
      new_reference <- reactiveVal()
      
      # Swap treatments when button clicked
      observe({
        new_intervention <- input$reference
        # Prevent the comparator selection from updating until after the choices have been updated.
        reference_observer$suspend()
        new_reference(input$intervention)
        updateSelectInput(inputId = "intervention", selected = new_intervention)
      }) %>% bindEvent(input$swap_treatments)
      
      # Swap treatments when button clicked
      reference_observer <- observe({
        updateSelectInput(inputId = "reference", selected = new_reference())
        new_reference(NULL)
      }) %>% bindEvent(new_reference(), ignoreNULL = TRUE)
      
      return(
        list(
          reference = reactive({ input$reference }),
          intervention = reactive({ input$intervention })
        )
      )
    }
  )
}