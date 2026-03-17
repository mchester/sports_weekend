library(shiny)
library(DT)

family_members <- c("Mom", "Marshall", "Bonita", "June", "Justine", "Tristan", "Savannah", "Holly", "Nick", "Gordon", "Rowyn")

ui <- fluidPage(
  titlePanel("Ultimate Toronto Sports Weekend! (April 10–12)"),
  hr(),
  
  fluidRow(
    column(3,
           selectInput("member", "Family Member:",
                       choices = c("— Select —", family_members))
    )
  ),
  
  fluidRow(
    column(3,
           h4("Friday, April 11"),
           checkboxInput("fri_jays", "Blue Jays - 7:07 PM", value = FALSE)
    ),
    column(3,
           h4("Saturday, April 12"),
           tags$label(
             tags$input(type = "checkbox", checked = "checked", disabled = "disabled"),
             " Blue Jays — 3:07 PM"
           ),
           br(),
           checkboxInput("sat_leafs", "Maple Leafs — 7:07 PM", value = FALSE)
    ),
    column(3,
           h4("Sunday, April 13"),
           checkboxInput("sun_jays",    "Blue Jays — 1:37 PM", value = FALSE),
           checkboxInput("sun_raptors", "Raptors — 6:00 PM",   value = FALSE)
    )
  ),
  
  hr(),
  actionButton("submit", "Submit"),
  br(), br(),
  
  h4("Roster"),
  DTOutput("roster_table")
)

server <- function(input, output, session) {
  
  roster <- reactiveVal(data.frame(
    Member   = character(),
    Friday   = character(),
    Saturday = character(),
    Sunday   = character(),
    stringsAsFactors = FALSE
  ))
  
  observeEvent(input$submit, {
    if (input$member == "— Select —") {
      showNotification("Select family member.", type = "warning")
      return()
    }
    
    fri <- if (input$fri_jays) "Blue Jays" else "—"
    
    sat_games <- "Blue Jays 3:07 PM"
    if (input$sat_leafs) sat_games <- paste0(sat_games, ", Maple Leafs 7:07 PM")
    
    sun_games <- paste(
      c(if (input$sun_jays)    "Blue Jays 1:37 PM",
        if (input$sun_raptors) "Raptors 6:00 PM"),
      collapse = ", "
    )
    if (nchar(sun_games) == 0) sun_games <- "—"
    
    new_row <- data.frame(
      Member   = input$member,
      Friday   = fri,
      Saturday = sat_games,
      Sunday   = sun_games,
      stringsAsFactors = FALSE
    )
    
    current <- roster()
    current <- current[current$Member != input$member, ]
    roster(rbind(current, new_row))
    
    showNotification(paste0(input$member, " saved!"), type = "message")
  })
  
  output$roster_table <- renderDT({
    df <- roster()
    if (nrow(df) == 0)
      df <- data.frame(Member="—", Friday="—", Saturday="—", Sunday="—")
    datatable(df, rownames = FALSE, options = list(dom = "t", pageLength = 20))
  })
}

shinyApp(ui, server)