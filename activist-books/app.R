
library(shiny)
library(tidyverse)

gr <- read_rds(here::here("goodreads.rda"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Activist Books"),

    mainPanel(width = 12,
              DT::dataTableOutput("books_table"),
              em("Data Sources: https://www.goodreads.com/list/show/76908.Activist_memoirs")
              )

    # # Sidebar with a slider input for number of bins
    # sidebarLayout(
    #    # sidebarPanel(
    #         # checkboxGroupInput("genre",
    #         #             "Genre",
    #         #             choices = c("Race", "Memoir")
    #         #             )
    #     #),
    #
    #     # Show a plot of the generated distribution
    #     mainPanel(
    #        dataTableOutput("books_table")
    #     )
    # )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$books_table <- DT::renderDataTable({
      gr
      #gr |>
        #filter(map_lgl(gr$genres, ~`%in%`("Race", .x)))

    })
}

# Run the application
shinyApp(ui = ui, server = server)
