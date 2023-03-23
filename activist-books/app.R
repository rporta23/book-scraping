
library(shiny)
library(tidyverse)

# read in data
githubURL <- "https://github.com/rporta23/book-scraping/raw/main/data_categories.rda"
gr <- readRDS(url(githubURL))
#gr <- read_rds(here::here("data_categories.rda"))

# list of categories
categories <- c("All", "Urbanism", "Race", "Women's Rights", "Disability Justice", "LGBT","Environmentalism",
                "Education", "Public Health", "Religion", "Arts", "Government")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Activist Memoirs Database"),

    sidebarLayout(
      sidebarPanel(
        radioButtons(
          inputId = "category",
          label = "Filter Category",
          choices = categories,
          selected = "All"
        )
      ),

      mainPanel(width = 12,
                DT::dataTableOutput("books_table"),
                em("Data Sources:"), br(),
                a("goodreads", href = "https://www.goodreads.com/list/show/76908.Activist_memoirs"), br(),
                a("Amazon Best Sellers", href = "https://www.amazon.com/Best-Sellers-Books-Social-Activist-Biographies/zgbs/books/9681289011"), br(),
                a("Parnassus Books", href = "https://www.parnassusbooks.net/browse/book/BIO032000"), br(),
                p("Source code for this app: https://github.com/rporta23/book-scraping"),
                a("Contribute a new book to the database here", href = "https://docs.google.com/spreadsheets/d/1j9WbbSm49T4H9xE0CHsqUiA8plcDHMHW7I0wFVfTRyk/edit#gid=0")
      )
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
      if(input$category != "All"){
        gr |>
          filter(category == input$category)
      }else {
        gr
      }


    })
}

# Run the application
shinyApp(ui = ui, server = server)
