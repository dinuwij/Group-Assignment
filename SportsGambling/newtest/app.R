library(shiny)
library(readxl)
library(ggplot2)
#load(file = "C:/Users/dwijayaweera/Documents/Open source programming/OpenSourceProgramming-master/OpenSourceProgramming-master/Group Assignment/datamart_final.Rdata")

ui <- fluidPage(
    h1("Sports Gambling Data"),
    
    sliderInput(inputId = "TotalBets", label = "No of Bets",
                min = 0, max = 500,
                value = c(1, 50)),
    selectInput("country", "Country",
                choices = c("All", unique(datamart_final$CountryName))),
    
    # Add a plot output as the first tab
tabsetPanel(    
    tabPanel(
        title = "Plot",    
    plotOutput("plot")),
    
    tabPanel(
        title = "Data table",
    DT::dataTableOutput("table")
    )
)
)

server <- function(input, output) {
    
    filtered_data <- reactive({
        data <- datamart_final
        data <- subset(
            data,
            TotalBets >= input$TotalBets[1] & TotalBets <= input$TotalBets[2]
        )
        if (input$country != "All") {
            data <- subset(
                data,
                input$country == datamart_final$CountryName
            )
        }
        data
    })
    
    #outputtable
    output$table <- DT::renderDataTable({
        data <- filtered_data()
        data
    })
    
    # Create the plot render function  
    output$plot <- renderPlot({
        # Use the same filtered data that the table uses
        data <- datamart_final
        data <- subset(
            data,
            TotalBets >= input$TotalBets[1] & TotalBets <= input$TotalBets[2]
        )
        if (input$country != "All") {
            data <- subset(
                data,
                input$country == datamart_final$CountryName
            )
        }
        ggplot(data, aes(OverallGains, TotalBets)) +
            geom_point() +
            scale_x_log10()
    })
}

shinyApp(ui, server)