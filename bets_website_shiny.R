#
# This interactive graph provides total number of bets per website
#

library(shiny)
load(file = 'C:/Users/alopezzeron/Desktop/Fall_2019/Business_Analytics_Tools_OS/Group_Assignment/Group-Assignment/datamart_final.Rdata')

countrynamesort = unique(datamart_final$CountryName)
countrynamesort = sort(countrynamesort)

# Use a fluid layout
ui = fluidPage(    
    
    # Give the page a title
    titlePanel("Number of Bets Per Website"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
        
        # Define the sidebar with one input
        sidebarPanel(
                                                    
            selectInput("Application", "Name of the website:", 
                        choices=unique(datamart_final$`Application Description`)),
            hr(),
            helpText("Data from BWIN 2005")
        ),
        
        # Create a spot for the barplot
        mainPanel(
            plotOutput("WebsitePlot")  
        )
        
    )
)

# Define a server for the Shiny app
server = function(input, output) {
    
    # Fill in the spot we created for a plot
    output$WebsitePlot <- renderPlot({
        
        # Render a barplot
        hist(datamart_final[datamart_final$`Application Description` == input$Application, "TotalNumberofBets"],
             main=input$TotalNumberofBets,
             ylab="Number of people",
             xlab="Number of bets",
             breaks = 1000,
             xlim = c(0,2000))
    })
}
# Run the application 
shinyApp(ui = ui, server = server) 