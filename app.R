#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rtweet)
library(tidyverse)
library(scales)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Twitter Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "How many trends would you like to see?",
                        min = 1,
                        max = 25,  # helps avoid API limits of 18,000/15 mins
                        value = 3) # default valie
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    trends <- get_trends("washington")

    output$trends <- renderPlot({
        
        trends[, c('trends','tweet_volume')] %>% 
            arrange(desc(tweet_volume)) %>% 
            rename(Trend = trend, Volume = tweet_volume)

        # generate bins based on input$bins from ui.R
        x    <- trends$Volume
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the plot with the specified number of bins
        ggplot() +
            geom_bar(mapping = aes(x = reorder(trend, -tweet_volume), 
                                   y = tweet_volume), 
                     stat = "identity") +
            scale_y_continuous(labels = comma) +
            theme(axis.title.x = element_blank()) +
            theme(axis.title.y = element_blank()) +
            labs(title = "Washington DC Twitter Trends",
                 caption = "\nSource: Data collected via rtweet - graphic by @mjhendrickson")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
