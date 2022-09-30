library(shiny)
library(magrittr)
library(dplyr)
library(magrittr)
library(ggplot2)


ui <- fluidPage(
  
  # checkboxGroupInput("timeframe", "Group by", c("transaction_month","transaction_week")),

  plotOutput(outputId = "budget_overall"),
  
  selectInput(inputId = 'category_main', "Category of choice", c("Groceries",
                                                                 "Food delivery",
                                                                 "Bars",
                                                                 "Convenience shops",
                                                                 "Subscriptions",
                                                                 "Shopping",
                                                                 "Lunch at work",
                                                                 "Culture&Relax",
                                                                 "Beauty",
                                                                 "Self-Development",
                                                                 "Taxi",
                                                                 "Home",
                                                                 "Poker",
                                                                 "PhoneBill",
                                                                 "Other")),
  plotOutput(outputId = "budget_category")
  )


server <- function(input, output) {
  
  dat_1 <- reactive({bank_stmt %>% filter(!(category %in% c('Incoming', 'Cash withdrawal','Self Transfer')),
                                          !(is.na(transaction_yearmonth))) %>% 
    group_by(transaction_yearmonth, category_main) %>% 
    summarise(total_amount = sum(amount, na.rm = T)) %>% 
    ungroup()})
    
  output$budget_overall <-   renderPlot(
    {
      ggplot(dat_1(),aes(transaction_yearmonth, total_amount)) +geom_bar(position = 'stack', stat = 'identity',aes(fill = category_main)) + 
        scale_colour_manual(values = 
                              c('red',
                                'blue',
                                'green',
                                'yellow',
                                'violet',
                                'deeppink2',
                                'black',
                                'salmon',
                                'beige',
                                'lightgrey',
                                'purple',
                                'darkgreen',
                                'orange1',
                                'lightblue',
                                'brown',
                                'blue2',
                                'pink',
                                'darkgrey',
                                'white',
                                'lightgreen',
                                'darkgoldenrod3',
                                'cyan'), aesthetics = "fill", breaks = waiver())
    }
  )

  
  dat <- reactive({bank_stmt[bank_stmt$category_main==input$category_main,] %>% 
    group_by(transaction_yearmonth) %>% 
    summarise(total_amount = sum(amount))})
    
  output$budget_category <- renderPlot(
    {

        ggplot(dat(),aes(transaction_yearmonth,total_amount))+geom_bar(position = 'stack', stat = 'identity', fill = 'pink') +
        geom_text(aes(label=total_amount), position=position_dodge(width=0.9), vjust=0.5) + 
        xlab(input$category)
    }
  )
  
}
shinyApp(ui = ui, server = server)
