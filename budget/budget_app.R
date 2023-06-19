library(shiny)
library(magrittr)
library(dplyr)
library(magrittr)
library(ggplot2)


ui <- fluidPage(
  
  # checkboxGroupInput("timeframe", "Group by", c("transaction_month","transaction_week")),
  titlePanel("Budget app"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput("datesrange", 
                  "Dates:",
                  min = min(bank_stmt$transaction_yearmonth),
                  max = max(bank_stmt$transaction_yearmonth),
                  value = c(as.Date("2021-06-01"),as.Date("2022-01-01")),
                  timeFormat = "%Y-%m-%d"),
      
      checkboxGroupInput(inputId = "category_main",
                  label = "Pick a Category:",
                  choices = bank_stmt$category_main %>% unique())
    ),
    
    mainPanel(
  
  plotOutput(outputId = "budget_overall", click = "plot_click"),
  
  plotOutput(outputId = "budget_category"),
  
  plotOutput(outputId = "budget_overall_categories"),
  
  selectInput(inputId = 'category_major', "Category of choice", c('Bills',
                                                                  'Living Expenses',
                                                                  'Self-care Expenses',
                                                                  'Indulging Expenses',
                                                                  'Vacation')),
  plotOutput(outputId = "budget_category_major")
  ))) 


server <- function(input, output) {
  
  dat_1 <- reactive({bank_stmt %>% filter(!(category %in% c('Incoming', 'Cash withdrawal','Self Transfer')),
                                          !(is.na(transaction_yearmonth))) %>% 
    group_by(transaction_yearmonth, category_main) %>% 
    summarise(total_amount = sum(amount, na.rm = T)) %>% 
    ungroup()})
    
  output$budget_overall <-   renderPlot(
    {
      ggplot(dat_1(),aes(transaction_yearmonth, total_amount)) +
        geom_bar(stat = 'identity',aes(fill = category_main)) + 
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
                                'yellow4',
                                'pink',
                                'darkgrey',
                                'white',
                                'lightgreen',
                                'darkgoldenrod3',
                                'cyan',
                                'navy',
                                'orange4',
                                'purple3',
                                'salmon',
                                'green3',
                                'red3',
                                'thistle',
                                'thistle3'), aesthetics = "fill", breaks = waiver())
    }
  )
  
  dat <- reactive({bank_stmt[bank_stmt$category_main==input$category_main,] %>% 
      group_by(transaction_yearmonth) %>% 
      summarise(total_amount = round(sum(amount),0))})
  
  output$budget_category <- renderPlot(
    {
      
      ggplot(dat(),aes(transaction_yearmonth,total_amount))+
        geom_bar(position = 'stack', stat = 'identity', fill = 'pink') +
        geom_text(aes(label=total_amount), position=position_dodge(width=0.9), vjust=0.5) + 
        xlab(input$category)
    }
  )

  dat_2 <- reactive({bank_stmt %>% filter(!(category %in% c('Incoming', 'Cash withdrawal','Self Transfer',
                                                            'Transfers','Other')),
                                          !(is.na(transaction_yearmonth))) %>% 
      group_by(transaction_yearmonth, category_major) %>% 
      summarise(total_amount = sum(amount, na.rm = T)) %>% 
      filter(total_amount > 100) %>% 
      ungroup()})
  
  output$budget_overall_categories <-   renderPlot(
    {
      ggplot(dat_2(),aes(transaction_yearmonth, total_amount)) +geom_bar(position =position_dodge(), stat = 'identity',aes(fill = category_major)) + 
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
                                     'yellow4',
                                     'pink',
                                     'darkgrey',
                                     'white',
                                     'lightgreen',
                                     'darkgoldenrod3',
                                     'cyan',
                                     'navy',
                                     'orange4',
                                     'purple3',
                                     'salmon',
                                     'green3',
                                     'red3'),aesthetics = c("colour", "fill"), breaks = waiver())
    }
  )
  
  
  dat3 <- reactive({bank_stmt[bank_stmt$category_major==input$category_major,] %>% 
      group_by(transaction_yearmonth) %>% 
      summarise(total_amount = sum(amount))})
  
  output$budget_category_major <- renderPlot(
    {
      
      ggplot(dat3(),aes(transaction_yearmonth,total_amount))+
        geom_bar(position = 'stack', stat = 'identity', fill = 'lightgreen') +
        geom_text(aes(label=total_amount), position=position_dodge(), vjust=0.5) + 
        xlab(input$category)
    }
  )
  
}

shinyApp(ui = ui, server = server)
