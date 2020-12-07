library(shiny) # you may need to install.packages() this
library(tidyverse)

library(shiny)
library(fec16)
library(gganimate)
library(gapminder)
library(foreign)
library(rstanarm)
library(tidymodels)
library(png)

# include url
# use a function

walleye <- read.csv("raw_data/wae_length_data.csv") %>% 
    mutate(mean_temp = mean.daily.temp) %>% 
    select(mean_temp, Year, Length, Treatment, Lake)

ui <- fluidPage(navbarPage(
    "Minnesota Fisheries",
    
    # ____________________________________________________________________________
    
    tabPanel("About",
             h3("Hey, Minnesota fishermen! Welcome to your personal help\
                guide on where to go and what to do for the upcoming year of fishing! \
                This project is all about determining the best spots to go fish for walleye during the 2021 \ 
                season. We will tell you what Northern Minnesota lakes have the biggest and the smallest walleye on average, \
                and what our predictions are regarding the best lakes to hit next summer. Daily temperature and invasive species \
                treatments are all factors we have taken into account in making our predictive model.
                As you gear up for the spring and begin to day-dream about putting away those winter augers, we hope you \
                find this page useful to plan ahead for your spring and summer fishing adventures!")),
    
    # ____________________________________________________________________________
    
    tabPanel("Walleye Length by Year",
             
             sidebarPanel(
                 selectInput(
                     inputId = "selected_lake",
                     label = "Choose a lake from this list!",
                     choices = walleye$Lake
                 )),
             
             mainPanel(
                 
                 textInput(
                     inputId = "entered_text",
                     label = "Place your title text here:",
                     value = "Example Title"
                 ),  
                 
                 textOutput("lake_message"),
                 textOutput("text_message"),
                 plotOutput("walleye_plot")
             )),
    
    tabPanel("Model",
             
             textOutput("lake_message"),
             textOutput("text_message"),
             plotOutput("walleye_model_plot")
    ),
    
    tabPanel("Analysis",
             h3("We have found"))
    
))

# ____________________________________________________________________________


server <- function(input, output, session) {
    output$lake_message <- renderText({
        paste0("This is the lake you chose: ",
               input$selected_lake, "!")
    })
    
    output$text_message <- renderText({
        paste0("This is the label you typed: ",
               input$entered_text, "!")
    })
    
    walleye_reactive <- reactive({
        walleye
    })
    
    output$walleye_plot <- renderPlot({
        walleye_reactive() %>%
            filter(Lake == input$selected_lake) %>%
            
            ggplot(aes(x = Year, y = as.numeric(Length))) +
            geom_point() +
            labs(title = input$entered_text, 
                 x = "Year", 
                 y = "Length") +
            theme_bw() + 
            theme(axis.text.x = element_text(angle = 90))
        
    })
    
    # ____________________________________________________________________________
    
    fit_1 <- stan_glm(Length ~ Treatment,
                      data = walleye,
                      refresh = 0)
    
    print(fit_1, digits = 3)
    
    new_obs <- tibble(Treatment = c("ZM", "SWF", "ZM, SWF"))
    
    pe <- posterior_epred(fit_1, newdata = new_obs) %>% 
        as_tibble() %>% 
        mutate_all(as.numeric) %>% 
        rename(ZM = `1`,
               SWF = `2`,
               Both = `3`)
    
    model_reactive <- reactive({
        pe
    })
    
    output$walleye_model_plot <- renderPlot({
        model_reactive %>% 
            pivot_longer(cols = 1:3,
                         names_to = "Treatments",
                         values_to = "Length") %>% 
            ggplot(aes(Length, y = after_stat(count/sum(count)),
                       fill = Treatments)) + 
            geom_histogram(bins = 100,
                           color = "white") + 
            theme_bw() + 
            labs(title = "Posterior Probability Distribution",
                 subtitle = "Expected average walleye length by treatment",
                 x = "Length",
                 y = "Probability")
    })
    
}

# ____________________________________________________________________________



shinyApp(ui, server)



