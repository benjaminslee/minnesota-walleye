library(shiny)
library(tidyverse)
library(shiny)
library(fec16)
library(gganimate)
library(gapminder)
library(foreign)
library(rstanarm)
library(tidymodels)
library(png)
library(gt)
library(gtsummary)
library(broom.mixed)
library(shinythemes)
library(osmdata)
library(ggplot2)
library(ggthemes)

# include url
# use a function

walleye <- read.csv("raw_data/wae_length_data.csv") %>%
    mutate(mean_temp = mean.daily.temp) %>%
    select(mean_temp, Year, Length, Treatment, Lake)

lake_points <- read.csv("raw_data/lake_points.csv")

map_maker <- function(min_lon, max_lon, min_lat, max_lat) {
    bbx <- getbb("Minnesota")
    
    
    bbx <- rbind(x = c(min_lon, max_lon),
                 y = c(min_lat, max_lat))
    colnames(bbx) <- c("min", "max")
    
    highways <- bbx %>%
        opq() %>%
        add_osm_feature(
            key = "highway",
            value = c(
                "motorway",
                "trunk",
                "primary",
                "secondary",
                "tertiary",
                "motorway_link",
                "trunk_link",
                "primary_link",
                "secondary_link",
                "tertiary_link"
            )
        ) %>%
        osmdata_sf()
    
    require(sf)
    
    streets <- bbx %>%
        opq() %>%
        add_osm_feature(
            key = "highway",
            value = c(
                "residential",
                "living_street",
                "service",
                "unclassified",
                "pedestrian",
                "footway",
                "track",
                "path"
            )
        ) %>%
        osmdata_sf()
    
    color_roads <- rgb(0.42, 0.449, 0.488)
    
    require(tigris)
    counties_MN <- counties(state = "MN",
                            cb = T,
                            class = "sf",
    )
    counties_MN <- st_crop(
        counties_MN,
        xmin = min_lon,
        xmax = max_lon,
        ymin = min_lat,
        ymax = max_lat
    )
    
    options(tigris_use_cache = TRUE)
    
    get_water <- function(county_GEOID) {
        area_water("MN", county_GEOID, class = "sf")
    }
    water <- do.call(rbind,
                     lapply(counties_MN$COUNTYFP, get_water))
    water <- st_crop(
        water,
        xmin = min_lon,
        xmax = max_lon,
        ymin = min_lat,
        ymax = max_lat
    )
    
    st_erase <- function(x, y) {
        st_difference(x, st_union(y))
    }
    counties_MN <- st_erase(counties_MN, water)
    
    final_map <- ggplot() +
        geom_sf(
            data = counties_MN,
            inherit.aes = FALSE,
            lwd = 0.0,
            fill = rgb(0.203, 0.234, 0.277)
        ) +
        geom_sf(
            data = streets$osm_lines,
            inherit.aes = FALSE,
            color = color_roads,
            size = .4,
            alpha = .65,
            lwd = 0.0,
            fill = rgb(1, 0.988, 0.890)
        ) +
        geom_sf(
            data = highways$osm_lines,
            inherit.aes = FALSE,
            color = color_roads,
            size = .6,
            alpha = .65
        ) +
        annotate(
            "point",
            x = -93.509216,
            y = 44.968670,
            color = rgb(1, 0.89, 0.709),
            size = 20
        ) +
        coord_sf(
            xlim = c(min(bbx[1, ]), max(bbx[1, ])),
            ylim = c(min(bbx[2, ]), max(bbx[2, ])),
            expand = FALSE
        ) +
        theme(legend.position = F) + theme_void() +
        theme(panel.background =
                  element_rect(fill = rgb(0.867, 0.961, 1)))
    final_map
    
}

fit_1 <- stan_glm(Length ~ Treatment,
                  data = walleye,
                  refresh = 0)

print(fit_1, digits = 3)

new_obs <- tibble(Treatment = c("ZM", "SWF", "ZM, SWF", "None"))

pe <- posterior_epred(fit_1, newdata = new_obs) %>%
    as_tibble() %>%
    mutate_all(as.numeric) %>%
    rename(ZM = `1`,
           SWF = `2`,
           Both = `3`,
           None = `4`)

ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage(
    "Minnesota Fisheries",
    
    # ____________________________________________________________________________
    
    tabPanel(
        "Introduction",
        
        mainPanel(
        
        h3(
            "Hey, Minnesota Fishermen!"
        ),
        
        h5(
            
            "Welcome to your personal help\
                guide on where to go and what to do for the upcoming year of fishing! \
                This project is all about determining the best spots to go fish for walleye during the 2021 \
                season. We will tell you what Northern Minnesota lakes have the biggest and the smallest walleye on average, \
                and what our predictions are regarding the best lakes to hit next summer. Invasive species \
                treatments are the primary factors we have taken into account in making our predictive model, and we have \
                also laid out an overview of age-0 walleye lengths over time and over daily temperatures.
                As you gear up for the spring and begin to day-dream about putting away those winter augers, we hope you \
                find this page useful to plan ahead for your spring and summer fishing adventures!"),
        h5("Your digital fishing guide, BL")
                
        ),
        
        
        
        fluidRow(
            splitLayout(cellWidths = c("33%", "33%", "33%"), 
                        plotOutput("intro_map_plot"), 
                        plotOutput("intro_map2_plot"),
                        plotOutput("intro_map3_plot"))

            
        ),
        
        h6("From left to right: Cass Lake, East Lake Vermillion, and Leech Lake")
    )),
    
    # ____________________________________________________________________________
    
    tabPanel(
        "Walleye Length",
        
        sidebarPanel(
            selectInput(
                inputId = "selected_lake",
                label = "Choose a lake from this list!",
                choices = walleye$Lake
            )
        ),
        
        mainPanel(

            plotOutput("year_plot"),
            plotOutput("temp_plot"),
            plotOutput("treatment_plot")
        )
    ),
    
    tabPanel(
        "Model & Expectations",
        
        sidebarPanel(
            selectInput(
                inputId = "selected_treatment",
                label = "Choose a treatment from this list!",
                choices = colnames(pe)
            ),
            gt_output("regression_table")
        ),
        
        mainPanel(
        plotOutput("walleye_model_plot"),
        
        h3("Analysis"),
        
        h5("Results."),
        
        fluidRow(
            splitLayout(cellWidths = c("33%", "33%", "33%"), 
                        plotOutput("mid_map_plot"), 
                        plotOutput("mid_map2_plot"),
                        plotOutput("mid_map3_plot"))),
        
        h6("From left to right:")
        
    )
    ),
    
    tabPanel("About",
             
             mainPanel(
                 
                 fluidRow(
                     splitLayout(cellWidths = c("33%", "33%", "33%"), 
                                 plotOutput("last_map_plot"), 
                                 plotOutput("last_map2_plot"),
                                 plotOutput("last_map3_plot"))),
             h3("Background"),
             
             h5("This is all about helping fishermen"),
             
             h3("Data"),
             
             h5("Data provided by the University of Minnesota"),
             
             h3("Acknowledgements"),
             
             h5("Thank you to Preceptor Kane, Tyler Simko, and the rest of the GOV 50 course staff for their assistance,\
              paitence, and enthusiasm."),
             
             h3("Author"),
             
             h5("Ben Lee is an A.B. candidate in Romance Studies and Government at Harvard College. \
             He is a born-and-raised Minnesotan and a sucker for a good walleye sandwich.
                Email: benjaminlee1@college.harvard.edu"),
             h5("Github Repo:"), tags$a(href="https://github.com/benjaminslee/minnesota-walleye.git",
                                       "https://github.com/benjaminslee/minnesota-walleye.git")
                    
             
             )
    ),
    
    tabPanel("“Stocked”",
             
             renderImage()
             
             )
    
    )

# ____________________________________________________________________________


server <- function(input, output, session) {

    output$Poem <- renderT
    
    
    output$intro_map_plot <- renderPlot({
        
        map_maker(min_lon = lake_points$Lon[6] - 0.2, max_lon = lake_points$Lon[6] + 0.2, 
                  min_lat = lake_points$Lat[6] - 0.2, max_lat = lake_points$Lat[6] + 0.2) 
    })
    
    output$intro_map2_plot <- renderPlot({
        
        map_maker(min_lon = lake_points$Lon[9] - 0.2, max_lon = lake_points$Lon[9] + 0.2, 
                  min_lat = lake_points$Lat[9] - 0.2, max_lat = lake_points$Lat[9] + 0.2)
    })
    
    output$intro_map3_plot <- renderPlot({
        
        map_maker(min_lon = lake_points$Lon[1] - 0.2, max_lon = lake_points$Lon[1] + 0.2, 
                  min_lat = lake_points$Lat[1] - 0.2, max_lat = lake_points$Lat[1] + 0.2)
    })
    
    output$mid_map_plot <- renderPlot({
        
        map_maker(min_lon = lake_points$Lon[2] - 0.2, max_lon = lake_points$Lon[2] + 0.2, 
                  min_lat = lake_points$Lat[2] - 0.2, max_lat = lake_points$Lat[2] + 0.2)
    })
    
    output$mid_map2_plot <- renderPlot({
        
        map_maker(min_lon = lake_points$Lon[3] - 0.2, max_lon = lake_points$Lon[3] + 0.2, 
                  min_lat = lake_points$Lat[3] - 0.2, max_lat = lake_points$Lat[3] + 0.2)
    })
    
    
    output$mid_map3_plot <- renderPlot({
        
        map_maker(min_lon = lake_points$Lon[4] - 0.2, max_lon = lake_points$Lon[4] + 0.2, 
                  min_lat = lake_points$Lat[4] - 0.2, max_lat = lake_points$Lat[4] + 0.2)
    })
    
    output$last_map_plot <- renderPlot({
        
        map_maker(min_lon = lake_points$Lon[5] - 0.2, max_lon = lake_points$Lon[5] + 0.2, 
                  min_lat = lake_points$Lat[5] - 0.2, max_lat = lake_points$Lat[5] + 0.2)
    })
    
    output$last_map2_plot <- renderPlot({
        
        map_maker(min_lon = lake_points$Lon[7] - 0.2, max_lon = lake_points$Lon[7] + 0.2, 
                  min_lat = lake_points$Lat[7] - 0.2, max_lat = lake_points$Lat[7] + 0.2)
    })
    
    output$last_map3_plot <- renderPlot({
        
        map_maker(min_lon = lake_points$Lon[8] - 0.2, max_lon = lake_points$Lon[8] + 0.2, 
                  min_lat = lake_points$Lat[8] - 0.2, max_lat = lake_points$Lat[8] + 0.2)
    })
    
    
    walleye_reactive <- reactive({
        walleye
    })
    
    output$year_plot <- renderPlot({
        walleye_reactive() %>%
            filter(Lake == input$selected_lake) %>%
            ggplot(aes(x = Year, y = as.numeric(Length))) +
            geom_point() +
            labs(title = "Age-0 Walleye Length Over Years 1983-2018",
                 x = "Year",
                 y = "Length (mm)") +
            theme_bw()
        
    })
    
    output$temp_plot <- renderPlot({
        walleye_reactive() %>% 
            filter(Lake == input$selected_lake) %>%
            ggplot(aes(x = mean_temp, y = as.numeric(Length))) +
            geom_point() +
            labs(title = "Age-0 Walleye Length Over Mean Daily Temperatures",
                 x = "Temperature (ºC)",
                 y = "Length (mm)") +
            theme_bw()
        
    })
    
    output$treatment_plot <- renderPlot({
        walleye_reactive() %>% 
            ggplot(aes(x = Treatment, y = as.numeric(Length))) +
            geom_point() +
            labs(title = "Age-0 Walleye Length Under Various Invasive Species Treatments",
                 x = "Treatment Type",
                 y = "Length (mm)") +
            theme_bw()
        
    })
    
    # ____________________________________________________________________________
    
    
    model_reactive <- reactive({
        pe
    })
    
    output$walleye_model_plot <- renderPlot({
        model_reactive() %>%
            pivot_longer(cols = 1:4,
                         names_to = "Treatment",
                         values_to = "Length") %>%
            filter(Treatment == input$selected_treatment) %>%
            ggplot(aes(Length, y = after_stat(count / sum(count)),
                       fill = Treatment)) +
            geom_histogram(bins = 100,
                           color = "white") +
            theme_bw() +
            labs(
                title = "Posterior Probability Distribution",
                subtitle = "Expected average age-0 walleye length by treatment",
                x = "Length (mm)",
                y = "Probability"
            )
    })
    
    output$regression_table <- render_gt({
        
        tbl_regression(fit_1, intercept = TRUE) %>% 
            as_gt() %>% 
            tab_header(title = "Regression of Walleye Length",
                       subtitle = "The Effect of Treatment on Age-0 Walleye Growth") %>% 
            tab_source_note(source_note = "raw_data/wae_length_data.csv")
    })
    
}



# ____________________________________________________________________________



shinyApp(ui, server)
