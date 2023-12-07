
# set up ------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(maps)
library(wesanderson)
library(rsconnect)

# preparing data ----------------------------------------------------------

# imports world happiness score data
world.happiness <- read.csv("data/shiny_data_worldhappiness.csv") %>% # reads csv file from local folder
    # renames variables for simplicity
    rename(region = Entity,
           year = Year,
           happiness.score = Life.satisfaction.in.Cantril.Ladder..World.Happiness.Report.2022.) %>%
    # mutates variables to ensure cohesion with world.map data
    mutate(region = ifelse(region == "United States", "USA", region),
           year = as.numeric(year))

# imports world map data to generate chloropeth map
world.map <- map_data("world")

# generates wes-anderson themed colour palette
palette <- wes_palette("GrandBudapest2", 100, type = "continuous")


# user interface ----------------------------------------------------------
ui = fluidPage(
    # navigation bar title
    navbarPage("Happiness Score",

               # tab panel "World Overview"
               tabPanel("World Overview",

                        # page header
                        h3("World Overview", style = "font-family: arial;"),

                        # page instructions and description
                        fluidRow(
                            column(12,
                                   p(strong("Description:",),
                                     "The intention of this page is to visualize the variation of happiness scores across the world, over the years. This is useful because it allows the user to easily compare and analyze a large amount of data."),
                                   p(strong("Instructions:", style = "font-family: arial;"),
                                     "Drag the slider to your year of interest to generate the corresponding choropleth map. This map visualizes the variation of happiness scores across the world."),
                                   hr())
                        ),

                        # shiny widgets
                        fluidRow(
                            # creates slider for year
                            column(3,
                                   sliderInput("year", "Year", # name and label of slider
                                               min = 2004, # minimum value
                                               max = 2020, # maximum value
                                               value = 2010, # default value
                                               sep = "" # removes "," for thousand places
                                   )),
                            # creates chloropeth map
                            column(9, align = "center",
                                   plotOutput("map"))
                        ),

                        # explanation of score and data source
                        fluidRow(
                            column(12,
                                   hr(),
                                   p(strong("Note:"),
                                     "The happiness score was derived by asking respondants to evaluate their current life as a whole using the mental image of a ladder, with the best possible life for them as a 10 and worst possible as a 0. Each respondent provides a numerical response on this scale, referred to as the Cantril ladder."),
                                   p(strong("Data Source:"),
                                     "Helliwell, J. F., Layard, R., Sachs, J. D., De Neve, J.-E., Aknin, L. B., & Wang, S. (Eds.). (2022). World Happiness Report 2022. New York: Sustainable Development Solutions Network."),
                            )
                        ),
               ),

               # tab panel "By Country"
               tabPanel("By Country",

                        # page header
                        h3("By Country", style = "font-family: arial;"),

                        # page instructions and description
                        fluidRow(
                            column(12,
                                   p(strong("Description:"),
                                     "The intention of this page is to visualize the change in happiness scores for a country of interest. This is useful because it allows the user to track changes and see trends over a period of time."),
                                   p(strong("Instructions:"),
                                     "Please select your country of interest to generate a graph that outlines the changes in its happiness score."),
                                   hr())
                        ),

                        # shiny widgets
                        fluidRow(
                            # creates selector for country
                            column(3, align = "center",
                                   selectInput("country", "Country", # name and label of selector
                                               choices = unique(world.happiness$region), # choices
                                               selected = "Canada"), # default selection
                            ),
                            # creates line graph
                            column(9,
                                   plotOutput("linegraph")),
                        ),

                        # explanation of score and data source
                        fluidRow(
                            column(12,
                                   hr(),
                                   p(strong("Note:"),
                                     "The happiness score was derived by asking respondants to evaluate their current life as a whole using the mental image of a ladder, with the best possible life for them as a 10 and worst possible as a 0. Each respondent provides a numerical response on this scale, referred to as the Cantril ladder."),
                                   p(strong("Data Source:"),
                                     "Helliwell, J. F., Layard, R., Sachs, J. D., De Neve, J.-E., Aknin, L. B., & Wang, S. (Eds.). (2022). World Happiness Report 2022. New York: Sustainable Development Solutions Network."),
                            )
                        ),
               ),

               # tab panel "Custom Dataset"
               tabPanel("Custom Dataset",

                        # page header
                        h3("Custom Dataset", style = "font-family: arial;"),

                        # page instructions and description
                        fluidRow(
                            column(12,
                                   p(strong("Description:"),
                                     "The intention of this page is to allow the user to create their own custom data set of happiness scores. This is useful because it allows the user to see the raw data in a more digestible format."),
                                   p(strong("Instructions:"),
                                     "Please select your year range and country/countries of interest. Once you are finished, press the 'Filter' button to generate your custom dataset."),
                                   hr())
                        ),

                        # shiny widgets
                        fluidRow(
                            column(3,
                                   tabPanel(
                                       # instructions
                                       p("Export a custom dataset by selecting your year(s) and countries of interest"),
                                       # creates slider for year
                                       sliderInput("data_year", "Years", # name and label of slider
                                                   min = 2004, # minimum value
                                                   max = 2020, # maximum value
                                                   value = c(2004, 2020), # default range
                                                   sep = ""), # removes "," for thousand places
                                       # creates selector for country
                                       selectInput("data_country", "Countries", # name and label of selector
                                                   choices = unique(world.happiness$region), # choices
                                                   multiple = TRUE), # allows for multiple selections
                                       actionButton("generate", "Generate",  # name and label of button
                                                    class = "btn-success") # button class is "success"
                                   )
                            ),
                            column(9,
                                   # creates interactive data table
                                   dataTableOutput("data"))
                        ),

                        # explanation of score and data source
                        fluidRow(
                            column(12,
                                   hr(),
                                   p(strong("Note:"),
                                     "The happiness score was derived by asking respondants to evaluate their current life as a whole using the mental image of a ladder, with the best possible life for them as a 10 and worst possible as a 0. Each respondent provides a numerical response on this scale, referred to as the Cantril ladder."),
                                   p(strong("Data Source:"),
                                     "Helliwell, J. F., Layard, R., Sachs, J. D., De Neve, J.-E., Aknin, L. B., & Wang, S. (Eds.). (2022). World Happiness Report 2022. New York: Sustainable Development Solutions Network."),
                            )
                        ),
               ),
    )
)


# server ------------------------------------------------------------------
server <- function(input, output) {

    # creates chloropeth map
    output$map = renderPlot({
        # filters world.happiness data based on slider input
        world.happiness <- filter(world.happiness, year==input$year)
        # joins with world.map data for longitude and latitude coordinates
        world.happiness.map <- left_join(world.happiness, world.map, by = "region")
        # creates ggplot
        ggplot(world.happiness.map, aes(x = long, y = lat, fill = happiness.score, group = group)) +
            geom_polygon(colour = "#F8F9F9") + # outlines countries
            ggtitle(paste(input$year, "Happiness Score World Map")) + # map title
            scale_fill_gradientn(colours = palette) + # gradient colour scheme
            labs(y = "Latitude", x = "Longitude", fill = "Happiness Score") # adjusts axis labels, legend title
    })

    # creates line graph
    output$linegraph = renderPlot({
        # filters world.happiness data based on selector input
        world.happiness <- filter(world.happiness, region==input$country)
        # creates bounds for x-axis
        min <- as.numeric(min(world.happiness$year, na.rm=TRUE))
        max <- as.numeric(max(world.happiness$year, na.rm=TRUE))
        # creates ggplot
        ggplot(world.happiness, aes(x = year, y = happiness.score, colour = happiness.score)) +
            geom_line(size = 2) + # line size
            geom_point(size = 2) + # point size
            ggtitle(paste("The Happiness Score of", input$country, "Over the Years")) + # graph title
            scale_colour_gradientn(colours = palette) + # gradient colour scheme
            labs(x = "Year", y = "Happiness Score", colour = "Happiness Score") + # adjusts axis labels, legend title
            scale_x_continuous(breaks = seq(min, max, by = 1)) + # specifies x-axis increments
            ylim(0, 10)
    })

    # creates interactive data table
    output$data = renderDataTable({
        # button input
        input$generate
        # data table is only generated after button is clicked
        isolate(
            # data set
            world.happiness %>%
                # filters for year range
                filter(year > input$data_year[1] & year < input$data_year[2]) %>%
                # filters for country/countries
                subset(region %in% input$data_country) %>%
                # renames variables for clarity
                rename("Country" = region,
                       "Year" = year,
                       "Happiness Score" = happiness.score)
        )
    })
}

# shiny application -------------------------------------------------------
# runs shiny app
shinyApp(ui = ui, server = server)
