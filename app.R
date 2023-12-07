# set up ------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(maps)
library(viridis)
library(rsconnect)
library(shinythemes)

# preparing data ----------------------------------------------------------

# imports child mortality data
child.mortality <- read.csv("data/shiny_data_childmortalityrates.csv") %>% # reads csv file from local folder
    # renames variables for simplicity
    rename(region = Entity,
           year = Year,
           child.mortality = Child.mortality) %>%
    # mutates variables to ensure cohesion with world.map data
    mutate(region = ifelse(region == "United States", "USA", region),
           year = as.numeric(year))

# imports world map data to generate chloropeth map
world.map <- map_data("world")

# lists colour choices for viridis colour palette
colour.viridis <- c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")

# user interface ----------------------------------------------------------
ui = fluidPage(

    # shiny theme
    theme = shinytheme("spacelab"),

    # navigation bar title
    navbarPage("Child Mortality",

               # tab panel "World Overview"
               tabPanel("World Overview",

                        # page header
                        h3("World Overview", style = "font-family: arial;"),

                        # page instructions and description
                        fluidRow(
                            column(12,
                                   p(strong("Description:",),
                                     "The intention of this page is to visualize the variation of child mortality across the world, over the years. This is useful because it allows the user to easily compare and analyze a large amount of data."),
                                   p(strong("Instructions:", style = "font-family: arial;"),
                                     "Please select your year of interest to generate the corresponding choropleth map. This map visualizes the variation of child mortality across the world. The default colour palette is 'viridis' from the viridis R package, but can be customized to other palettes within the package."),
                                   hr())
                        ),

                        # shiny widgets
                        fluidRow(
                            # creates slider for year
                            column(3,
                                   selectInput("year", "Year", # name and label of slider
                                               choices = unique(child.mortality$year)), # choices
                                   selectInput("mapcolour", "Colour Palette", # name and label of selector
                                               choices = unique(colour.viridis), # choices
                                               selected = "viridis") # default choice
                                   ),
                            # creates chloropeth map
                            column(9, align = "center",
                                   plotOutput("map"))
                        ),

                        # explanation of score and data source
                        fluidRow(
                            column(12,
                                   hr())
                        ),

                        fluidRow(
                            column(6, align = "left",
                                   p(strong("Note:"),
                                     "Child mortality is defined as the death of children under five years of age per 1000 live births.")),
                            column(6, align = "right",
                                   p(strong("Data Source:"),
                                     "Based on free material from GAPMINDER.ORG, CC-BY LICENSE"))
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
                                     "The intention of this page is to visualize the change in child mortality for a country of interest. This is useful because it allows the user to track changes and see trends over a period of time."),
                                   p(strong("Instructions:"),
                                     "Please select your country of interest to generate a graph that outlines the changes in its child mortality. The default colour palette is 'mako' from the viridis R package, but can be customized to other palettes within the package."),
                                   hr())
                        ),

                        # shiny widgets
                        fluidRow(
                            # creates selector for country
                            column(3,
                                   selectInput("country", "Country", # name and label of selector
                                               choices = unique(child.mortality$region), # choices
                                               selected = "Canada"), # default selection
                                   selectInput("linecolour", "Colour Palette", # name and label of selector
                                               choices = unique(colour.viridis), # choices
                                               selected = "mako") # default selection
                            ),
                            # creates line graph
                            column(9,
                                   plotOutput("linegraph")),
                        ),

                        # explanation of score and data source
                        fluidRow(
                            column(12,
                                   hr())
                        ),

                        fluidRow(
                            column(6, align = "left",
                                   p(strong("Note:"),
                                     "Child mortality is defined as the death of children under five years of age per 1000 live births.")),
                            column(6, align = "right",
                                   p(strong("Data Source:"),
                                     "Based on free material from GAPMINDER.ORG, CC-BY LICENSE"))
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
                                     "The intention of this page is to allow the user to create their own custom data set of child mortality scores. This is useful because it allows the user to see the raw data in a more digestible format."),
                                   p(strong("Instructions:"),
                                     "Please select your year range and country/countries of interest. Once you are finished, press the 'Download' button to export your custom dataset as a CSV file."),
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
                                                   min = 1800, # minimum value
                                                   max = 2100, # maximum value
                                                   value = c(1900, 2000), # default range
                                                   sep = ""), # removes "," for thousand places
                                       # creates selector for country
                                       selectInput("data_country", "Countries", # name and label of selector
                                                   choices = unique(child.mortality$region), # choices
                                                   multiple = TRUE), # allows for multiple selections
                                       downloadButton('downloadData', 'Download') # name and label of button
                                   )
                            ),
                            column(9,
                                   # creates interactive data table
                                   dataTableOutput("data"))
                        ),

                        # explanation of score and data source
                        fluidRow(
                            column(12,
                                   hr())
                            ),

                        fluidRow(
                            column(6, align = "left",
                                   p(strong("Note:"),
                                     "Child mortality is defined as the death of children under five years of age per 1000 live births.")),
                            column(6, align = "right",
                                   p(strong("Data Source:"),
                                     "Based on free material from GAPMINDER.ORG, CC-BY LICENSE"))
                        ),
               ),
    )
)


# server ------------------------------------------------------------------
server <- function(input, output) {

    # creates chloropeth map
    output$map = renderPlot({
        # filters child.mortality data based on slider input
        child.mortality <- filter(child.mortality, year==input$year)
        # joins with world.map data for longitude and latitude coordinates
        child.mortality.map <- left_join(child.mortality, world.map, by = "region")
        # creates ggplot
        ggplot(child.mortality.map, aes(x = long, y = lat, fill = child.mortality, group = group)) +
            geom_polygon(colour = "#F8F9F9") + # outlines countries
            ggtitle(paste(input$year, "Child Mortality World Map")) + # map title
            scale_fill_viridis(option = input$mapcolour) + # viridis colour scheme
            labs(y = "Latitude", x = "Longitude", fill = "Child Mortality") # adjusts axis labels, legend title
    })

    # creates line graph
    output$linegraph = renderPlot({
        # filters child.mortality data based on selector input
        child.mortality <- filter(child.mortality, region==input$country)
        # creates bounds for x-axis
        x_min <- as.numeric(min(child.mortality$year, na.rm=TRUE))
        x_max <- as.numeric(max(child.mortality$year, na.rm=TRUE))
        # creates bounds for y-axis
        y_min <- as.numeric(min(child.mortality$child.mortality, na.rm=TRUE))
        y_max <- as.numeric(max(child.mortality$child.mortality, na.rm=TRUE))
        # creates ggplot
        ggplot(child.mortality, aes(x = year, y = child.mortality, colour = child.mortality)) +
            geom_line(linewidth = 2) + # line size
            geom_point(size = 2) + # point size
            ggtitle(paste("The Child Mortality of", input$country, "Over the Years")) + # graph title
            scale_colour_viridis(option = input$linecolour) + # viridis colour scheme
            labs(x = "Year", y = "Child Mortality", colour = "Child Mortality") + # adjusts axis labels, legend title
            scale_x_continuous(breaks = seq(x_min, x_max, by = 20)) + # specifies x-axis increments
            scale_y_continuous(breaks = seq(0, y_max, by = 50))
    })

    dataInput <- reactive({ # allows interactive data table to changes reactively to input
            # data set
            child.mortality %>%
                # filters for year range
                filter(year > input$data_year[1] & year < input$data_year[2]) %>%
                # filters for country/countries
                subset(region %in% input$data_country) %>%
                # renames variables for clarity
                rename("Country" = region,
                       "Year" = year,
                       "Child Mortality" = child.mortality)
    })

    # creates interactive data table
    output$data = renderDataTable({
        dataInput()
    })

    # creates download file
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("custom_childmortality_", Sys.Date(), ".csv", sep="")
            },
        content = function(file) {
            write.csv(dataInput(), file)
            }
        )
}

# shiny application -------------------------------------------------------
# runs shiny app
shinyApp(ui = ui, server = server)
