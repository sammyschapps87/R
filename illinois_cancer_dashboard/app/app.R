
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(readxl)
library(reshape)
library(RColorBrewer)
library(scales)
library(ggrepel)

ill_cancer_data <- read.csv('../data/cancer_rates.csv')
zip_codes_df <- read.csv('../data/lake_county_zip_codes.csv')

### Renaming column
zip_codes_df$ZIP <-zip_codes_df$ï..ZIP

## Dropping old column
drop  <- subset(zip_codes_df, select = ("ï..ZIP"))
zip_codes_df <- zip_codes_df[,!(names(zip_codes_df) %in% drop)]

## Merging data sets 
complete_df <- merge(ill_cancer_data, zip_codes_df, by="ZIP")

## creating percent of population with cancer variable 
complete_df$percent_of_pop_w_cancer <- complete_df$All_Cancer / complete_df$Population

## Creating Labels variable containing percent values for population / all_cancer for pie chart labeling 
complete_df$labels = percent(round(complete_df$percent_of_pop_w_cancer, digits = 2))

## Creating Data subset for data melting 
cancer_type_subset <- complete_df[c("Lung_Bronc", "Breast_Can", "Prostate_C", "Urinary_Sy", "Town")]

## Melting data
melted_df <- melt(cancer_type_subset, id=c("Town"))



# Define UI for application that draws different plots
ui <- dashboardPage(
    dashboardHeader(title = "Lakeview County"),
    dashboardSidebar(), 
    dashboardBody(
        fluidRow(box(plotOutput("cancer_bar")), box(plotOutput("cancer_by_type_bar"))), 
        fluidRow(box(plotOutput("pie")), box(plotOutput("reg")))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$cancer_bar <- renderPlot({
        zip_code_bar + coord_flip() + labs(x = "All Cancer", y = "Towns in Lakeview County Illinois", 
        title = "Population with Cancer") + theme(plot.title = element_text(hjust = .5))
    })
    
    output$cancer_by_type_bar <- renderPlot({
        cancer_by_type_bar + scale_fill_brewer(palette = "Paired") + coord_flip() + theme_light() + 
        labs(x = "People with Cancer", y = "Towns in Lakeview County Illinois", title = "Population with Cancer by Type") + 
        theme(plot.title = element_text(hjust = .5))
    })
    
    output$pie <- renderPlot({
        pie
    })
    
    output$reg <- renderPlot({
        ggplot(complete_df, aes(x = complete_df$All_Cancer, y = complete_df$SHAPE_Area)) + 
            geom_point(color = "red") + geom_smooth(method = loess) + 
            labs(x = "Pop with Cancer ", y =  "Shape Area", title = "Cancer Patients vs Shape Area") + 
            theme(plot.title = element_text(hjust = .5))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
