library(shiny)
library(bslib)
library(bsicons)
library(plotly)
library(dplyr)
library(readxl)

# Helper function for axis labels
get_axis_label <- function(var) {
  if (var == "production") {
    return("Production (Tonnes)")
  } else if (var == "area") {
    return("Area Cultivated (Ha)")
  } else if (var == "yield") {
    return("Yield (Tonnes/Ha)")
  } else {
    return(var)
  }
}

ui <- page_fillable(
  theme = bs_theme(
    bootswatch = "minty",
    primary = "#449d95",
    secondary = "#ffbaba",
    info = "#bfddff",
    warning = "#feffba",
    light = "#c4ffb2",
    base_font = font_google("Roboto")
  ),
  
  titlePanel("Kenya Maize Production Insights"),
  
  layout_columns(
    col_widths = c(3, 3, 3, 3),
    value_box(
      title = "Highest Producing County", value = textOutput("topCounty"),
      showcase = bs_icon("award"), theme_color = "light"
    ),
    value_box(
      title = "Lowest Producing County", value = textOutput("lowCounty"),
      showcase = bs_icon("arrow-down"), theme_color = "secondary"
    ),
    value_box(
      title = "National Total Production (Tonnes)", value = textOutput("totalProd"),
      showcase = bs_icon("database"), theme_color = "info"
    ),
    value_box(
      title = "National Average Yield (Tonnes/Ha)", value = textOutput("avgYield"),
      showcase = bs_icon("bar-chart"), theme_color = "warning"
    )
  ),
  
  layout_columns(
    col_widths = c(7, 5),
    
    navset_tab(
      nav_panel("National Trends",
                selectInput("varInput", "Select Variable",
                            choices = c("Production" = "production", "Yield" = "yield", "Area" = "area"),
                            selected = "production"),
                navset_tab(
                  nav_panel("Trend Plot", card(full_screen = TRUE, plotlyOutput("trendPlot", height = "350px"))),
                  nav_panel("Yearly % Change", card(full_screen = TRUE, plotlyOutput("pctChange", height = "350px")))
                )
      ),
      
      nav_panel("County Analysis",
                layout_columns(
                  col_widths = c(6, 6),
                  selectInput("countySelect", "Select County", choices = NULL, multiple = TRUE),
                  selectInput("varInputCounty", "Select Variable",
                              choices = c("Production" = "production", "Yield" = "yield", "Area" = "area"),
                              selected = "production")
                ),
                card(full_screen = TRUE, plotlyOutput("multiCountyCompare", height = "400px"))
      ),
      
      nav_panel("Production vs Area",
                layout_columns(
                  col_widths = c(6, 6),
                  selectInput("yearScatter", "Select Year", choices = 2012:2023, selected = 2023)
                ),
                card(full_screen = TRUE, plotlyOutput("scatterProdArea", height = "400px"))
      ),
      
      nav_panel("Yield Heatmap",
                card(full_screen = TRUE, plotlyOutput("yieldHeatmap", height = "500px"))
      ),
      
    ),
    
    navset_tab(
      nav_panel("County Averages",
                layout_columns(
                  col_widths = c(6, 6),
                  selectInput("yearAvg", "Select Year", choices = 2012:2023, selected = 2023),
                  selectInput("varAvg", "Select Variable",
                              choices = c("Production" = "production", "Area" = "area", "Yield" = "yield"),
                              selected = "production")
                ),
                card(full_screen = TRUE, plotlyOutput("avgCountyBar", height = "500px"))
      ),
      
      nav_panel("Best & Worst Producers",
                layout_columns(
                  col_widths = c(6, 6),
                  card(full_screen = TRUE, plotlyOutput("bestProducersPlot", height = "450px")),
                  card(full_screen = TRUE, plotlyOutput("worstProducersPlot", height = "450px"))
                )
      )
    )
  )
)

server <- function(input, output, session) {
  df <- read_xlsx("data/maize-production-2012-2023-combined.xlsx") %>%
    mutate(year = as.integer(year)) %>%
    filter(year >= 2012, year <= 2023)
  
  updateSelectInput(session, "countySelect", choices = sort(unique(df$county)))
  
  observe({
    df_year <- df %>% filter(year == input$yearAvg)
    top <- df_year %>% arrange(desc(production)) %>% slice(1)
    low <- df_year %>% arrange(production) %>% slice(1)
    output$topCounty <- renderText(top$county)
    output$lowCounty <- renderText(low$county)
    
    total_prod <- sum(df_year$production, na.rm = TRUE)
    avg_yield <- mean(df_year$yield, na.rm = TRUE)
    output$totalProd <- renderText(format(total_prod, big.mark = ","))
    output$avgYield <- renderText(round(avg_yield, 2))
  })
  
  output$trendPlot <- renderPlotly({
    y_label <- get_axis_label(input$varInput)
    df %>%
      group_by(year) %>%
      summarise(value = sum(.data[[input$varInput]], na.rm = TRUE)) %>%
      plot_ly(x = ~year, y = ~value, type = "scatter", mode = "lines+markers") %>%
      layout(title = paste("National", tools::toTitleCase(input$varInput), "Trend"),
             xaxis = list(title = "Year"),
             yaxis = list(title = y_label))
  })
  
  output$pctChange <- renderPlotly({
    df %>%
      group_by(year) %>%
      summarise(val = sum(.data[[input$varInput]], na.rm = TRUE)) %>%
      mutate(pct_change = (val / lag(val) - 1) * 100) %>%
      plot_ly(x = ~year, y = ~pct_change, type = "scatter", mode = "lines+markers") %>%
      layout(title = paste("Yearly % Change in", tools::toTitleCase(input$varInput)),
             xaxis = list(title = "Year"),
             yaxis = list(title = "% Change"))
  })
  
  output$scatterProdArea <- renderPlotly({
    df %>%
      filter(year == input$yearScatter) %>%
      plot_ly(x = ~area, y = ~production, text = ~county, type = "scatter", mode = "markers", size = ~yield) %>%
      layout(title = "Production vs Area",
             xaxis = list(title = "Area Cultivated (Ha)"),
             yaxis = list(title = "Production (Tonnes)"))
  })
  
  output$yieldHeatmap <- renderPlotly({
    df %>%
      group_by(county, year) %>%
      summarise(avg_yield = mean(yield, na.rm = TRUE)) %>%
      plot_ly(
        x = ~year, 
        y = ~county, 
        z = ~avg_yield, 
        type = "heatmap", 
        colorscale = "Viridis",
        colorbar = list(title = "Average Yield (Tonnes/Ha)") # ✅ Legend title fixed
      ) %>%
      layout(
        title = "Average Yield Heatmap",
        xaxis = list(title = "Year"),
        yaxis = list(title = "County")
      )
  })
  
  
  output$multiCountyCompare <- renderPlotly({
    req(input$countySelect)
    y_label <- get_axis_label(input$varInputCounty)
    
    df %>%
      filter(county %in% input$countySelect) %>%
      group_by(county, year) %>%
      summarise(value = sum(.data[[input$varInputCounty]], na.rm = TRUE)) %>%
      plot_ly(x = ~year, y = ~value, color = ~county, type = "scatter", mode = "lines+markers") %>%
      layout(title = "Multi-County Comparison",
             xaxis = list(title = "Year"),
             yaxis = list(title = y_label))
  })
  
  output$avgCountyBar <- renderPlotly({
    var_col <- input$varAvg
    y_label <- get_axis_label(var_col)
    
    df %>%
      filter(year == input$yearAvg) %>%
      group_by(county) %>%
      summarise(val = if (var_col == "yield") mean(.data[[var_col]], na.rm = TRUE) else sum(.data[[var_col]], na.rm = TRUE)) %>%
      arrange(val) %>%
      plot_ly(x = ~val, y = ~reorder(county, val), type = "bar", orientation = "h") %>%
      layout(title = paste("Average", tools::toTitleCase(var_col), "per County"),
             xaxis = list(title = y_label),
             yaxis = list(title = "County"))
  })
  
  # ---- New Plots for Top & Worst Producers ----
  output$bestProducersPlot <- renderPlotly({
    best <- df %>%
      group_by(county) %>%
      summarise(avg_prod = mean(production, na.rm = TRUE)) %>%
      arrange(desc(avg_prod)) %>%
      slice(1:5)
    
    plot_ly(best, x = ~avg_prod, y = ~reorder(county, avg_prod),
            type = "bar", orientation = "h", marker = list(color = "forestgreen")) %>%
      layout(title = "Top 5 Producers",
             xaxis = list(title = "Average Production (Tonnes)"),
             yaxis = list(title = ""))
  })
  
  output$worstProducersPlot <- renderPlotly({
    worst <- df %>%
      group_by(county) %>%
      summarise(avg_prod = mean(production, na.rm = TRUE)) %>%
      arrange(avg_prod) %>%
      slice(1:5)
    
    plot_ly(worst, x = ~avg_prod, y = ~reorder(county, avg_prod),
            type = "bar", orientation = "h", marker = list(color = "firebrick")) %>%
      layout(title = "Bottom 5 Producers",
             xaxis = list(title = "Average Production (Tonnes)"),
             yaxis = list(title = ""))
  })
}

shinyApp(ui, server)