---
title: "Final Project STAT206A"
Author: "RAMEEN"
Date: "2024-06-16"
output: 
  flexdashboard::flex_dashboard:
    orientation: rows
    vertical_layout: fill
    runtime: shiny
---

```{r setup, include=FALSE}
library(flexdashboard)
```

```{r}
library(tidyverse)
library(plotly)
library(DT)
library(shiny)
library(readxl)
library(ggplot2)
library(reshape2)
library(ggcorrplot)
library(corrplot)
library(graphics)

exceldata1 <- read_excel("/Users/rafeiamunir/Final Project Spring/Counties Data Set 1.xlsx")

exceldata2 <- read_excel("/Users/rafeiamunir/Final Project Spring/Counties Data Set 2.xlsx")

exceldata3 <- read_excel("/Users/rafeiamunir/Final Project Spring/Counties Data Set 3.xlsx")

combined_data <- bind_rows(exceldata1, exceldata2, exceldata3)

View(combined_data)

data <- combined_data %>%
  select(state, pop.density, pop, democrat, republican, white, black, turnout)

View(data)
```

### Statistical Summary


```{r}
ui <- fluidPage(
  titlePanel(" Statistical Summary"),
  mainPanel(
    tabsetPanel(
      id = "plot_tabs",
      tabPanel("Box Plot and Whisker Plots",
               plotOutput("box_plot_pop"),
               plotOutput("box_plot_pop_density"),
               plotOutput("box_plot_democrat"),
               plotOutput("box_plot_republican"),
               plotOutput("box_plot_white"),
               plotOutput("box_plot_black"),
               plotOutput("box_plot_turnout")
      ),
      tabPanel("Density Plots",
               plotOutput("density_plot_pop"),
               plotOutput("density_plot_pop_density"),
               plotOutput("density_plot_democrat"),
               plotOutput("density_plot_republican"),
               plotOutput("density_plot_white"),
               plotOutput("density_plot_black"),
               plotOutput("density_plot_turnout")
      ),
      tabPanel("Correlation Plot", plotOutput("correlation_plot")),
      tabPanel("Bar Plot",
               plotOutput("bar_plot_white"),
               plotOutput("bar_plot_black")
      ),
      tabPanel("Summary Table", tableOutput("summary_table"))
    )
  )
)


server <- function(input, output) {
  
  #  Box plots
  
  output$box_plot_pop <- renderPlot({
    ggplot(combined_data, aes(x = state, y = pop)) +
      geom_boxplot() +
      labs(title = "Population Distribution by State", x = "State", y = "Population") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$box_plot_pop_density <- renderPlot({
    ggplot(combined_data, aes(x = state, y = pop.density)) +
      geom_boxplot() +
      labs(title = "Population Density Distribution by State", x = "State", y = "Population Density") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$box_plot_democrat <- renderPlot({
    ggplot(combined_data, aes(x = state, y = democrat)) +
      geom_boxplot() +
      labs(title = "Number of Democrats Distribution by State", x = "State", y = "Number of Democrats") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$box_plot_republican <- renderPlot({
    ggplot(combined_data, aes(x = state, y = republican)) +
      geom_boxplot() +
      labs(title = "Number of Republicans Distribution by State", x = "State", y = "Number of Republicans") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$box_plot_white <- renderPlot({
    ggplot(combined_data, aes(x = state, y = white)) +
      geom_boxplot() +
      labs(title = "Number of White Population Distribution by State", x = "State", y = "Number of White Population") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$box_plot_black <- renderPlot({
    ggplot(combined_data, aes(x = state, y = black)) +
      geom_boxplot() +
      labs(title = "Number of Black Population Distribution by State", x = "State", y = "Number of Black Population") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$box_plot_turnout <- renderPlot({
    ggplot(combined_data, aes(x = state, y = turnout)) +
      geom_boxplot() +
      labs(title = "Turnout Distribution by State", x = "State", y = "Turnout") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  # Density plots
  
  output$density_plot_pop <- renderPlot({
    ggplot(combined_data, aes(x= pop)) + 
      geom_density(color="green", alpha=0.5) +
      labs(title = "Population Density Plot", x = "Population")
  })
  
  output$density_plot_pop_density <- renderPlot({
    ggplot(combined_data, aes(x= pop.density)) + 
      geom_density(color="purple", alpha=0.5) +
      labs(title = "Population Density Density Plot", x = "Population Density")
  })
  
  output$density_plot_democrat <- renderPlot({
    ggplot(combined_data, aes(x= democrat)) + 
      geom_density(color="yellow", alpha=0.5) +
      labs(title = "Democrats Density Plot", x = "Number of Democrats")
  })
  
  output$density_plot_republican <- renderPlot({
    ggplot(combined_data, aes(x= republican)) + 
      geom_density(color="orange", alpha=0.5) +
      labs(title = "Republicans Density Plot", x = "Number of Republicans")
  })
  
  output$density_plot_white <- renderPlot({
    ggplot(combined_data, aes(x= white)) + 
      geom_density(color="blue", alpha=0.5) +
      labs(title = "White Population Density Plot", x = "Number of White Population")
  })
  
  output$density_plot_black <- renderPlot({
    ggplot(combined_data, aes(x= black)) + 
      geom_density(color="brown", alpha=0.5) +
      labs(title = "Black Population Density Plot", x = "Number of Black Population")
  })
  
  output$density_plot_turnout <- renderPlot({
    ggplot(combined_data, aes(x= turnout)) + 
      geom_density(color="cyan", alpha=0.5) +
      labs(title = "Turnout Density Plot", x = "Turnout")
  })
  
  # Correlation plot
  
  output$correlation_plot <- renderPlot({
    corr_data <- select(combined_data, -state, -pop.density, -pop, -democrat, -republican)
    str(corr_data)
    corr_data <- sapply(corr_data, as.numeric)
    correlation <- cor(corr_data)
    ggcorrplot(correlation, type = "upper", colors = c("blue", "green", "red"))
  })
  
  # Bar plots
  
  output$bar_plot_white <- renderPlot({
    ggplot(data = combined_data, aes(x = state, y = white, fill = state)) +
      geom_bar(stat = "identity") +
      labs(title = "Average White Population by State", x = "State", y = "Average White Population") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$bar_plot_black <- renderPlot({
    ggplot(data = combined_data, aes(x = state, y = black, fill = state)) +
      geom_bar(stat = "identity") +
      labs(title = "Average Black Population by State", x = "State", y = "Average Black Population") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  # Summary table
  
  output$summary_table <- renderTable({
    summary_data <- data %>%
      group_by(state) %>%
      summarize(
        Pop_Mean = mean(pop, na.rm = TRUE),
        Pop_SD = sd(pop, na.rm = TRUE),
        PopDensity_Mean = mean(pop.density, na.rm = TRUE),
        PopDensity_SD = sd(pop.density, na.rm = TRUE),
        Democrat_Mean = mean(democrat, na.rm = TRUE),
        Democrat_SD = sd(democrat, na.rm = TRUE),
        Republican_Mean = mean(republican, na.rm = TRUE),
        Republican_SD = sd(republican, na.rm = TRUE),
        White_Mean = mean(white, na.rm = TRUE),
        White_SD = sd(white, na.rm = TRUE),
        Black_Mean = mean(black, na.rm = TRUE),
        Black_SD = sd(black, na.rm = TRUE),
        Turnout_Mean = mean(turnout, na.rm = TRUE),
        Turnout_SD = sd(turnout, na.rm = TRUE)
      )
    summary_data
  })
}

# Run the app
shinyApp(ui, server)
```


column{.tabset data-width=400}
--------------------------------------------
###Explanation of Box plots and density plots:

### Box plot

```{r}
("Box plot:As can be seen from the display that constructing them separately would be a better option as all the variables have different values with different ranges. Constructing them together is compromising the plotting of few variables i.e.democrat, republican and white plots are being shrinked due to different ranges of value.")
```

### Density plots 

###Population change

```{r}

("The density plot of population shows a unimodal distribution, indicating that most states have populations clustered around a central value. There are some outliers with higher population densities, potentially representing urban areas or states with high population concentrations.")

```

###Population density

```{r}

("The density plot of population density shows a skewed distribution, with a peak towards lower population density values. This indicates that many states have lower population densities, while fewer states have very high population densities.")

```

###Democrats

```{r}

("The density plot for the number of Democrats shows a distribution that is skewed towards lower values, with a long tail extending towards higher values. This suggests that a majority of states have fewer Democrats, while a few states have relatively higher numbers")

```

###Republics

```{r}

("Similar to Democrats, the density plot for the number of Republicans shows a distribution skewed towards lower values, with a long tail towards higher values. This implies a similar pattern of political distribution across states.")
```

###White population

```{r}

("The density plot for the number of White population shows a distribution that is likely bimodal or multimodal, with peaks at different population levels. This suggests diverse demographics across states, with some states having predominantly White populations and others more diverse.")
```

###Black population

```{r}

("Similar to the White population, the density plot for the number of Black population also shows a distribution that is likely bimodal or multimodal, indicating varying levels of Black population across states.")

```

###Turnover

```{r}

("The density plot for voter turnout shows a distribution that is typically skewed or multimodal, with peaks indicating varying levels of voter participation across states.")

```




