library(shiny)
library(ggplot2)
library(Rmisc)


ui <- fluidPage(
  
  # Title
  titlePanel("Investment Scenarios"),
  
  # Widgets
  fluidRow(
    column(4,
           sliderInput(inputId = "init",
                       label = "Initial amount",
                       min = 1,
                       max = 10000,
                       pre = "$",
                       value = 1000)
    ),
    column(4,
           sliderInput(inputId = "return",
                       label = "Return Rate (in %)",
                       min = 0,
                       max = 20,
                       value = 5)
    ),
    column(4,
           sliderInput(inputId = "years",
                       label = "Years",
                       min = 1,
                       max = 50,
                       value = 10)
    ),
    hr(),
    column(4,
           sliderInput(inputId = "ancont",
                       label = "Annual Contribution",
                       min = 0,
                       max = 50000,
                       pre = "$",
                       value = 2000)
    ),
    column(4,
           sliderInput(inputId = "growr",
                       label = "Growth Rate (in %)",
                       min = 0,
                       max = 20,
                       value = 2)
    ),
    column(4,
           selectInput(inputId = "facetyn",
                       label = "Facet?",
                       choices = list("No" = 1, "Yes" = 2),
                       selected = 1)
    ),
    hr(),
    # Plot
    column(12,
           plotOutput("distPlot")
           
    ),
    hr(),
    # Table of data
    column(12,
           h4("Balances"),
           verbatimTextOutput("summary")
           
    )
  )
)

#ENTIRE FUNCTION COPY PASTED FROM WARMUP06, THEN MODIFIED SLIGHTLY FOR FACETING, ETC...

#' @title Future Value Calculator
#' @description Calculates the future value of an investment with a given flat rate over a certain period of time.
#' @param amount The amount of money invested into the product.
#' @param rate The rate of return, annually.
#' @param years The time in years.
#' @return The amount of money at the end of the time period.

future_value <- function(amount = 0, rate = 0, years = 0) {
  money <- amount*(1 + rate) ^ years
  return(money)
}

future_value(100, 0.05, 1)
future_value(500, 0.05, 5)
future_value(1000, 0.05, 10)

#' @title Annuity Calculator
#' @description Calculates the money returned from continually investing a certain amount every year.
#' @param contrib The amount of money invested into the product, per year.
#' @param rate The rate of return, annually.
#' @param years The time in years.
#' @return The amount of money at the end of the time period.

annuity <- function(contrib = 0, rate = 0, years = 0) {
  money <- contrib*(((1 + rate)^years-1)/rate)
  return(money) 
}

annuity(200, 0.05,1)
annuity(200, 0.05,5)
annuity(200, 0.05,10)


#' @title Growing Annuity Calculator
#' @description Calculates the money returned from continually investing an increasing amount every year.
#' @param contrib The amount of money invested into the product, per year.
#' @param rate The rate of return, annually.
#' @param growth The rate of growth for the amount invested.
#' @param years The time in years.
#' @return The amount of money at the end of the time period.

growing_annuity <- function(contrib = 0, rate = 0, growth = 0, years = 0) {
  money <- contrib*(((1 + rate)^years-(1 + growth)^years)/(rate - growth))
  return(money) 
}


#' @title Investment Graph Data Creator
#' @description Creates the data for the investment graph that will be used to plot the timelines of investing
#' @param facetka the only unorthodox input, if set to 1, then no faceting else it will give a faceted datatable
#' @return The proper data table to graph
datamorpher <- function(init = 1000, rrate = 5, years = 10, acont = 2000, growrate = 2, facetka = 1) {
  i = 0
  year <- vector(length = years + 1)
  no_contrib <- vector(length = years + 1)
  fixed_contrib <- vector(length = years + 1)
  growing_contrib <- vector(length = years + 1)
  while(i < years + 1) {
    year[i+1] <- i
    no_contrib[i+1] <- future_value(init,rrate/100,i)
    fixed_contrib[i+1] <- future_value(init,rrate/100,i) + future_value(annuity(acont, rrate/100, i))
    growing_contrib[i+1] <- future_value(init,rrate/100,i) + future_value(growing_annuity(acont, rrate/100, growrate/100, i))
    i <- i + 1
  }
  if (facetka == 1) {
    modalities <- data.frame(year,no_contrib,fixed_contrib,growing_contrib)
  } else {
    modalities <- data.frame(c(year,year,year),c(no_contrib,fixed_contrib,growing_contrib),factor(c(rep("no_contrib",times = length(year)),rep("fixed_contrib",times = length(year)),rep("growing_contrib",times =length(year))),levels = c("no_contrib","fixed_contrib","growing_contrib")))
    return(modalities)
  }
}

#' @title Investment Graph Generator
#' @description Takes a correct data table and creates a graph
#' @param datafr The dataframe to build a graph out of
#' @param years The number of years
#' @param facetka 1 means make a non-faceted graph, and 2 means make a faceted graph.
#' @return a ggplot object that will be displayed

plotgraph <- function(datafr = NA, years = 10, facetka = 1) {
  if (facetka == 1) {
    moneygraph <- ggplot(datafr) + geom_point(aes(x = year, y = no_contrib, color = "No Contribution"),size = 2.5) + geom_line(aes(x = year, y = no_contrib, color = "No Contribution"),size = 1.2) + geom_point(aes(x = year, y = fixed_contrib, color = "Fixed Contribution"),size=2.5) + geom_line(aes(x = year, y = fixed_contrib, color = "Fixed Contribution"),size=1.2) + geom_point(aes(x = year, y = growing_contrib, color = "Growing Contribution"),size=2.5) + geom_line(aes(x = year, y = growing_contrib, color = "Growing Contribution"),size=1.2) + labs(color = "Modality:", x = "Years", y = "Balance (in dollars)", title = "Balance over 10 years with Different Savings-Investing Modalities")  + theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = 0:years)
  } else {
    variable <- datafr[,3]
    moneygraph <- ggplot(data=datafr,aes(x = datafr[,1],y = datafr[,2],color = variable,fill=variable)) + geom_line(size=1) + geom_point(size=2) + geom_area(alpha = 0.5) + labs(x = "Years", y = "Balance (in dollars)", title = "Balance over 10 years with Different Savings-Investing Modalities") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + facet_wrap(.~datafr[,3])
  }
  return(moneygraph)
}



server <- function(input, output) {
  #the plot to be graphed
  output$distPlot <- renderPlot({
    plotgraph(datamorpher(input$init, input$return, input$years, input$ancont, input$growr, input$facetyn),input$years, input$facetyn)
  })
  #the data table
  output$summary <- renderPrint({
    dataset <- datamorpher(input$init, input$return, input$years, input$ancont, input$growr, 1)
    print(dataset)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

