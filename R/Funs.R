#' Normal vs the Student
#'
#' Plot both the Normal density and the Student density using ggplot. The plot also shows shaded areas under each density corresponding to a user-specified interval.
#'
#' @param mean mean of the Normal distribution
#' @param sd Standard Deviation of the Normal distribution
#' @param df Degree of freedom of the Student's t distribution
#' @param a lower bound of the interval of the shaded area
#' @param b upper bound of the interval of the shaded area
#' @param output which density to plot? "norm", "t", or "overlay" (both). This latter is the default.
#'
#' @return ggplot object
#'
#' @export
#'
#' @examples
#' PlotDens()
#'
#' @import stats
#' @import ggplot2
#'

PlotDens <- function(a = -1, b = 1, mean=0, sd=1, df=1, output="overlay") {
  theme_set(theme_minimal())

  if (output=="norm"){
    p<- ggplot() + xlim(-5,5)+
      stat_function(fun=dnorm, col="red", args = list(mean = mean, sd = sd))+
      stat_function(fun = dnorm, xlim = c(a,b), geom = "area", fill="red", alpha=0.2, args = list(mean = mean, sd = sd))+
      labs(y="density", x=" ")+

      geom_segment(aes(x=a, xend=a, y=0, yend= dnorm(a, mean=mean, sd=sd)), linetype=2)+
      geom_segment(aes(x=b, xend=b, y=0, yend=dnorm(b, mean=mean, sd=sd)), linetype=2)+

      annotate("text", x=a, y=0, label="a", vjust=1, fontface="bold")+
      annotate("text", x=b, y=0, label ='b', vjust=1, fontface="bold")
  }

  if(output=="t"){

    p <- ggplot() + xlim(-5,5)+
      stat_function(fun = dt, col="blue", args = list(df=df))+
      stat_function(fun = dt, xlim = c(a,b), geom = "area", fill="blue", alpha=0.2, args = list(df=df))+

      labs(y="density", x=" ")+

      geom_segment(aes(x=a, xend=a, y=0, yend= dt(a, df=df)), linetype=2)+
      geom_segment(aes(x=b, xend=b, y=0, yend=dt(b, df=df)), linetype=2)+

      annotate("text", x=a, y=0, label="a", vjust=1, fontface="bold")+
      annotate("text", x=b, y=0, label ='b', vjust=1, fontface="bold")
  }

  if(output=="overlay"){

    p<- ggplot() + xlim(-5,5)+
      stat_function(fun=dnorm, col="red", args = list(mean = mean, sd = sd))+
      stat_function(fun = dnorm, xlim = c(a,b), geom = "area", fill="red", alpha=0.2, args = list(mean = mean, sd = sd))+
      labs(y="density", x=" ")+
      geom_segment(aes(x=a, xend=a, y=0, yend= max(dnorm(a, mean=mean, sd=sd), dt(a, df=df))), linetype=2)+
      geom_segment(aes(x=b, xend=b, y=0, yend=max(dnorm(b, mean=mean, sd=sd), dt(b, df=df))), linetype=2)+
      annotate("text", x=a, y=0, label="a", vjust=1, fontface="bold")+
      annotate("text", x=b, y=0, label ='b', vjust=1, fontface="bold")+

      stat_function(fun = dt, col="blue", args = list(df=df))+
      stat_function(fun = dt, xlim = c(a,b), geom = "area", fill="blue", alpha=0.2, args = list(df=df))
  }
  return(p)
}

#' Normal vs the Student
#'
#' Plot both the Normal density and the Student density **interactively** using shiny.
#'
#' @return shiny app
#'
#' @export
#'
#' @examples
#' # runPlotDens()
#'
#' @import shiny

runPlotDens <- function() {
  ui <- fluidPage(
    titlePanel("Normal vs Student"),
    sidebarLayout(
      sidebarPanel(width = 3,
                   sliderInput(
                     inputId="mean",
                     label="Mean - Normal",
                     min=-5, max=5, value=0, step=0.5
                   ),
                   sliderInput(
                     inputId = "sd",
                     label="Standard Deviation ('sd') - Normal",
                     min = 0.1, max=3, value = 1, step = 0.1
                   ),
                   br(),
                   sliderInput(
                     inputId = "df",
                     label = "Degree of freedom ('df) - Student",
                     min=1, max=50, value = 1, step=1
                   ),
                   sliderInput(
                     inputId = "range",
                     label = "Interval (a,b):",
                     min = -5, max = 5, value = c(-1, 1), step = 0.5
                   ),
                   radioButtons(
                     inputId = "out",
                     label = "Output",
                     choices = list("Overlay"="overlay", "Normal"="norm", "Student"="t"),
                     selected = "overlay"
                   )
      ),
      mainPanel(width=9,
                plotOutput("plot")
      )
    )
  )
  server <- function(input, output) {
    output$plot <- renderPlot({

      PlotDens(a=input$range[1], b=input$range[2], mean = input$mean, sd=input$sd, df=input$df, output=input$out)
    }, height = 700)
  }
  runApp(shinyApp(ui, server))
}



