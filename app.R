library(shiny)
library(estmeansd)

ui <- fluidPage(
  titlePanel("Estimating the sample mean and standard deviation"),

  HTML(paste0("This webpage applies the either the quantile estimation (QE) method or the Box-Cox (BC) method of ", tags$a(href="https://journals.sagepub.com/doi/full/10.1177/0962280219889080", "McGrath et al. (2020)"), " to estimate the sample mean and standard deviation from a study that presents one of the following sets of summary statistics:")),

  tags$ul(
    tags$li("S1: median, minimum and maximum values, and sample size"),
    tags$li("S2: median, first and third quartiles, and sample size"),
    tags$li("S3: median, minimum and maximum values, first and third quartiles, and sample size")
  ),

  HTML(paste0("These methods are also implemented in the R package ", tags$a(href="https://CRAN.R-project.org/package=estmeansd", "estmeansd"), ", which provides detailed documentation and greater flexibility for applying these methods.")),
  br(),
  br(),

  tags$i("If the summary statistics are not in the appropriate form (i.e., S1, S2, or S3), values of NA will be given for the estimated sample mean and standard deviation."),
  br(),
  br(),

  sidebarLayout(
    sidebarPanel(numericInput("min.val", "Minimum value (if available)", value = NA),
                 numericInput("q1.val", "First quartile value (if available)", value = NA),
                 numericInput("med.val", "Median value (if available)", value = NA),
                 numericInput("q3.val", "Third quartile value (if available)", value = NA),
                 numericInput("max.val", "Maximum value (if available)", value = NA),
                 numericInput("n", "Sample size", value = NA, min = 2, max=1e6),
                 radioButtons("method.name", "Method",
                              choices = c("Quantile Estimation (QE)", "Box-Cox (BC)"),
                              selected = "Quantile Estimation (QE)")),
    mainPanel(
      uiOutput(outputId = "est_mean"),
      br(),
      uiOutput(outputId = "est_sd")
    )
  ),
  hr(),
  tags$h4("Reference:"),
  print("McGrath S., Zhao X., Steele R., Thombs B.D., Benedetti A., and the DEPRESsion Screening Data (DEPRESSD) Collaboration. (2020). Estimating the sample mean and standard deviation from commonly reported quantiles in meta-analysis. Statistical Methods in Medical Research, 29(9), 2520–2537."),
  HTML(paste0(tags$a(href="https://journals.sagepub.com/doi/full/10.1177/0962280219889080", "https://journals.sagepub.com/doi/full/10.1177/0962280219889080")))
)

server <- function(input, output) {
  no.fit <- function(e) {
    return(list(est.mean = NA, est.sd = NA))
  }

  myreact <- reactive({
    set.seed(1)
    if(input$method.name == "Quantile Estimation (QE)"){
      tryCatch({
        qe.mean.sd(min.val = input$min.val, q1.val = input$q1.val, med.val = input$med.val,
                   q3.val = input$q3.val, max.val = input$max.val, n = input$n)
      }, error = no.fit, warning = no.fit)
    } else if (input$method.name == "Box-Cox (BC)"){
      tryCatch({
        bc.mean.sd(min.val = input$min.val, q1.val = input$q1.val, med.val = input$med.val,
                   q3.val = input$q3.val, max.val = input$max.val, n = input$n)
      }, error = no.fit, warning = no.fit)
    }
  })


  output$est_mean <- renderText({
    HTML(paste("<b>","Estimated sample mean:", sprintf('%.2f', round(myreact()$est.mean, 2)), "</b>"))
  })
  output$est_sd <- renderText({
    HTML(paste("<b>", "Estimated standard deviation:", sprintf('%.2f', round(myreact()$est.sd, 2)), "</b>"))
  })

}

shinyApp(ui = ui, server = server)
