library(shiny)
library(estmeansd)

ui <- fluidPage(
  titlePanel("Estimating the sample mean and standard deviation"),

  HTML(paste0("This webpage applies the quantile estimation (QE) method and the Box-Cox (BC) method of ", tags$a(href="https://journals.sagepub.com/doi/full/10.1177/0962280219889080", "McGrath et al. (2020)"), " as well as the Method for Unknown Non-Normal Distributions (MLN) approach of ", tags$a(href="https://journals.sagepub.com/doi/full/10.1177/09622802211047348", "Cai et al. (2021)"), " to estimate the sample mean and standard deviation from a study that presents one of the following sets of summary statistics:")),

  tags$ul(
    tags$li("S1: median, minimum and maximum values, and sample size"),
    tags$li("S2: median, first and third quartiles, and sample size"),
    tags$li("S3: median, minimum and maximum values, first and third quartiles, and sample size")
  ),
  HTML(paste0("The standard error (SE) of the mean is estimated by the parametric bootstrap approach of ", tags$a(href="https://arxiv.org/abs/2206.14386", "McGrath et al. (2022)"), ".")),
  br(), br(), 
  
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
                 numericInput("nboot", "Number of bootstrap samples (for estimating the SE)", value = 1e3, min = 2, max=1e5),
                 radioButtons("method.name", "Method",
                              choices = c("Quantile Estimation (QE)", "Box-Cox (BC)", "Method for Unknown Non-Normal Distributions (MLN)"),
                              selected = "Quantile Estimation (QE)")),
    mainPanel(
      uiOutput(outputId = "est_mean"),
      br(),
      uiOutput(outputId = "est_sd"), 
      br(),
      uiOutput(outputId = "est_se")
    )
  ),
  hr(),
  tags$h4("References:"),
  print("McGrath S., Zhao X., Steele R., Thombs B.D., Benedetti A., and the DEPRESsion Screening Data (DEPRESSD) Collaboration. (2020). Estimating the sample mean and standard deviation from commonly reported quantiles in meta-analysis. Statistical Methods in Medical Research, 29(9), 2520–2537."),
  HTML(paste0(tags$a(href="https://journals.sagepub.com/doi/full/10.1177/0962280219889080", "https://journals.sagepub.com/doi/full/10.1177/0962280219889080"))), 
  br(), br(),
  print("Cai S., Zhou J., and Pan J. (2021). Estimating the sample mean and standard deviation from order statistics and sample size in meta-analysis. Statistical Methods in Medical Research, 30(12), 2701-2719."),
  HTML(paste0(tags$a(href="https://journals.sagepub.com/doi/full/10.1177/09622802211047348", "https://journals.sagepub.com/doi/full/10.1177/09622802211047348"))), 
  br(), br(),
  print("McGrath S., Katzenschlager S., Zimmer A.J., Seitel A., Steele R., and Benedetti A. (2022). Standard error estimation in meta-analysis of studies reporting medians. arXiv e-prints."), 
  HTML(paste0(tags$a(href="https://arxiv.org/abs/2206.14386", "https://arxiv.org/abs/2206.14386"))), 
)

server <- function(input, output) {
  no.fit <- function(e) {
    return(list(est.mean = NA, est.sd = NA))
  }

  myreact <- reactive({
    set.seed(1)
    if(input$method.name == "Quantile Estimation (QE)"){
      est <- tryCatch({
        qe.mean.sd(min.val = input$min.val, q1.val = input$q1.val, med.val = input$med.val,
                   q3.val = input$q3.val, max.val = input$max.val, n = input$n)
        }, error = no.fit, warning = no.fit)
    } else if (input$method.name == "Box-Cox (BC)"){
      est <- tryCatch({
        bc.mean.sd(min.val = input$min.val, q1.val = input$q1.val, med.val = input$med.val,
                   q3.val = input$q3.val, max.val = input$max.val, n = input$n)
        }, error = no.fit, warning = no.fit)
    } else if (input$method.name == "Method for Unknown Non-Normal Distributions (MLN)"){
      est <- tryCatch({
        mln.mean.sd(min.val = input$min.val, q1.val = input$q1.val, med.val = input$med.val,
                    q3.val = input$q3.val, max.val = input$max.val, n = input$n)
        }, error = no.fit, warning = no.fit)
    }
    if (class(est) %in% c('qe.mean.sd', 'bc.mean.sd', 'mln.mean.sd')){
      SE <- get_SE(est, nboot = input$nboot)$est.se
    } else {
      SE <- NA
    }
    return(list(est.mean = est$est.mean, est.sd = est$est.sd, est.se = SE))
  })


  output$est_mean <- renderText({
    HTML(paste("<b>","Estimated sample mean:", sprintf('%.2f', round(myreact()$est.mean, 2)), "</b>"))
  })
  output$est_sd <- renderText({
    HTML(paste("<b>", "Estimated standard deviation:", sprintf('%.2f', round(myreact()$est.sd, 2)), "</b>"))
  })
  output$est_se <- renderText({
    HTML(paste("<b>", "Estimated standard error of the mean:", sprintf('%.2f', round(myreact()$est.se, 2)), "</b>"))
  })

}

shinyApp(ui = ui, server = server)
