library(shiny)
library(moments)
#change to test...
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {


  
  observe({
    N <- input$populationSize
    #val <- ifelse(N < 1000, N, 1000)
    # Control the value, min, max, and step.
    updateSliderInput(session, "sampleSize", value = 5,
                      min = 5, max = round(N*.75,0), step = 5)
  })
  
  
  observe({
    a <- input$param
    # Control the value, min, max, and step.
    if (a > input$param2 & input$distribution=="Uniform"){
    updateSliderInput(session, "param2", value = a+1,
                      min = 1, max = 100, step = 1)
    }
  })
  
  
  observe({
    dist <- input$distribution
    if  (dist =="Exponential"){
      updateSliderInput(session, "param", "Rate of Exponential Distribution", value = 1,
                      min = 1, max = 20, step=1)
    }
    if (dist=="Poisson"){
      updateSliderInput(session, "param", "Mean/Variance of Poisson Distribution", value = 1,
                        min = 1, max = 10, step=1)
      
    }
    
    if (dist=="Uniform"){
      updateSliderInput(session, "param", "Minimum value of uniform distribution", value = 1,
                        min = 1, max = 100, step=1)
      
    }
    
    if (dist=="Negative Binomial"){
      updateSliderInput(session, "param", "Mean", value = 3,
                        min = 1, max = 10, step=1)
      
    }
    
    if (dist=="Beta"){
      updateSliderInput(session, "param", "Non-negative parameter1 for Beta Distribution", value = 5,
                        min = 0, max = 20, step=0.5)
      
    }
    
    
    
    if (dist=="Normal"){
      updateSliderInput(session, "param", "Mean", value = 0,
                        min = 0, max = 500, step=1)
      
    }
    
    if (dist=="Uniform"){
      updateSliderInput(session, "param2", "Maximum Value of uniform distribution", value = 110,
                        min = 100, max = 200, step=1)
      
    }
    
    
    
    if (dist=="Negative Binomial"){
      updateSliderInput(session, "param2", "Size", value = 2,
                        min = 1, max = 100, step=1)

    }
    
    if (dist=="Beta"){
      updateSliderInput(session, "param2", "Non-negative parameter2 for Beta Distribution", value = 1,
                        min = 0, max = 20, step=0.5)
      
    }
    
    if (dist=="Normal"){
      updateSliderInput(session, "param2", "Standard Deviation", value = 1,
                        min = 1, max = 50, step=1)
      
    }
    
    
  })
  
  
  
  

  output$selectedParameters <- renderText({
    
    s.distribution <- input$distribution
    print(paste0("Param2:" ,input$param2))

  }) 
  
  get_Population_data <- reactive({
    if (input$distribution=="Exponential")
    {
      n <- input$populationSize
      rate <- input$param
      set.seed(1805)
      vals <- rexp(n = n, rate = rate)
    }
    if (input$distribution == "Poisson"){
      n <- input$populationSize
      lambda <- input$param
      vals <- rpois(n=n, lambda = lambda)
    }
    if (input$distribution == "Uniform"){
      n <- input$populationSize
      min <- input$param
      max <- input$param2
      vals <- runif(n=n, min=min, max=max)
    }
    if (input$distribution == "Negative Binomial"){
      n <- input$populationSize
      mu <- input$param
      size <- input$param2
      vals <- rnbinom(n=n, mu = mu, size = size)
    }
    
    if (input$distribution == "Beta"){
      n <- input$populationSize
      shape1 <- input$param
      shape2 <- input$param2
      vals <- rbeta(n=n, shape1 = shape1, shape2 = shape2)
    }
    
    if (input$distribution == "Normal"){
      n <- input$populationSize
      mean <- input$param
      sd <- input$param2
      vals <- rnorm(n=n, mean = mean, sd= sd)
    }
    
    
    return(vals)
    
  })  
  
  get_sample_means <- reactive({
    
    x <- get_Population_data()
    sample.size <- input$sampleSize
    num.samples <- input$numSamples
    
    sample.means <- as.numeric()
    
    for (i in 1:num.samples){
      sample.means[i] = mean(sample(x, sample.size, replace = TRUE))
    }

    return(sample.means)
    
    
  })
  
  
  
  output$CLT <- renderText("The Central Limit Theorem (CLT) is one of the most fundamental and important theorems in statistics. It states that
                           if you have a population and if you take sufficiently large random samples from the population, then the distribution
                           of the sample means will approximately follow a normal distribution (the bell curve). This holds true even if the original distribution
                           of the population is not normal!")
  
  output$desc1 <- renderText("This RShiny app allows you to define a population with some specified size. Options to choose from 6 different 
                             distributions are provided. For each distribution, a population variable can be generated by specifying the 
                             distribution parameters. The distribution of this variable from population is shown in the top left histogram")
  
  output$desc2 <- renderText("The last two sliders allows the user to specify the sample size as well as the number of samples that are to
                             be drawn from the population. The idea is to demonstrate that as user increases sample size and/or number of
                             samples, the distribution of the sample means will start approximating towards a normal distribution. The histogram
                             on the top right side depicts the distribution of a single random sample drawn from the population.")
  
  output$desc3 <- renderText("The histogram above shows the distribution of the sample means of the different random samples drawn from the
                             population. This is essentially nothing but the sampling distribution of the sample means. As the sample size
                             and/or number of samples are increased by manipulating the bottom two sliders, the CLT can be visualized by seeing
                             that the sampling distribution gets closer to resembling a normal distribution. This is evident even if
                             the original distribution of the variable in population is not normal. This is probably one of the most fundamental
                             theorems in statistics that has practical use in literally all sorts of inferential statistics. CLT enables us 
                             to test for hypothesis, evaluate statistical significances and generate confidence intervals. For example, in randomized
                             clinical trials, the different arms in the trial are nothing but random samples from population. Whether or not a drug
                             works better as opposed to another one is measured by a some statistic (eg:- mean change in biomarker levels, \
                             relative risks, incidence proportions, odds-ratios, etc.) and this statistic (not the individual patient values) can
                             be assumed to follow a normal distribution thereby allowing us to perform hypothesis testing. In a way, CLT is probably one
                             of the most applied and impactful theorem in the whole of statistics.")
  
  
  
  
  
  
  
 
  
  output$distPlot <- renderPlot({
    
    data <- data.frame(get_Population_data())
    colnames(data) <- "popVals"
    p <- (ggplot(data=data, aes(popVals)) + 
            geom_histogram(aes(y =..density..), col="black", fill="seagreen3") + 
            geom_density(col="maroon", lwd=1))
    
    vowel <- ifelse(input$distribution %in% c("Exponential, Uniform"), as.character("an"), as.character("a"))
    q <- p + ggtitle(label = "Distribution in the hypothetical population ",
                     subtitle = paste0("This variable of the population is generated from ", 
                                       vowel, " ", tolower(input$distribution), " distribution","\n", "Shows the distribution of the ",
                                       input$populationSize, " values chosen randomly from the aforementioned distribution")) +
      xlab("Values within the population") + ylab("Density") + theme(axis.text.y = element_text(face="bold", color="#000000",
                                                                                                          size=10, angle=0),
                                                                               axis.text.x = element_text(face="bold", color="#000000",
                                                                                                          size=10, angle=0),
                                                                               axis.title.x = element_text(size = 10, face="bold"),
                                                                               axis.title.y = element_text(size = 10, face="bold"),
                                                                               plot.title = element_text(face="bold", size = 15, hjust = 0.5),
                                                                               plot.subtitle = element_text(face="bold", size = 12),
                                                                               panel.background = element_blank())
    
    
    q
    
  })
  
  

  
  output$distPlot2 <- renderPlot({
    
    popvals <- get_Population_data()
    data <- data.frame(sample(popvals, input$sampleSize, replace = TRUE))
    colnames(data) <- "sampleVals"
    p <- (ggplot(data=data, aes(sampleVals)) + 
            geom_histogram(aes(y =..density..), col="black", fill="turquoise4") + 
            geom_density(col="maroon", lwd=1))
    
    vowel <- ifelse(input$distribution %in% c("Exponential, Uniform"), as.character("an"), as.character("a"))
    q <- p + ggtitle(label = "Distribution of the variable in a single random sample from the population",
                     subtitle = paste0("This sample is drawn from a hypothetical population whose original distribution follows ", 
                                       vowel, " ", tolower(input$distribution), " distribution","\n", "Note that this sample is a single random",
                                       "sample among the ", input$numSamples, " drawn from the hypothetical population")) +
      xlab("Values within the single random sample") + ylab("Density") + theme(axis.text.y = element_text(face="bold", color="#000000",
                                                                                size=10, angle=0),
                                                     axis.text.x = element_text(face="bold", color="#000000",
                                                                                size=10, angle=0),
                                                     axis.title.x = element_text(size = 10, face="bold"),
                                                     axis.title.y = element_text(size = 10, face="bold"),
                                                     plot.title = element_text(face="bold", size = 15, hjust = 0.5),
                                                     plot.subtitle = element_text(face="bold", size = 12),
                                                     panel.background = element_blank())
    
    
    q
    
  })
  
  
  output$distPlot3 <- renderPlot({
    
    data <- data.frame(get_sample_means())
    colnames(data) <- "sampleMean"
    p <- (ggplot(data=data, aes(sampleMean)) + 
            geom_histogram(aes(y =..density..), col="black", fill="dodgerblue4") + 
            geom_density(col="maroon", lwd=1))
    
    vowel <- ifelse(input$distribution %in% c("Exponential, Uniform"), "an", "a")
    q <- p + ggtitle(label = "Sampling distribution of the sample means",
                     subtitle = paste0("The samples are drawn from a hypothetical population whose original distribution follows ", 
                                       vowel, " ", tolower(input$distribution), " distribution", "\n", "This distribution will approximate towards a normal distribution as sample size and number of samples are increased. This graph depicts the CLT in action!")) +
                  xlab("Sample Means") + ylab("Density") + theme(axis.text.y = element_text(face="bold", color="#000000",
                                                                                            size=12, angle=0),
                                                                 axis.text.x = element_text(face="bold", color="#000000",
                                                                                            size=12, angle=0),
                                                                 axis.title.x = element_text(size = 14, face="bold"),
                                                                 axis.title.y = element_text(size = 14, face="bold"),
                                                                 plot.title = element_text(face="bold", size = 20, hjust = 0.5),
                                                                 plot.subtitle = element_text(face="bold", size = 15),
                                                                 panel.background = element_blank())
    

    q
    
  })
  
  
  

  
})