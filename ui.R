library("shiny")
library("ggplot2")
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Visualizing Central Limit Theorem in action"),
  
  sidebarLayout(position='left',
    sidebarPanel(width=3,
      
      
      radioButtons("distribution", "Choose the distribution of the hypothetical variable in a hypothetical population", c("Normal","Exponential", "Poisson", "Uniform", 
                                                                                             "Negative Binomial", "Beta"),
                  selected = "Exponential"

      ),
      
      sliderInput("populationSize",
                  "Population Size",
                  min = 100,
                  max = 10000,
                  value = 500, step = 100),
      
      
      sliderInput("param", "Rate",
                    min = 1, max=100, value=1, step=1),
      
      
      # Only show this panel if the distribution selected is uniform
      conditionalPanel(
        condition = "input.distribution == 'Uniform' || input.distribution=='Negative Binomial' || input.distribution=='Beta' || input.distribution=='Normal'",
        sliderInput(
          "param2", "Maximum value of uniform distribution", value = 20,
          min = 1, max = 100, step=1)
        ),
        
      sliderInput("numSamples",
                  "Number of samples to be drawn from population",
                  min = 5,
                  max = 10000,
                  value =5, step = 10),

      sliderInput("sampleSize",
                  "Sample Size (size of one sample)",
                  min = 5,
                  max = 10000,
                  value =5, step = 10)

    
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      h4(textOutput("CLT")),
      splitLayout(plotOutput("distPlot", 
                             width = 600, height = 400), 
                  plotOutput("distPlot2", 
                             width=600, height=400)),
      plotOutput("distPlot3"),
      h5(textOutput("desc1")),
      h5(textOutput("desc2")),
      h5(textOutput("desc3"))
      
                           
                  )
               
                  
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      # textOutput("PLACEHOLDER"),
      # 
      # plotOutput("distPlot"),
      # plotOutput("diff")
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
    )
  )
)
