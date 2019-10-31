
library(shiny)
library(readxl)
library(reshape2)
library(plyr)
library(countrycode)
library(janitor)
library(jtools)
library(kableExtra)
library(gvlma)
library(maps)
library(png)
library(cowplot)
library(shinythemes)
library(reprex)
library(ggrepel)
library(gganimate)
library(tidyverse)



ui <- fluidPage(theme = shinytheme("sandstone"),
                
                
    
    navbarPage(
        
        title = "China and 'One Belt, One Road'",
        
        tabPanel(
            
            title = "China as a Global Economic Superpower",
    
    fluidRow(
        
        column(width = 2),
        
        column(width = 8,
               
               tags$h1("China and 'One Belt, One Road'")
               
        ),
        
        column(width = 2)
        
    ),
    
    fluidRow(
        
        column(width = 2),
        
        column(width = 8,
               
               tags$h2("Chinese Investment"),
               
               tags$p("There is a consensus among experts that with the dissolution of the USSR in 1991, the world became unipolar with the United States enjoying a near total global hegemony in both political and ecnonomic terms. The explosive economic growth of China, however, from the early 2000's to now, has challenged this concept and shifted public opinion to recognize the advent of a multi-polar world. A major goal of China during this period has been domestic economic growth, but as its economy imporved drastically, so did China's investment in foreign countries. Considering the Chinese Government's economic model, the relationship between private firms and the Communist party is closure than in most other countries. Examining China's Foreign Direct Investment, therefore gives insight into the Chinese government's overall geopolitical goals. This is especially true with the announcment of the 'One Belt, One Road project' in 2014, where Xi made it clear that China was interested in recreating the Silk Road. This ambitous infastrucutral investing project is mostly targetted towards developing regions such as West Asia, the Middle East, Sub-Saharan Africa, and South America. Many experts believe that this initiative, the Belt and Road Initiative, is actually an ambitious attempt to win political influence and capture emerging markets in vital areas neglected by the U.S. with the ultimate goal of shifting the economic center of the world from New York City to Beijing. "),
               
               tags$br()
               
        ),
        
        column(width = 2)
        
    ),
  
    fluidRow(
        
        column(width = 2),
        
        column(width = 8,
               
               tags$h2("Foreign Direct Investment Broadly"),
               
               tags$br(),
               
               plotOutput(outputId = "plot1", height = ("700px")),
               
               tags$br(),
               
               tags$p("The American Enterprise Institute and Heritage Foundation has recorded almost every single Chinese Foreign Direct Investment since 2005 to now. The cummulative results of which can be seen in the world map above.  The important note here is to see that China has invested in almost every single country in the world, but a truly massive amount in the U.S. and a few other key states."),
            
               tags$p("Below, however, is the culmination of all investments China has made per year. It demonstrates the staggering increase in FDI from 2005 to now. This increase in FDI closely coorelates to China's overall GDP increase in this time frame. It is clear that as China' economy became stronger and stronger, it chose to invest more and more globally."), 
               
               tags$br(), 
               
               plotOutput(outputId = "plot2", height = ("600px")) 
               
               
        ),
        
        column(width = 2)
        
    ),
    
    fluidRow(
      
      column(width = 2),
      
      column(width = 8,
             
             tags$h2("Chinese FDI per Region"),
             
             tags$p("But how does Chinese FDI differ by region. In looking at the graph below we can see cummulative Chinese FDI each year per region. With it,  the regions China chose to prioritize per year are evident. In the beginning there was little difference between regions in terms of investment, mostly due to the fact that China was not investing much globally at all. Over time, however, we can see that China began to prioritize investing within a few key regions."),
        
             plotOutput(outputId = "plot3", height = ("600px")),
             
             tags$p("We see  that in general the bulk of Chinese Direct Foreign Investment went to Europe, North America, and specifically the US. To demonstrate the enormity of Chinese FDI in the U.S., the United States has been separated from North America. While Europe, East Asia, West Asia, and Sub-Saharan Africa recieved the most Chinese FDI, the U.S. by itself comes in fifth place, more than all investment made into all of South America for example. If the bulk of Chinese investment flows into Europe and North America though, does this really mean that China is winning political influence world wide as many experts believe? Where does the Belt and Road Initiative fit into this picture?"),
             
             tags$br()
             
      ),
      
      column(width = 2)
      
    ),
    
    fluidRow(
      
      column(width = 2),
      
      column(width = 8,
             
             tags$h2("'One Belt, One Road'"),
             
             tags$p("The Belt and Road initiative is not immediatley apparent when we view Chinese FDI in its totality. This is because the BRI is an investment and political strategy based on a few key, typically infastructural, sectors. Namely through the BRI the Chinese government wants to invest heavily in construction (i.e. real estate within our graph), energy, metals, and transportation. These are the sectors that China believes will facillitate the creation of a New Silk Road that will tie all of Africa, the Middle East, Asia, and potentiall even South America to Beijing. In investing in these areas, often with the help of attractive soft loans, China allows these developing countries to improve their infastructures, access their resources, and improve their connection to the world economy vis-a-vi China. In return, however, they assume massive amounts of debt to China and China gains almost sole access to the emerging markets it helped develop. This economic and political dependency is what is in line with the overall geopolitical fears regarding BRI. "),
             
             tags$br(), 
             
             plotOutput(outputId = "plot5", height = ("600px")), 
             
             tags$br(), 
             
             tags$p("Of note with the Graph above is that when we only look at the key infastrucutral sectors that comprise BRI's focus, it is clear that the four target regions of West Asia, the Middle East, Sub-Saharan Africa, and South America, recieve far more funding than Europe or the U.S. This also suggests that a closure examination of these four target regions would aid in understanding the One Belt, One Road project as a whole. ")
             
      ),
  
    
      
      column(width = 2)
      
    )

    
 
    ),
    tabPanel(
      
      title = "Belt and Road Initiative", 
      
      fluidRow(
        
        column(width = 2),
        
        column(width = 8,
               
               tags$h1("BRI"), 
               
               plotOutput(outputId = "plot6", height = ("600px")), 
               
               plotOutput(outputId = "plot7", height = ("700px"))
               
               
               
               
        ),
        
        column(width = 2)
        
      ),
      
      fluidRow(
        
        column(width = 2),
        
        column(width = 8,
               
               sidebarLayout(
                 sidebarPanel(
                   sliderInput("years", label = h3("Slider"), min = 2005, 
                               max = 2019, value = 1, sep = "")
                 ),
                 mainPanel(
                   plotOutput("plot8")
                 )
                 
               )
        ),
        
        column(width = 2)
        
      ),
      
      fluidRow(
        
        column(width = 2),
        
        column(width = 8,
               
               

               plotOutput(outputId = "plot9", height = ("600px"))
               
               
               
               
        ),
        
        column(width = 2)
        
      )
      
    ),
    
    tabPanel(
      
      title = "Economic Effects of BRI", 
      
      fluidRow(
        
        column(width = 2),
        
        column(width = 8,
               
               tags$h1("BRI and GDP"), 
               
               plotOutput(outputId = "plot10", height = ("600px")), 
               
               verbatimTextOutput(outputId = "plot11")
               

               
        ),
        
        column(width = 2)
        
      ),
      
      fluidRow(
        
        column(width = 2),
        
        column(width = 8,
               
               sidebarLayout(
                 sidebarPanel(
                   sliderInput("years2", label = h3("Slider"), min = 2005, 
                               max = 2018, value = 1, sep = "")
                 ),
                 mainPanel(
                   plotOutput("plot12")
                 )
               )
        
        ),
        
        column(width = 2)
        
      )
      
    ),
    
    tabPanel(
      
      title = "Political Effects of BRI", 
      
      fluidRow(
        
        column(width = 2),
        
        column(width = 8,
               
               tags$h1("BRI and Political Rights"), 
               
               plotOutput(outputId = "plot15", height = ("600px")), 
               
               verbatimTextOutput(outputId = "plot17")

               
               
               
               
        ),
        
        column(width = 2)
        
      ),
      
      fluidRow(
        
        column(width = 6, 
               
               plotOutput(outputId = "plot18", height = ("700px"))
               
               ),

        
        column(width = 6,
               

               plotOutput(outputId = "plot19", height = ("700px"))
              
               
              )
        

      )
      
    ),
    
    tabPanel(
      
      title = "About", 
      
      fluidRow(
        
        column(width = 4),
        
        column(width = 6,
               
               tags$h1("Data"), 
               
               tags$p("Something about my data here"), 
               
               tags$h1("About me"), 
               
               tags$p("Something about me here"), 
               
               tags$h1("Source Code"), 
               
               tags$p("Git hub link") 
               
        ),
        
        column(width= 4)
      )
      
    )

  )

)
server <- function(input, output) {
  
    output$plot1 <- renderPlot(plot1)
      
    output$plot2 <- renderPlot(plot2)
    
    output$plot3 <- renderPlot(plot3)
    
    output$plot5 <- renderPlot(plot5)
    
    output$plot6 <- renderPlot(plot6) 
    
    output$plot7 <- renderPlot(plot7)
    
    output$plot8 <- renderPlot({
      # Subset the gapminder data by the chosen years
      trial3 <- subset(trial2,
                       year == input$years[1])
      
      ggplot(trial3, aes(x = country2, y = FDI, size = FDI, color = region)) +
        geom_jitter()+
        labs(x= "Countries", y="Foreign Direct Investment", fill= NULL, title="BRI Investment Per Country") +
        guides(size = FALSE, color = FALSE) +
        theme(axis.text.x=element_blank()) +
        facet_wrap(~region) +
        expand_limits(y = 7e+10)
      
      
    })
    
    output$plot9 <- renderPlot(plot9)
    
    output$plot10 <- renderPlot(plot10)
    
    output$plot11 <- renderPrint(
      summary(linearMod)
    )
    
    output$plot12 <- renderPlot({
      ohno <- subset(test4,
                       year == input$years2[1])
      
      ggplot(ohno, aes(x = FDI, size = FDI, color = region, y = GDP_Increase)) +
        geom_point() +
        geom_text(data = subset(ohno, FDI > 2e+10 | GDP_Increase > 1e+12), aes(label = country2, hjust = 1, vjust = -2)) +
        labs(x= "BRI Chinese FDI (in Billions USD)",
             y= "GDP PPP (in Billions USD)",
             fill=NULL,
             title="GDP and FDI") +
        guides(size = FALSE, color = FALSE) +
        facet_wrap(~region) +
        expand_limits(y = 6e+12, x = 7e+10)
      
    })
    
    
    output$plot15 <- renderPlot(plot15)
  
    
    
    output$plot17 <- renderPrint(
      summary(linearMod3) 
    )
    
    output$plot18 <- renderPlot(plot18)
    
    output$plot19 <- renderPlot(plot19)
}


# Run the application 
shinyApp(ui = ui, server = server)

