
library(shiny)
library(maps)
library(readr)
library(shinyanimate)
library(png)
library(cowplot)
library(shinythemes)
library(reprex)
library(ggrepel)
library(gganimate)
library(tidyverse)

plot1 <- read_rds("r_objects/plot1.rds")
plot2 <- read_rds("r_objects/plot2.rds")
plot3 <- read_rds("r_objects/plot3.rds")
plot4 <- read_rds("r_objects/plot4.rds")
plot5 <- read_rds("r_objects/plot5.rds")
plot6 <- read_rds("r_objects/plot6.rds")
data7 <- read_rds("r_objects/data7.rds")
plot8 <- read_rds("r_objects/plot8.rds")
plot9 <- read_rds("r_objects/plot9.rds")
linearMod10 <- read_rds("r_objects/linearMod10.rds")
data11 <- read_rds("r_objects/data11.rds")
plot12 <- read_rds("r_objects/plot12.rds")
plot13 <- read_rds("r_objects/plot13.rds")
linearMod14 <- read_rds("r_objects/linearMod14.rds")
plot15 <- read_rds("r_objects/plot15.rds")
plot16 <- read_rds("r_objects/plot16.rds")

ui <- fluidPage(theme = shinytheme("superhero"),
                
                
    
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

               tags$p("There is a consensus among experts that with the dissolution of the USSR in 1991, the world became unipolar with the United States enjoying a near total global hegemony in both political and economic terms. The explosive economic growth of China, however, from the early 2000's to now, has challenged this concept and shifted public opinion to recognize the advent of a multi-polar world. A major goal of China during this period has been domestic economic growth, but as its economy improved drastically, so did China's investment in foreign countries. Considering the Chinese Government's economic model, the relationship between private firms and the Communist party is closure than in most other countries. Examining China's Foreign Direct Investment, therefore gives insight into the Chinese government's overall geopolitical goals. This is especially true with the announcement of the 'One Belt, One Road project' in 2014, where Xi made it clear that China was interested in recreating the Silk Road. This ambitious infrastructural investing project is mostly targeted towards developing regions such as West Asia, the Middle East, Sub-Saharan Africa, and South America. Many experts believe that this initiative, the Belt and Road Initiative, is actually an ambitious attempt to win political influence and capture emerging markets in vital areas neglected by the U.S. with the ultimate goal of shifting the economic center of the world from New York City to Beijing."),
               
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
               
               tags$p("The American Enterprise Institute and Heritage Foundation has recorded almost every single Chinese Foreign Direct Investment since 2005 to now. The cumulative results of which can be seen in the world map above.  The important note here is to see that China has invested in almost every single country in the world, but a truly massive amount in the U.S. and a few other key states."), 
               
               tags$p("Below, however, is the culmination of all investments China has made per year. It demonstrates the staggering increase in FDI from 2005 to now. This increase in FDI closely correlates to China's overall GDP increase in this time frame. It is clear, that as China' economy became stronger and stronger, it chose to invest more and more globally."), 
               
               tags$br(), 
               
               plotOutput(outputId = "plot2", height = ("600px")) 
               
               
        ),
        
        column(width = 2)
        
    ),
    
    fluidRow(
      
      column(width = 2),
      
      column(width = 8,
             
             tags$h2("Chinese FDI per Region"),
             
             tags$p("But how does Chinese FDI differ by region. In looking at the graph below we can see cumulative Chinese FDI each year per region. With this graph the regions China chose to prioritize per year are evident. In the beginning there was little difference between regions in terms of investment, mostly because China was not investing much globally at all. Over time, however, we can see that China began to prioritize investing within a few key regions."), 
             
             plotOutput(outputId = "plot3", height = ("600px")),
             
             tags$p("We see that in general the bulk of Chinese Direct Foreign Investment went to Europe, North America, and specifically the US. To demonstrate the enormity of Chinese FDI in the U.S., the United States has been separated from North America. While Europe, East Asia, West Asia, and Sub-Saharan Africa received the most Chinese FDI, the U.S. by itself comes in fifth place, more than all investment made into all of South America for example. If the bulk of Chinese investment flows into Europe and North America though, does this really mean that China is winning political influence worldwide as many experts believe? Where does the Belt and Road Initiative fit into this picture?"), 
             
             tags$br()
             
      ),
      
      column(width = 2)
      
    ),
    
    fluidRow(
      
      column(width = 2),
      
      column(width = 8,
             
             tags$h2("'One Belt, One Road'"),
             
             tags$p("The Belt and Road initiative is not immediately apparent when we view Chinese FDI in its totality. This is because the BRI is an investment and political strategy based on a few key, typically infrastructural, sectors. Namely through the BRI the Chinese government wants to invest heavily in construction (i.e. real estate within our graph), energy, metals, and transportation. These are the sectors that China believes will facilitate the creation of a New Silk Road that will tie all of Africa, the Middle East, Asia, and potential even South America to Beijing. In investing in these areas, often with the help of attractive soft loans, China allows these developing countries to improve their infrastructures, access their resources, and improve their connection to the world economy vis-a-vi China. In return, however, they assume massive amounts of debt to China and China gains almost sole access to the emerging markets it helped develop. This economic and political dependency is what is in line with the overall geopolitical fears regarding BRI."), 
             
             tags$br(), 
             
             plotOutput(outputId = "plot4", height = ("600px")), 
             
             tags$br(), 
             
             tags$p("Of note with the Graph above is that when we only look at the key infrastructural sectors that comprise BRI's focus, the four target regions of West Asia, the Middle East, Sub-Saharan Africa, and South America, receive far more funding than Europe or the U.S. This also suggests that a closure examination of these four target regions would aid in understanding the One Belt, One Road project as a whole. ")
             
      ),
  
    
      
      column(width = 2)
      
    )

    
 
    ),
    tabPanel(
      
      title = "Belt and Road Initiative Broadly", 
      
      fluidRow(
        
        column(width = 2),
        
        column(width = 8,
               
               tags$h1("BRI"), 
               
               tags$br(), 
               
               tags$p("‘One Belt, One Road’ is mainly an infrastructural investment strategy, although it does have components outside this area of investment. For the purpose of this report, however, I focus on four major areas of investment: Energy, Metals, Real Estate, and Transport. These four key sectors comprise the focus of the Belt and Road Initiative as they are the sectors that best create physical, infrastructural connections between China and recipient countries. A major focus of BRI is to improve the physical connections between China and the target regions by investing in new roads, ports, trains, etc. Another focus is to invest in resource sectors directly, which is captured in the energy and metals sectors in the graph below. Real estate captures most of the large infrastructural projects such as new buildings, processing centers, and headquarters. In looking at the graph below, we can see that energy is the focus of BRI spending in all five target regions, typically followed by transport. Real estate and Transport then follow in importance "),
               
               tags$br(),
               
               plotOutput(outputId = "plot5", height = ("600px")), 
               
               tags$h1("BRI Geographically"), 
               
               tags$br(),
               
               tags$p("After dividing the BRI target regions into the sectors of investment, it is important to see geographically where the BRI investment is going. The map below shows where cumulative Chinese investments have gone from 2005 to 2019. It is clear that certain countries are receiving the bulk of the investment. Brazil, Russia, and Pakistan stand out the most from the other countries. Also, interesting to note is that within certain regions there are clear leaders, for example, within Central Asia Kazakhstan is the most popular investment target by China. "), 
               
               plotOutput(outputId = "plot6", height = ("700px")),
               
               tags$br()
               
              
               
               
        ),
        
        column(width = 2)
        
      ),
      
      fluidRow(
        
        column(width = 2),
        
        column(width = 8,
               
               tags$h1("Investment By Country"), 
               
               
               tags$p("To see this cumulative effect more closely, the interactive graphic below allows us to see how Chinese BRI investment has changed, per country over time. The countries are listed on the x axis in no particular order and the abbreviated country names are used for clarity. By changing the year from 2005 to 2019 we can see once again that certain countries are the real focuses of Chinese investment. "),
               
               tags$br(),
               
               sidebarLayout(
                 sidebarPanel(
                   sliderInput("years", label = h3("Slider"), min = 2005, 
                               max = 2019, value = 1, sep = "")
                 ),
                 
                 mainPanel(
                   plotOutput("plot7"),
                   
                tags$br()
                   
                 )
                 
               )
        ),
        
        column(width = 2)
        
      ),
      
      fluidRow(
        
        column(width = 2),
        
        column(width = 8,
               
               tags$h1("Top Investment Targets"), 
               
               
               tags$br(),
               
               tags$p("To simplify the information further, the graph below shows the top cumulative investment targets for China in 2019. By removing the lower investment target countries, we can see even more clearly where the main BRI investment focus is. This information suggests a few key questions. Generally speaking, what are the effects of BRI investment? Are there any clear trends or tendencies that we can see as a result of BRI? Is there anything apparent in our data that would explain why certain countries receive more funding than others? I attempt to answer these questions in the following panels by examining the economic and political effects of ‘One Belt, One Road’. "),
               

               plotOutput(outputId = "plot8", height = ("600px")),
               
               tags$br()
               
               
               
               
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
               
               tags$br(),
               
               tags$p("Measuring the real effects of the Belt and Road Initiative is a daunting task and attempts to do so have not been entirely successful. This is not an academic piece and so I am avoiding stringent and nuanced models that attempt to identify a causal variable. This is a descriptive report whose goal is largely descriptive in nature. Therefore, my model is much simpler than an academic model would be. I use GDP PPP (Purchasing Power Parity) from the World Bank to illustrate what relationship, if any, exists between Chinese BRI investment and overall economic health. I do this in two stages. The first is just a scatter plot with a linear regression line written in for a visualization of the relationship, and the second is a linear regression of the effect itself. The first is shown below with both GDP and Foreign Direct reduced to the log10 to avoid the massive difference between GDPs in our countries. "),
               
               tags$br(),
               
               plotOutput(outputId = "plot9", height = ("600px")), 
               
               tags$p("As you can see above, the relationship has a positive relationship as demonstrated by the upward slope of the regression line. You can also see, however, that many countries fall outside of our standard errors, which demonstrates that this relationship is not capturing all the variance that exists. How much is not explained in terms of GDP and FDI? That’s more explicitly explained in actual regression shown below. "),
               
               tags$p("There are a few things within the linear regression below that are of note. First would be the statistical significance of the results for GDP, which is highly significant. This demonstrates that it is highly unlikely that this relationship exists by chance. Second, the 0. between FDI and GDP is positive in nature and significant considering the log10 of 0.01161 is around 1 million USD. Finally, the adjusted R-squared value of .24 means that only about a quarter of the variance is captured by this regression. While not terrible, this does suggest that many variables exist that explain GDP growth other than BRI investment, which is an intuitive concept. That being said, there is a large portion of this variance that is explained by the GDP and FDI relationship. The directionality is not clear. It seems unlikely that Chinese FDI causes overall economic growth and far more likely that high GDP levels make countries more attractive for investment overall. "), 
               
               verbatimTextOutput(outputId = "plot10")
               

               
        ),
        
        column(width = 2)
        
      ),
      
      fluidRow(
        
        column(width = 2),
        
        column(width = 8,
               
               tags$h1("A Closer Look"), 
               
               tags$br(),
               
               tags$p("The regressions above give insight into this relationship in the large-scale. Viewing the relationship between GDP (PPP) and FDI on a smaller scale is also useful. In the interactive graph below, you can see the relationship between GDP (PPP) and FDI over time per country. Again, I am using the abbreviated country names for clarity’s sake. Of note here are certain outliers that do not fit the relationship at all, such as Japan in East Asia. Japan has the highest GDP of any country in the data, but receives relatively little Chinese investment. This would contradict the concept that GDP is the most predictive factor behind FDI, but one case does not necessarily discount the overall trend. It does suggest that there are other variables that dictate where China invests. "), 
               
               sidebarLayout(
                 sidebarPanel(
                   sliderInput("years2", label = h3("Slider"), min = 2005, 
                               max = 2018, value = 1, sep = "")
                 ),
                 mainPanel(
                   plotOutput("plot11")
                 )
               )
        
        ),
        
        column(width = 2)
        
      ),
      
      fluidRow(
        
        column(width = 2),
        
        column(width = 8,
               
               tags$br(),
               
               tags$p("Once again it’s useful to isolate the top recipient countries to see clearly which high GDP countries are the main focuses of Chinese investment. This is information is included in the graph below. "), 
               
               tags$br(),
               
               plotOutput(outputId = "plot12", height = ("600px")) 
              
               
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
               
               plotOutput(outputId = "plot13", height = ("600px")), 
               
               verbatimTextOutput(outputId = "plot14")

               
               
               
               
        ),
        
        column(width = 2)
        
      ),
      
      fluidRow(
        
        column(width = 6, 
               
               plotOutput(outputId = "plot15", height = ("700px"))
               
               ),

        
        column(width = 6,
               

               plotOutput(outputId = "plot16", height = ("700px"))
              
               
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
    
    output$plot4 <- renderPlot(plot4)
    
    output$plot5 <- renderPlot(plot5) 
    
    output$plot6 <- renderPlot(plot6)
    
    output$plot7 <- renderPlot({
      # Subset the gapminder data by the chosen years
      trial3 <- subset(data7,
                       year == input$years[1])
      
      ggplot(trial3, aes(x = country2, y = FDI, size = FDI, color = region)) +
        geom_point()+
        geom_text(data = subset(trial3, FDI >= .25e+10), aes(label = country2, hjust = .2, vjust = -2)) +
        labs(x= "Countries", y="Foreign Direct Investment", fill= NULL, title="BRI Investment Per Country") +
        guides(size = FALSE, color = FALSE) +
        theme(axis.text.x=element_blank()) +
        facet_wrap(~region) +
        expand_limits(y = 7e+10)
      
      
    })
    
    output$plot8 <- renderPlot(plot8)
    
    output$plot9 <- renderPlot(plot9)
    
    output$plot10 <- renderPrint(
      summary(linearMod10)
    )
    
    output$plot11 <- renderPlot({
      ohno <- subset(data11,
                       year == input$years2[1])
      
      ggplot(ohno, aes(x = FDI, size = FDI, color = region, y = GDP_Increase)) +
        geom_jitter() +
        geom_text(data = subset(ohno, FDI > 2e+10 | GDP_Increase > 1e+12), aes(label = country2, hjust = 1, vjust = -2)) +
        labs(x= "BRI Chinese FDI (in Billions USD)",
             y= "GDP PPP (in Billions USD)",
             fill=NULL,
             title="GDP and FDI") +
        guides(size = FALSE, color = FALSE) +
        facet_wrap(~region) +
        expand_limits(y = 6e+12, x = 7e+10)
      
    })
    
    output$plot12 <- renderPlot(plot12)
    
    output$plot13 <- renderPlot(plot13)
    
    output$plot14 <- renderPrint(
      summary(linearMod14) 
    )
    
    output$plot15 <- renderPlot(plot15)
    
    output$plot16 <- renderPlot(plot16)
}


# Run the application 
shinyApp(ui = ui, server = server)
