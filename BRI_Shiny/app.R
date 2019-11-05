
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


data7 <- read_rds("r_objects/data7.rds")
linearMod10 <- read_rds("r_objects/linearMod10.rds")
data11 <- read_rds("r_objects/data11.rds")
linearMod14 <- read_rds("r_objects/linearMod14.rds")

ui <- fluidPage(theme = shinytheme("superhero"),
                
                
    
    navbarPage(
        
        title = "China and 'One Belt, One Road'",
        
        tabPanel(
            
            title = "China as a Global Economic Superpower",
    
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
               
               plotOutput(outputId = "plot2", height = ("600px")),
               
               tags$br() 
               
               
               
               
        ),
        
        column(width = 2)
        
    ),
    
    fluidRow(
      
      column(width = 2),
      
      column(width = 8,
             
             tags$h2("Chinese FDI per Region"),
             
             tags$p("But how does Chinese FDI differ by region. In looking at the graph below we can see cumulative Chinese FDI each year per region. With this graph the regions China chose to prioritize per year are evident. In the beginning there was little difference between regions in terms of investment, mostly because China was not investing much globally at all. Over time, however, we can see that China began to prioritize investing within a few key regions."), 
             
             tags$br(), 
             
             plotOutput(outputId = "plot3", height = ("600px")),
             
             tags$br(), 
             
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
               
               tags$p("Once again, it’s useful to isolate the top recipient countries to see clearly which high GDP countries are the main focuses of Chinese investment. This is information is included in the graph below. There are some interesting outliers here as well. India has the highest GDP (PPP) of any top FDI receiving country but received less investment in comparison to other countries. It is again clear that economic performance is a clear predictive factor behind investment, but not the only predictive measure. An examination of the political preferences of these countries may explain more of the variance. "), 
               
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
               
               tags$br(),
               
               tags$p("Measuring the political preferences of regimes is always a daunting and somewhat inaccurate task. There are, however, some data sets that have tried to quantify these preferences. Human Rights Watch has compiled data on the political rights of all countries around the world. The scale exists on a 7-point scale with 1 being the “most free” and 7 being the most illiberal. Obviously, there are some inconsistencies with quantifying the liberal or illiberal nature of a country on a 7-point metric, but for the purposes of this report, this scale demonstrates the broad political preferences the target BRI countries."), 
               
               tags$p("The goal of this section of the report is to test the hypothesis China’s ‘One Belt, One Road’ project will have an illiberal effect throughout the BRI regions. China offers a non-western, non-liberal model of development through no strings attached loans. Has the enormous amount of funding invested by China shifted the overall political preferences of target countries over time? Or much like with GDP can we see any clear indications of a preference among Chinese investors for illiberal or liberal countries? The data below explores this topic. "), 
               
               tags$br(),
               
               plotOutput(outputId = "plot13", height = ("600px")), 
               
               tags$br(),
               
               tags$p("Above are two box-and-whisker plots that demonstrate the cumulative amount of Chinese foreign direct investment given to countries in each category of political rights: one through seven. The graphs represent this information in 2008 and 2019 respectively. 2008 is used instead of 2005 because in 2005 the amount of Chinese FDI was so low, may political rights categories were not yet represented in the graph. The main takeaways of the graphic above is the lack of any clear pattern in terms of a political rights category. It is not clear that Chinese FDI has generated an overall illiberal shift, but the number of category ‘seven’ countries is much higher in 2019 than in 2008. Considering the fact that investment in category 4 and 2 countries has also improved dramatically, it would be improper to state that any direct relationship exists between Chinese FDI and an illiberal shift. A multi-variant linear regression is preformed below to more rigorously test this visualization. "), 
              
               tags$br(), 
               
               verbatimTextOutput(outputId = "plot14"),
                
               tags$br(), 
               
               tags$p("The regression above essentially demonstrates the same information from the box-and-whisker plots above. While a positive relationship between GDP and FDI is still apparent, only the political rights category of 1 has a statistically significant negative relationship and even then, it’s weaker at only 0.05. No other category has a significant relationship. This adequately demonstrates that little relationship if any exists between BRI Chinese investment and the political preferences of target countries. Potentially the negative relationship between category 1 countries and FDI signifies the preference of Chinese investors to avoid the most democratic countries within the BRI regions. One thing worth exploring, however, is geographically how did preferences change over time. ")
               
               
        ),
        
        column(width = 2)
        
      ),
      
      fluidRow(
        
        column(width = 2),
        
        column(width = 8, 
               
               tags$h1("Global Shifts in Poltical Rights"), 
               
               tags$br(),
               
               tags$p("Looking at the BRI region countries and their political rights ratings independent of Chinese investment data demonstrates an interesting trend. It is not the case that many liberal countries shifted in an illiberal direction. In fact, the majority of liberal regimes in 2005 stayed at their same ratings through 2018. There is, however, a noticeable increase in category 7 countries from 2005 to 2018, but they are almost always countries that were originally category 6 that simply moved up one more tier to the highest level of illiberalism. Considering there is no relationship between Chinese investment and political preference shift, this data suggests an overall shift of illiberal regimes to become more illiberal during the current era that is caused by completely spurious variables. "), 
               
               tags$br()
               
               
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
        
        column(width = 3),
        
        column(width = 6,
               
               tags$h1("Conclusion"),
               
               tags$p("The purpose of this report was not to identify any causal impact of Chinese Direct Investment or the Belt and Road Initiative overall. That is task that requires much more precise models and more data than is realistically available at this point. The purpose was to describe in broad terms the scope of Chinese FDI and if any broad economic or political trends are visible due to this large-scale investment increase. China’s economic growth has been shocking and its growing role in international relations dictates that its activities should be better understood and analyzed. This report will hopefully serve as a clear introduction to this phenomenon. "), 
               
               tags$p("I would not say that there are clear conclusions from my report or that my goal was ever to produce conclusions to begin within. To summarize the narrative of the report though, Chinese investment has grown tremendously overtime but the regional preference for investment depends on the investment sector. In isolating Belt and Road Initiative investment sectors, a clear preference for Asia, the Middle East, Africa, and South America is visible. In examining economic and political trends in these regions, high economic performance attracts Chinese investment and that investment in turn has a positive impact on overall economic growth. In terms of political trends though, little to no relationship exists. In either model, however, the amount of variance not explained by the models used is high, which suggests, as you might expect, that such a complicated topic requires much more complex and more in-depth analysis. "), 
               
               tags$h1("Data"), 
               
               tags$p("The data sets for the project are the following:"), 
               
               tags$p("American Enterprise Foundation’s Global Investment Tracker: https://www.aei.org/china-global-investment-tracker/"), 
               
               tags$p("The World Bank’s GDP (PPP) Data: https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.CD"), 
               
               tags$p("Freedom House’s Political Rights Index: https://freedomhouse.org/report/freedom-world/freedom-world-2018"),
               
               tags$h1("About me"), 
               
               tags$p("My name is Cian Stryker and I am a graduate student at Harvard University. I am pursuing a Master’s in Russian, Eastern European, and Central Asian studies with a focus on the Digital Silk Road, or the digital aspect of the Belt and Road Initiative. I am especially interested in the digital expansion of Chinese technology and data management systems in Central Asia. This report was the final project for my ‘Data’ class at Harvard (GOV 1005). "), 
               
               tags$h1("Source Code"), 
               
               tags$p("Here is the link to my Github page for this project: https://github.com/CianStryker/BRI_Project") 
               
        ),
        
        column(width= 3)
      )
      
    )

  )

)
server <- function(input, output) {
    
  output$plot1 <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path("./Images",
                                        paste("plot1.png")))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         width = 1300,
         height = 700
         )

  }, deleteFile = FALSE)
      
    output$plot2 <- renderImage({
      
      filename2 <- normalizePath(file.path("./Images",
                                          paste("plot2.png")))
      
      list(src = filename2,
           width = 1300,
           height = 600
      )
      
    }, deleteFile = FALSE)
    
    output$plot3 <- renderImage({
      
      filename3 <- normalizePath(file.path("./Images",
                                          paste("plot3.png")))
      
      list(src = filename3,
           width = 1300,
           height = 600
      )
      
    }, deleteFile = FALSE)
    
    output$plot4 <- renderImage({
      
      filename4 <- normalizePath(file.path("./Images",
                                           paste("plot4.png")))
      
      list(src = filename4,
           width = 1300,
           height = 600
      )
      
    }, deleteFile = FALSE)
    
    output$plot5 <- renderImage({
      
      filename5 <- normalizePath(file.path("./Images",
                                           paste("plot5.png")))
      
      list(src = filename5,
           width = 1300,
           height = 600
      )
      
    }, deleteFile = FALSE) 
    
    output$plot6 <- renderImage({
      
      filename6 <- normalizePath(file.path("./Images",
                                           paste("plot6.png")))
      
      list(src = filename6,
           width = 1300,
           height = 700
      )
      
    }, deleteFile = FALSE)
    
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
    
    output$plot8 <- renderImage({
      
      filename8 <- normalizePath(file.path("./Images",
                                           paste("plot8.png")))
      
      list(src = filename8,
           width = 1300,
           height = 600
      )
      
    }, deleteFile = FALSE)
    
    output$plot9 <- renderImage({
      
      filename9 <- normalizePath(file.path("./Images",
                                           paste("plot9.png")))
      
      list(src = filename9,
           width = 1300,
           height = 600
      )
      
    }, deleteFile = FALSE)
    
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
    
    output$plot12 <- renderImage({
      
      filename12 <- normalizePath(file.path("./Images",
                                           paste("plot12.png")))
      
      list(src = filename12,
           width = 1300,
           height = 600
      )
      
    }, deleteFile = FALSE)
    
    output$plot13 <- renderImage({
      
      filename13 <- normalizePath(file.path("./Images",
                                           paste("plot13.png")))
      
      list(src = filename13,
           width = 1300,
           height = 600
      )
      
    }, deleteFile = FALSE)
    
    output$plot14 <- renderPrint(
      summary(linearMod14) 
    )
    
    output$plot15 <- renderImage({
      
      filename15 <- normalizePath(file.path("./Images",
                                           paste("plot15.png")))
      
      list(src = filename15,
           width = 900,
           height = 700
      )
      
    }, deleteFile = FALSE)
    
    output$plot16 <- renderImage({
      
      filename16 <- normalizePath(file.path("./Images",
                                           paste("plot16.png")))
      
      list(src = filename16,
           width = 900,
           height = 700
      )
      
    }, deleteFile = FALSE)
}


# Run the application 
shinyApp(ui = ui, server = server)
