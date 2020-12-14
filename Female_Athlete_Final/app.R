#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(twitteR)
library(tidytext)
library(tidyverse)
library(textdata)
library(shiny)
library(tidyverse)
library(janitor)
library(shinythemes)
library(gtrendsR)
library(rgeos)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(gt)
library(gtsummary)
library(broom.mixed)
library(rstanarm)

sidata_201015 <- read_rds("Female_Athlete_Data_Final/sidata_201015.rds")
athletedata <- read_rds("Female_Athlete_Data_Final/athletedata.rds")
noc_regions <- read_rds("Female_Athlete_Data_Final/noc_regions.rds")
d2 <- read_rds("Female_Athlete_Data_Final/d2.rds")
d3 <- read_rds("Female_Athlete_Data_Final/d3.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("cosmo"),
    
    navbarPage(
        "Female Athletes, Body Image, and Societal Expectations",
        
        tabPanel("About",
                 div(img(src = "bodyimage.png", height = 300, width = 400), style="text-align: center;"),
                 h1(strong("About this Project"), align = "center"),
                 br(),
                 h4("Why?"),
                 p("Being a female athlete has always been a huge part of my identity. Sophomore year, when I 
                   joined the team behind Undergraduate Women of Harvard Athletics was when I realized my passion
                   for advocating for female athletes and the very niche issues we go through. Having grown up in 
                   China as a female rugby player, there were many bits and pieces of stigma and stereotypes that
                   I experienced, relating to femininity and my body. I wanted to pursue this project to look at
                   data on female athletes, on female body image and combine the two to tell the story of the paradox that
                   many female athletes go through having to juggle ahthletic performance and adhering to societal beauty ideals."),
                 br(),
                 h4("About the data"),
                 p("I had difficulty looking for datasets relating to female athletes and 
                 the topic of body image. I wanted to work with complex data that I could 
                 wrangle and clean, which led me to looking at Twitter data. In addition to the 
                 Twitter data, I also wanted to work with Olympic athlete data to display 
                 the increase in women's sport participation. This dataset is from the IOC Research and Reference Service 
                 and published by The Guardian's Datablog. These two were the more complex 
                 datasets that I used which I had to filter and select to look at only the parts I cared about: female athletes. The more specific datasets were related to Sports Illustrated
                 and ESPN Body Issue Covers to emphasize the involvement of media within the issue.
                 The Sports Illustrated Cover data was created by Alex Albright and her github is linked below.
                 Her dataset only included the years of 2010-2015 so I decided to add onto the
                 data by using the same method she used and counted the different SI covers for the 
                 years from 2016-2020. You can look at the original data on her github which is linked", a("HERE", href="https://github.com/apalbright/SportsIllustrated")),
                 br(),
                 h4("About Me"),
                 p("My name is Sofie Fella and I am a junior at Harvard College
                   studying Psychology with a secondary in Economics. You can reach
                   me at Sofiefella@college.harvard.edu"),
                 h5("The source code for this Shiny App can be found at my GitHub", 
                    a("HERE", href="https://github.com/Sofiefella/Female-Athlete"))
                 
        ),
        
        tabPanel("Women in Sport",
                 titlePanel("Women in Sport"),
                 
                 tabsetPanel(
                     tabPanel("Participation Growth",
                              br(),
                              sidebarPanel(
                                  h4(strong("Growth of Women in Sport")),
                                  p("Made with data from the IOC,
                       this graph shows that since 1900, more and more
                         women have been attending the Olympics.")),
                              mainPanel(plotOutput("increaseinfathletes"),
                                        br(),
                                        p("The graph above displays the Olympic 
                         years on the x-axis from 1900, which was the first
                         year that women started participating in the Olympics.
                         The x-axis displays both the summer and winter 
                         Olympics. Originally, they were combined until the IOC
                         voted to make them every two years in 1986.The dataset
                         had separated the two earlier than that year, this is 
                         the case because they just alternated what to call it, 
                         but up until 1986 it was held every four years. There
                         was a period during WWII where there were no Olympics held
                         which is why there is a gap. The y-axis shows the number
                         of female olympians that attended since 1900, and we can 
                         conclude from the graph that there has been a major increase 
                         since 1900."))
                     ), 
                     
                     
                     tabPanel("Country Growth",
                              br(),
                              sidebarPanel(
                                  h4(strong("Female Athlete Country Representation")),
                                  p("The maps show the Olympics in Paris 1900, Rome 1960,
                       Montreal 1976, Atlanta 1996, and Rio 2016. Click on the
                       buttons below to take a look at the growth of female 
                       athletes across the world over the years!"),
                                  br(),
                                  radioButtons("olympiccountries", "Click here:", 
                                               c("Paris 1900" = "1900 Summer", "Rome 1960" = "1960 Summer", 
                                                 "Montreal 1976" = "1976 Summer", "Atlanta 1996" = "1996 Summer",
                                                 "Rio 2016" = "2016 Summer"))
                              ),
                              mainPanel(plotOutput("parismap"), 
                                        p("The Paris 1900 Olympics were the first Olympics
                                 in which women were allowed to compete in, therefore
                                 having a limited amount of female athletes. The progression
                                 through the maps displays the increase in female athletes within
                                 certain more developed countries, as well as the increase in
                                 countries allowing more and more women to participate in sport
                                 and compete at the highest level: at the Olympics")
                                        
                              )),
                     
                     tabPanel("Growth in Different Sports",
                              br(),
                              sidebarPanel(
                                  h4(strong("Female Athlete Growth in Different Sports")),
                                  p("These graphs show the increase in sports available
                       to female athletes over the decade. In 1900, there
                       were a limited amount of sports that female athetes 
                       could partake in. Over the years, the number of sports
                       has increased drastically as you can see on the y-axis
                       of these graphs. In addition, the number of female 
                       athletes participating within each sport has also 
                       grown tremendously.")),
                              mainPanel(plotOutput("parissports"),
                                        br(),
                                        plotOutput("romesports"),
                                        br(),
                                        plotOutput("riosports"))
                     ))
                 
                 
        ),
        
        
        
        tabPanel("Body Image in the Media",
                 titlePanel("Body Image in the Media"),
                 
                 tabsetPanel(
                     tabPanel("Twitter Sentiments",
                              br(),
                              sidebarPanel(
                                  h4(strong("Twitter Sentiments")),
                                  p("The graphs on the right compare Twitter sentiments
                     with Tweets including 'Female Athlete' or 'Female Model'. 
                     This was the easiest way to portray the differences in 
                     attitude that many people have in today's society
                     towards what is attractive, what is feminine, and aspects they 
                     are drawn to. The findings show that even though
                     there is more positive sentiment for 'female athlete' tweets than negative, 
                     there is a greater gap between negative and positive
                     sentiment towards 'female model' than for 'female athlete.'")),
                              mainPanel(
                                  plotOutput("twitter1"),
                                  br(),
                                  p("The graph above shows the number of tweets with 'female
                           athlete' included and the varying sentiments that they portray.
                           We can see that overall there is a positive sentiment within
                           the tweets, with trust and anticipation at higher counts too."),
                                  br(),
                                  plotOutput("twitter2"),
                                  br(),
                                  p("The graph above shows the number of tweets with 'female model'
                             included and the varying sentiments that they potray. We can see
                             that overall there is a positive sentiment, with trust and negative
                             following at higher counts as well."),
                                  br(),
                                  plotOutput("twitter3"),
                                  br(),
                                  p("The graph above shows the comparison between positive and negative
                             sentiments for the tweets with 'female athlete.' Overall, there are more
                             positive tweets than negative tweets, but the difference is not huge."),
                                  br(),
                                  plotOutput("twitter4"),
                                  br(),
                                  p("The graph above shows the comparison between positive and negative
                             sentiments for the tweets with 'female model.' Overall, there are 
                             more positive tweets than negative tweets. There is also a greater
                             difference between the negative and positive tweets in this case than in
                             the 'female athlete' case.")
                              ))
                     
                 )),
        
        
        
        tabPanel("Female Athletes in Media",
                 titlePanel("Female Athletes in the Media"),
                 
                 tabsetPanel(
                     
                     tabPanel("Sports Illustrated Covers",
                              br(),
                              sidebarLayout(
                                  sidebarPanel(
                                      p("See the distribution of covers by year by clicking below:"),
                                      br(),
                                      radioButtons("radio_options",
                                                   "Year:",
                                                   choices = c(2010, 2011, 2012, 2013, 2014, 2015,
                                                               2016, 2017, 2018, 2019, 2020),
                                                   selected = 2010)
                                  ),
                                  mainPanel(
                                      plotOutput("yearlygraphs"),
                                      br(),
                                      p("By clicking through the different years, we can see that male athletes/coaches
                       dominates the Sports Illustrated Covers. Female Athletes/Coaches and Female Models 
                       don't differ too significantly. The only years where Female Athletes/Coaches seem
                       to diverge from normality are 2015 and 2019, this is because those years the 
                       US Women's National Soccer Team won the FIFA Women's World Cup and SI gave 
                       each of their players an individual cover. The below graph summarizes all the
                       individual yearly graphs:"),
                                      br(),
                                      plotOutput("summary")))),
                     
                     tabPanel("Female Athlete Google Interest",
                              br(),
                              sidebarPanel(
                                  h4(strong("Google Search Hits")),
                                  p("The findings show that over the years, people
                              consistently search 'Female Model' more often than
                              'Female Athlete' on Google. The findings also show that
                              'Female Model' is a more popular search even when separated 
                              into different sports, even sports in whicn certain female athletes
                              have been deemed as very attractive e.g. Alex Morgan the USWNT soccer player."
                                  )),
                              mainPanel(plotOutput("google"),
                                        br(),
                                        p("The graph above displays the years on the x-axis and the
                                Google search hits on the y-axis to show the interest over
                                time in the words 'Female Athlete' vs. 'Female Model.' The 
                                data from Google shows that 'Female Model' is searched more than 
                                'Female Athlete' consistently, while there were two spikes of
                                'Female Athlete' around early and late 2017."),
                                        br(),
                                        plotOutput("moregoogle"),
                                        br(),
                                        p("The graph above displays the years on the x-axis and the
                                Google search hits on the y-axis to show the interest over
                                time in the words 'Female Model' vs. sports such as soccer,
                                volleyball, rugby. The graph shows that 'Female Model' is 
                                searched more than the 3 sports consistently, with volleyball 
                                and soccer having some spikes while rugby stays with low interest.")
                                        
                              )),
                     
                     tabPanel("Relationship between Female Athlete Growth & Media",
                              br(),
                              sidebarPanel(
                                  h4(strong("Female Athlete Growth and Media Regression Model")),
                                  p("The findings are that there is no significant relationship
                                between the increase in female athlete participation in the Olympics
                                over the years and media representation of female athletes measured in the 
                                form of Sports Illustrated Covers.")),
                              mainPanel(gt_output("table"),
                                        br(),
                                        p("The above table summarizes the stan_glm for this regression. 
                                          We are trying to look at whether an increase in female
                                          athlete participation measured through the numnber of 
                                          female Olympians can predict the representation of
                                          female athletes in media, measured through the number
                                          of female athletes on Sports Illustrated covers. The intercept
                                          represents the probability of the number of female athletes on covers
                                          with no impact from the increase in female athlete participation.
                                          The number of female athletes section shows that as female athletes
                                          increase in participation, there is even a slight negative relationship.
                                          However, because we do not have enough data points, this is not a
                                          significant relationship and more data needs to be looked at to confirm
                                          this relationship. I experimented with controlling for summer vs. 
                                          winter olympics to see if that had an effect on the significance. There was 
                                          no major effect in controlling it. I also looked at the interaction between
                                          female_num and summer vs. winter but there was also no major significance."),
                                        br(),
                                        plotOutput("scatterplot"),
                                        p("The scatterplot above maps the x-axis as the Olympic years, the y-axis
                                          as the number of female athletes/coaches on the SI covers and the 
                                          size of the points on the graph represent the number of female athletes
                                          at the Olympics over the years. Because the points are not getting bigger 
                                          with each year and moving in a positive direction, we can see the
                                          lack of a positive relationship between increased female athlete numbers
                                          and media representation. Because of the lack of data points however, 
                                          this is also not the most accurate graph to look at.")
                                        
                              ))
                 )),
        
        tabPanel("Female Athletes' Bodies",
                 titlePanel("Female Athletes' Bodies"),
                 br(),
                 sidebarPanel(
                     h4(strong("Female Athlete Heights & Weights")),
                     p("These graphs show the change over time in 
                     female athlete heights & weights in specific sports: 
                     more feminine and aesthetic ones such as figure skating,
                     volleyball, gymnastics and 'less feminine' ones such as ice hockey,
                     basketball, and weightlifting. The graphs display that over time, sports have
                     stayed relatively similar within the sport but there are major differences in 
                     height and weight between type of sport.")),
                 mainPanel(
                     plotOutput("weight"),
                     br(),
                     p("The above graph shows that female olympians have not neccassarily gotten 
                       heavier or lighter over time, there are distinct differences between sports in
                       terms of weight. For example, gymnasts and figure skaters have stayed
                       consistently lighter than the other sports."),
                     br(),
                     plotOutput("height"),
                     p("The above graph shows that female olympians differ in height very catered
                   to the sport that they are competing in. For example, basketball and volleyball players
                   are distincty taller than the other sports and over time, it seems like they have
                   increased in height as well."))
        ))
    
    
    
    
    
)





# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$summary <- renderPlot({
        
        sidata_201015 %>%
            clean_names() %>%
            
            ggplot(aes(x = year, y = frequency, 
                       fill = on_cover)) +
            geom_col() +
            scale_fill_manual(values = c("darkcyan", "coral", "azure4", 
                                         "darkolivegreen",
                                         "chocolate", "darkseagreen"), 
                              guide_legend(title="Who is on the cover?")) +
            labs(title = "Comparison of Who is on Sports Illustrated Covers 
      throughout the Decade",
                 x = "Year", 
                 y = "Number of Covers") +
            theme_minimal() +
            scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 
                                          2016, 2017, 2018, 2019, 2020), labels = c(2010, 2011, 2012, 2013, 
                                                                                    2014, 2015, 2016, 2017, 2018, 2019, 2020))
        
        
    })
    
    
    
    
    output$yearlygraphs <- renderPlot({
        
        sidata_201015 %>%
            clean_names() %>%
            filter(year == input$radio_options) %>%
            
            ggplot(aes(x = year, y = frequency, 
                       fill = on_cover)) +
            geom_col(position = "dodge") +
            scale_fill_manual(values = c("darkcyan", "coral", "azure4", 
                                         "darkolivegreen",
                                         "chocolate", "darkseagreen"), 
                              guide_legend(title="Who is on the cover?")) +
            labs(title = "Comparison of Who is on Sports Illustrated Covers 
      throughout the Decade",
                 x = "Year", 
                 y = "Number of Covers") +
            theme_classic() +
            scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 
                                          2016, 2017, 2018, 2019, 2020), 
                               labels = c(2010, 2011, 2012, 2013, 
                                          2014, 2015, 2016, 2017, 2018, 2019, 2020)) +
            scale_y_continuous(limits = c(0, 125))
        
        
    })
    
    output$increaseinfathletes <- renderPlot({
        
        femaleolympicathlete <- athletedata %>%
            filter(Sex == "F")
        femaleolympicathlete %>% 
            group_by(Year) %>% 
            distinct(Name, .keep_all = TRUE)
        fa_count <- femaleolympicathlete %>% 
            group_by(Year) %>% 
            distinct(Name, .keep_all = TRUE) %>% 
            mutate(female_num = n()) %>%
            distinct(female_num, .keep_all = TRUE)
        fa_count %>%
            ggplot(aes(x = Year, y = female_num, fill = Season)) +
            geom_col() +
            labs(title = "Women's Participation in Summer + Winter Olympics from 1900-2016",
                 subtitle = "There is an increase in female athlete olympic participation",
                 y = "Number of Olympic Female Athletes",
                 x = "Olympic Years (Summer + Winter)") +
            scale_x_continuous(breaks = c(1900, 1904, 1906, 1908, 1912, 1920, 1924, 1928,
                                          1932, 1936, 1948, 1952, 1956, 1960, 1964, 1968, 1972,
                                          1976, 1980, 1984, 1988, 1992, 1994, 1996, 1998, 2000,
                                          2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016)) +
            theme(axis.text.x = element_text(angle = 270, size = 8)) +
            theme(plot.title = element_text(size = 16,face = "bold"))
        
    })
    
    output$parismap <- renderPlot({
        
        femaleolympicathlete <- athletedata %>%
            filter(Sex == "F")
        
        regions <- femaleolympicathlete %>% 
            left_join(noc_regions, by = "NOC") %>%
            filter(!is.na(region))
        
        Paris_data <- regions %>% 
            filter(Games == input$olympiccountries) %>%
            group_by(region) %>%
            distinct(Name, .keep_all = TRUE)
        
        world <- map_data("world")
        worldmap <- ne_countries(scale = 'medium', type = "map_units", returnclass = 'sf')
        
        grouped_paris_data <- Paris_data %>%
            group_by(region) %>% 
            count()
        
        ppost_world <- full_join(worldmap, grouped_paris_data, by = c("name" = "region"))
        ppost_world$n[is.na(ppost_world$n)] <- 0
        
        ggplot(data = ppost_world) + 
            geom_sf(aes(fill = n)) +
            labs(title = paste("Number of Female Athletes at the", input$olympiccountries, "Olympics")) +
            scale_fill_gradient2(low = "white", high = "blue") +
            guides(fill = guide_colourbar(title = "#")) +
            theme(plot.title = element_text(hjust = 0.5),
                  panel.background = element_rect(fill = "lightblue"))
        
    })
    
    output$romemap <- renderPlot({
        
        femaleolympicathlete <- athletedata %>%
            filter(Sex == "F")
        
        regions <- femaleolympicathlete %>% 
            left_join(noc_regions, by = "NOC") %>%
            filter(!is.na(region))
        
        rome_data <- regions %>% 
            filter(Games == "1960 Summer") %>%
            group_by(region) %>%
            distinct(Name, .keep_all = TRUE)
        
        world <- map_data("world")
        worldmap <- ne_countries(scale = 'medium', type = "map_units", returnclass = 'sf')
        
        grouped_rome_data <- rome_data %>%
            group_by(region) %>% 
            count()
        
        romepost_world <- full_join(worldmap, grouped_rome_data, by = c("name" = "region"))
        romepost_world$n[is.na(romepost_world$n)] <- 0
        
        ggplot(data = romepost_world) + 
            geom_sf(aes(fill = n)) +
            labs(title = "Number of Female Athletes at the Rome 1960 Olympics") +
            scale_fill_gradient2(low = "white", high = "blue") +
            guides(fill = guide_colourbar(title = "#")) +
            theme(plot.title = element_text(hjust = 0.5),
                  panel.background = element_rect(fill = "lightblue"))
        
    })
    
    output$montrealmap <- renderPlot({
        
        femaleolympicathlete <- athletedata %>%
            filter(Sex == "F")
        
        regions <- femaleolympicathlete %>% 
            left_join(noc_regions, by = "NOC") %>%
            filter(!is.na(region))
        
        montreal_data <- regions %>% 
            filter(Games == "1976 Summer") %>%
            group_by(region) %>%
            distinct(Name, .keep_all = TRUE)
        
        world <- map_data("world")
        worldmap <- ne_countries(scale = 'medium', type = "map_units", returnclass = 'sf')
        
        grouped_montreal_data <- montreal_data %>%
            group_by(region) %>% 
            count()
        
        mpost_world <- full_join(worldmap, grouped_montreal_data, by = c("name" = "region"))
        mpost_world$n[is.na(mpost_world$n)] <- 0
        
        ggplot(data = mpost_world) + 
            geom_sf(aes(fill = n)) +
            labs(title = "Number of Female Athletes at the Montreal 1976 Olympics") +
            scale_fill_gradient2(low = "white", high = "blue") +
            guides(fill = guide_colourbar(title = "#")) +
            theme(plot.title = element_text(hjust = 0.5),
                  panel.background = element_rect(fill = "lightblue"))
    })
    
    output$atlantamap <- renderPlot({
        
        femaleolympicathlete <- athletedata %>%
            filter(Sex == "F")
        
        regions <- femaleolympicathlete %>% 
            left_join(noc_regions, by = "NOC") %>%
            filter(!is.na(region))
        
        atlanta_data <- regions %>% 
            filter(Games == "1996 Summer") %>%
            group_by(region) %>%
            distinct(Name, .keep_all = TRUE)
        
        world <- map_data("world")
        worldmap <- ne_countries(scale = 'medium', type = "map_units", returnclass = 'sf')
        
        grouped_atlanta_data <- atlanta_data %>%
            group_by(region) %>% 
            count()
        
        apost_world <- full_join(worldmap, grouped_atlanta_data, by = c("name" = "region"))
        apost_world$n[is.na(apost_world$n)] <- 0
        
        ggplot(data = apost_world) + 
            geom_sf(aes(fill = n)) +
            labs(title = "Number of Female Athletes at the Atlanta 1996 Olympics") +
            scale_fill_gradient2(low = "white", high = "blue") +
            guides(fill = guide_colourbar(title = "#")) +
            theme(plot.title = element_text(hjust = 0.50),
                  panel.background = element_rect(fill = "lightblue"))
    })
    
    output$riomap <- renderPlot({
        
        femaleolympicathlete <- athletedata %>%
            filter(Sex == "F")
        
        regions <- femaleolympicathlete %>% 
            left_join(noc_regions, by = "NOC") %>%
            filter(!is.na(region))
        
        rio_data <- regions %>% 
            filter(Games == "2016 Summer") %>%
            group_by(region) %>%
            distinct(Name, .keep_all = TRUE)
        
        world <- map_data("world")
        worldmap <- ne_countries(scale = 'medium', type = "map_units", returnclass = 'sf')
        
        grouped_rio_data <- rio_data %>%
            group_by(region) %>% 
            count()
        
        rpost_world <- full_join(worldmap, grouped_rio_data, by = c("name" = "region"))
        rpost_world$n[is.na(rpost_world$n)] <- 0
        
        ggplot(data = rpost_world) + 
            geom_sf(aes(fill = n)) +
            labs(title = "Number of Female Athletes at the Rio 2016 Olympics") +
            scale_fill_gradient2(low = "white", high = "blue") +
            guides(fill = guide_colourbar(title = "#")) +
            theme(plot.title = element_text(hjust = 0.5),
                  panel.background = element_rect(fill = "lightblue"))
    })
    
    output$google <- renderPlot({
        
        googletrends <- gtrends(c("female athlete", "female model"))
        plot(googletrends)
        
    })
    
    output$moregoogle <- renderPlot({
        
        gtrends <- gtrends(c("female rugby", "female model",
                             "female volleyball", "female soccer"))
        plot(gtrends)
        
    })
    
    
    output$twitter1 <- renderPlot({
        
        
        nrc <- get_sentiments("nrc")
        bing <- get_sentiments("bing")
        
        twittergraph1 <- d2 %>%
            unnest_tokens(word, text) %>%
            filter(!word %in% c("@", "female", "athlete")) %>% 
            anti_join(stop_words) %>% 
            inner_join(nrc) %>%
            count(sentiment, sort = TRUE) %>% 
            ggplot(aes(x = sentiment, y = n)) + 
            geom_col(fill = "darkblue") +
            labs(title = "Sentiments represented within Tweets with 'Female Athlete'",
                 x = "Sentiments",
                 y = "Number of Tweets") 
        
        twittergraph1
        
    })
    
    
    output$twitter2 <- renderPlot({
        
        
        nrc <- get_sentiments("nrc")
        bing <- get_sentiments("bing")
        
        
        twittergraph2 <- d3 %>%
            unnest_tokens(word, text) %>%
            filter(!word %in% c("@", "female", "model")) %>% 
            anti_join(stop_words) %>% 
            inner_join(nrc) %>%
            count(sentiment, sort = TRUE) %>% 
            ggplot(aes(x = sentiment, y = n)) + 
            geom_col(fill = "darkred") + 
            labs(title = "Sentiments represented within Tweets with 'Female Model'",
                 x = "Sentiments",
                 y = "Number of Tweets") 
        twittergraph2
        
    })
    
    output$twitter3 <- renderPlot({
        
        
        nrc <- get_sentiments("nrc")
        bing <- get_sentiments("bing")
        
        twittergraph3 <- d2 %>%
            unnest_tokens(word, text) %>%
            filter(!word %in% c("@", "female", "athlete")) %>% 
            anti_join(stop_words) %>% 
            inner_join(bing) %>%
            count(sentiment, sort = TRUE) %>% 
            ggplot(aes(x = sentiment, y = n, fill = sentiment)) + 
            geom_col() +
            labs(title = "Positive vs. Negative Sentiments represented within 
       Tweets with 'Female Athlete'",
                 x = "Positive vs. Negative Sentiments",
                 y = "Number of Tweets") 
        twittergraph3
    })
    
    output$twitter4 <- renderPlot({
        
        nrc <- get_sentiments("nrc")
        bing <- get_sentiments("bing")
        
        twittergraph4 <- d3 %>%
            unnest_tokens(word, text) %>%
            filter(!word %in% c("@", "female", "model")) %>% 
            anti_join(stop_words) %>% 
            inner_join(bing) %>%
            count(sentiment, sort = TRUE) %>% 
            ggplot(aes(x = sentiment, y = n, fill = sentiment)) + 
            geom_col() +
            labs(title = "Positive vs. Negative Sentiments represented within 
       Tweets with 'Female Model'",
                 x = "Positive vs. Negative Sentiments",
                 y = "Number of Tweets") 
        twittergraph4
        
    })
    
    
    output$height <- renderPlot({
        
        femaleolympicathlete <- athletedata %>%
            filter(Sex == "F")
        
        sport_filtered <- femaleolympicathlete %>%
            filter(Sport == "Basketball" | Sport == "Figure Skating" | Sport == 
                       "Volleyball"| Sport == "Ice Hockey" | Sport ==  "Weightlifting" |
                       Sport == "Gymnastics") 
        
        sport_filtered %>% 
            na.omit() %>%
            ggplot(aes(x = as.factor(Year), y = Height, fill = Sport)) +
            geom_boxplot() +
            theme(axis.text.x = element_text(size = 5, angle = 20)) +
            labs(title = "Height In Different Female Athletes Over Time", 
                 x = "Olympic Year", 
                 y = "Height (cm)",
                 subtitle = "How have the heights of female athletes in different sports changed over time?") +
            theme(plot.title = element_text(size = 16,face = "bold"))
        
    })
    
    
    
    output$weight <- renderPlot({
        
        femaleolympicathlete <- athletedata %>%
            filter(Sex == "F")
        
        sport_filtered <- femaleolympicathlete %>%
            filter(Sport == "Basketball" | Sport == "Figure Skating" | Sport == 
                       "Volleyball"| Sport == "Ice Hockey" | Sport ==  "Weightlifting" |
                       Sport == "Gymnastics") 
        
        sport_filtered %>% 
            na.omit() %>%
            ggplot(aes(x = as.factor(Year), y = Weight, fill = Sport)) +
            geom_boxplot() +
            theme(axis.text.x = element_text(size = 5, angle = 20)) +
            labs(title = "Weight In Different Female Athletes Over Time", 
                 x = "Olympic Year", 
                 y = "Weight (kg)",
                 subtitle = "How have the weights of female athletes in different sports changed over time?") +
            theme(plot.title = element_text(size = 16,face = "bold"))
        
    })
    
    output$scatterplot <- renderPlot({
        
        femaleolympicathlete <- athletedata %>%
            filter(Sex == "F")
        
        fa_count <- femaleolympicathlete %>% 
            group_by(Year) %>% 
            distinct(Name, .keep_all = TRUE) %>% 
            mutate(female_num = n()) %>%
            distinct(female_num, .keep_all = TRUE)
        
        fa_countupdated <- fa_count %>%
            filter(Year >= 2010) %>%
            select(Year, female_num) %>%
            rename(year = Year)
        
        sidataupdated <- sidata_201015 %>%
            filter(on_cover == "Female Athletes/Coaches") %>%
            filter(year == "2010" | year == "2012" | year == "2014" | year == "2016") %>%
            select(year, frequency)
        
        joined_data <- full_join(fa_countupdated, sidataupdated, by = "year")
        
        
        joined_data %>%
            ggplot(aes(y = frequency, x = female_num)) +
            geom_point(color = "darkred") +
            labs(title = "Relationship between Number of Female Athletes & 
       Representation on SI Covers",
                 x = "Number of Female Athletes",
                 y = "Number of Female Athletes/Coaches on SI Covers",
                 size = "# Female Athletes") +
            theme(plot.title = element_text(size = 13,face = "bold"))
        
    })
    
    
    output$table <- render_gt({
        
        femaleolympicathlete <- athletedata %>%
            filter(Sex == "F")
        
        fa_count <- femaleolympicathlete %>%
            group_by(Year) %>%
            distinct(Name, .keep_all = TRUE) %>%
            mutate(female_num = n()) %>%
            distinct(female_num, .keep_all = TRUE)

        fa_countupdated <- fa_count %>%
            filter(Year >= 2010) %>%
            select(Year, female_num) %>%
            rename(year = Year)

        sidataupdated <- sidata_201015 %>%
            filter(on_cover == "Female Athletes/Coaches") %>%
            filter(year == "2010" |
                       year == "2012" | year == "2014" | year == "2016") %>%
            select(year, frequency)

        joined_data <-
            full_join(fa_countupdated, sidataupdated, by = "year")

        model <- stan_glm(
            data = joined_data,
            formula = frequency ~ female_num,
            family = gaussian(),
            refresh = 0
        )

        table <- tbl_regression(
            model,
            intercept = TRUE,
            estimate_fun = function(x)
                style_sigfig(x, digits = 5)
        ) %>%
            as_gt() %>%
            tab_header(title = "The Effect of # Female Athletes on SI Cover Representation",
                       subtitle = "Will an increase in female athletes increase representation?")
    })
    
    output$parissports <- renderPlot({
        
        femaleolympicathlete <- athletedata %>%
            filter(Sex == "F")
        
        sportdata <- femaleolympicathlete %>% 
            group_by(Year, Sport) %>% 
            summarize(athletecount = n(), .groups = "drop")
        
        parissports <- sportdata %>%
            filter(Year == 1900)
        
        parissports %>%
            ggplot(aes(y = Sport, x = athletecount)) +
            geom_col(fill = "pink") +
            labs(title = "Paris 1900 Number of Female Athletes Competing in each Sport",
                 x = "Number of Female Athletes",
                 y = "Type of Sport") +
            theme_minimal() +
            theme(plot.title = element_text(size = 12,face = "bold"))
    })
    
    output$romesports <- renderPlot({
        
        femaleolympicathlete <- athletedata %>%
            filter(Sex == "F")
        
        sportdata <- femaleolympicathlete %>% 
            group_by(Year, Sport) %>% 
            summarize(athletecount = n(), .groups = "drop")
        
        romesports <- sportdata %>%
            filter(Year == 1960)
        
        romesports %>%
            ggplot(aes(y = Sport, x = athletecount)) +
            geom_col(fill = "maroon") +
            labs(title = "Rome 1960 Number of Female Athletes Competing in each Sport",
                 x = "Number of Female Athletes",
                 y = "Type of Sport") +
            theme_minimal() +
            theme(plot.title = element_text(size = 12,face = "bold"))
    })
    
    output$riosports <- renderPlot({
        
        femaleolympicathlete <- athletedata %>%
            filter(Sex == "F")
        
        sportdata <- femaleolympicathlete %>% 
            group_by(Year, Sport) %>% 
            summarize(athletecount = n(), .groups = "drop")
        
        riosports <- sportdata %>%
            filter(Year == 2016)
        
        riosports %>%
            ggplot(aes(y = Sport, x = athletecount)) +
            geom_col(fill = "darkred") +
            labs(title = "Rio 2016 Number of Female Athletes Competing in each Sport",
                 x = "Number of Female Athletes",
                 y = "Type of Sport") +
            theme_minimal() +
            theme(plot.title = element_text(size = 12,face = "bold"))
    })
    
}



# Run the application 
shinyApp(ui = ui, server = server)
