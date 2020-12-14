# First I needed to load all the packages that I thought I needed for this 
# final project. Packages like tidyverse so I have all the functions, readxl so 
# that I can properly read in my data, janitor so that I can clean my data, and
# before I realized how complicated using animations in shiny was, I wanted to 
# try out making gganimate graphs and importing. If I have time leftover, I might
# still choose to include some animations in the form of gifs or videos in my project.

library(tidyverse)
library(readxl)
library(janitor)
library(gganimate)

# First Tab: Women in Sport
# In this sections I want to talk about participation in women's sport, looking 
# at the olympic athlete data that I found. I want to look at the countries 
# around the world, their height and weight and especially emphasize how 
# women's participation in sport has increased. I will be creating graphs 
# from the olympic athlete data which I have cleaned below. 

# First I read in the data and took a look at the variables that I'm working with. 
# The dataset is huge so view() takes ages!! 

athletedata <- read_csv("Female_Athlete_Data_Final/athlete_events.csv")
write_rds(athletedata, "Female_Athlete_Data_Final/athletedata.rds")

# Then I filtered for only "F" so that only female athletes pop up. 

femaleolympicathlete <- athletedata %>%
  filter(Sex == "F")

# This is what I did to get rid of the duplicate female athletes within one year 
# but still keeping duplicate female athletes if they competed in different years. 
# Now that it's a bit cleaned and filtered, it's easier loading the data. 

femaleolympicathlete %>% 
  group_by(Year) %>% 
  distinct(Name, .keep_all = TRUE)

# First I want to create a graph that shows the increase in female athletes
# over the Olympics since the earliest one in the 1900. This means I have to 
# figure out how to create a column with the number of female athletes that 
# competed in each olympics (1900, 1904, 1908 and so on). I used count() and 
# assigned the piece of code to fa_count which gave me a table of years from 
# 1900 to 2016 and the number of female athletes that went under the column 
# title n. 

fa_count <- femaleolympicathlete %>% 
  group_by(Year) %>% 
  distinct(Name, .keep_all = TRUE) %>% 
  mutate(female_num = n()) %>%
  distinct(female_num, .keep_all = TRUE)

# In the graph I want n on the y-axis, with the different Years on the x-axis.

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
  theme(axis.text.x = element_text(angle = 270, size = 8))

# I also want to showcase the number of different countries and their female athletes
# represented in the early days of the Olympics vs. now. I want to do this with a 
# map. 

# I had to load many different libraries cuz Shivi and I were playing around with
# which ones were actually going to work, some of the ones kept giving me problems
# but then we finally figured it out! 

library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)

# I used another another final project as a guide on how to do the maps part 
# especially the set up with having to find another dataset with regions. I first
# joined these datasets and then filtered for the first Olympics I wanted to look at 
# which was Paris 1900, in which 23 female athletes attended. 

noc_regions <- read_csv("Female_Athlete_Data_Final/NOCregions.csv")
write_rds(noc_regions, "Female_Athlete_Data_Final/noc_regions.rds")

regions <- femaleolympicathlete %>% 
  left_join(noc_regions, by = "NOC") %>%
  filter(!is.na(region))

Paris_data <- regions %>% 
  filter(Games == "1900 Summer") %>%
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
  labs(title = "Number of Female Athletes at Paris 1900 Olympics") +
  scale_fill_gradient2(low = "white", high = "blue") +
  guides(fill = guide_colourbar(title = "#")) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "lightblue"))

# Now that I've made the Paris 1900 map, I'm going to skip ahead to 1960 in Rome

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
  labs(title = "Number of Female Athletes at Rome 1960 Olympics") +
  scale_fill_gradient2(low = "white", high = "blue") +
  guides(fill = guide_colourbar(title = "#")) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "lightblue"))

# Now that I've made the Rome map, I'm going to choose another three Olympics in 
# more recent years: Montreal in 1976, Atlanta in 1996 and Rio 2016.

# Montreal

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
  labs(title = "Number of Female Athletes at Montreal 1976 Olympics") +
  scale_fill_gradient2(low = "white", high = "blue") +
  guides(fill = guide_colourbar(title = "#")) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "lightblue"))

# Atlanta
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
  labs(title = "Number of Female Athletes at Atlanta 1996 Olympics") +
  scale_fill_gradient2(low = "white", high = "blue") +
  guides(fill = guide_colourbar(title = "#")) +
  theme(plot.title = element_text(hjust = 0.50),
        panel.background = element_rect(fill = "lightblue"))

# Rio 2016: 

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
  labs(title = "Number of Female Athletes at Rio 2016 Olympics") +
  scale_fill_gradient2(low = "white", high = "blue") +
  guides(fill = guide_colourbar(title = "#")) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "lightblue"))

# The next graph I want to make is looking at the different sports represented in 
# 1900 vs 1960 vs in 2016. 

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

# Second Tab: Body Image in the Media
# For this tab, I wanted to look at Twitter data and therefore had to 
# load the twitteR packages and then use the API key to make sure I had 
# access to everything. It was difficult to get the process sorted but once
# I had the API sorted out, I could actually use arguments to gather the 
# data and create my graphs.

# Twitter API Section

library(twitteR)
library(tidytext)
library(tidyverse)
library(textdata)

consumer_key = "MewBQ0YxBWx8RdCqECC74sDje"
consumer_secret = "DXmNo3q1wVx0u4LvuNWd2gztTIxK53oFe1oITsmilG2fMLGfIW"
access_token = "771740238909693953-paykKebLmDJxoKkZtTlGgSUG44qambo"
access_secret = "eX8mRBNgfjJLMPggjYupM5PyQpWAFm1iDwcXcHPmEEBFz"

setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

tw2 = searchTwitter('female athlete', n = 100)
d2 = twListToDF(tw2)

tw3 = searchTwitter('female model', n = 100)
d3 = twListToDF(tw3)

# After getting my data, I can now use get_sentiments to create my graphs of 
# the different sentiments. 

nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")

d2 %>%
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

d2 %>%
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

d3 %>%
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


d3 %>%
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

# Twitter was giving me a lot of trouble with publishing so the below is 
# me trying to write rds files so that it could work

tw2 = searchTwitter('female athlete', n = 100)
d2 = twListToDF(tw2)

write_rds(d2, "Female_Athlete_Data_Final/d2.rds")

tw3 = searchTwitter('female model', n = 100)
d3 = twListToDF(tw3)

write_rds(d3, "Female_Athlete_Data_Final/d3.rds")


# Third Tab: Female Athletes in Media 

# Sports Illustrated Section
# I had to first read in the data that I was inspired to add more data to by 
# another author's github. I had to read it but also write it into an rds 
# file to transfer it to my shinyapp. 

sidata_201015 <- read_csv("Female_Athlete_Data_Final/sicover_update.csv")
sidata_201015 <- sidata_201015 %>%
  clean_names()

write_rds(sidata_201015, "Female_Athlete_Data_Final/sidata_201015.rds")

# Viewing data always helps me get an idea of what I'm working with before 
# cleaning. Luckily, since this was in a pretty clean excel sheet that I 
# also added to myself, only thing I had to do was clean_names to make
# sure I could use the variables easily. 


# I then created my ggplot, with x being year and y being frequency. The fill
# was then put as who was on the cover, males, female athletes, models etc. I used
# a geom_col and then used scale_fill_manual to pick my colors for the different
# bars. I think this was a pretty good graph to visualize the differences
# between the people who were featured on the cover of Sports Illustrated. I'm
# glad I came up with the idea to update it to 2020 as well.

p <- ggplot(sidata_201015, aes(x = year, y = frequency, fill = on_cover)) +
  geom_col() +
  scale_fill_manual(values = c("darkcyan", "coral", "azure4", "darkolivegreen",
                               "chocolate", "darkseagreen"), 
                    guide_legend(title="Who is on the cover?")) +
  labs(title = "Comparison of Who is on Sports Illustrated Covers 
throughout the Decade",
       x = "Year", 
       y = "Number of Covers") +
  theme_minimal()
p

# I also wanted to include google trends in this section and installed the
# googletrends package. I wanted to show the comparison between search hits
# for female athletes and female models, to go along with the theme of the SI
# covers

library(gtrendsR)

googletrends <- gtrends(c("female athlete", "female model"))
plot(googletrends)

# I wanted to compare within sports as well, so I types in some more
# "masculine sports" like rugby, and more feminine like soccer and volleyball, 
# then further compared with female models

gtrends <- gtrends(c("female rugby", "female model",
                     "female volleyball", "female soccer"))
plot(gtrends)

# Also included in the media tab is my statistical model. 
# I want to show the effect of women's increased sport participation on the 
# representation of female athletes in the media. 

library(rstanarm)

fa_countupdated <- fa_count %>%
  filter(Year >= 2010) %>%
  select(Year, female_num) %>%
  rename(year = Year)

sidataupdated <- sidata_201015 %>%
  filter(on_cover == "Female Athletes/Coaches") %>%
  filter(year == "2010" | year == "2012" | year == "2014" | year == "2016") %>%
  select(year, frequency)

joined_data <- full_join(fa_countupdated, sidataupdated, by = "year") %>%
  mutate(summer = ifelse(year %in% c("2012", "2016"), TRUE, FALSE))


model <- stan_glm(data = joined_data,
                  formula = frequency ~ female_num * summer,
                  family = gaussian(),
                  refresh = 0)

print(model, digits = 5)

# In the version above, I had attempted to control for summer vs. winter olympics 
# and the interaction between that and female_num, but there was no major significance. 
# So I just stuck with my original model that I had. I created the table with the
# below libraries. I also created a scatter plot to display the lack of 
# relationship.

library(gt)
library(gtsummary)
library(broom.mixed)

tbl_regression(model, intercept = TRUE, 
               estimate_fun = function(x) style_sigfig(x, digits = 5)) %>%
  as_gt() %>%
  tab_header(title = "The Effect of # Female Athletes on SI Cover Representation",
             subtitle = "Will an increase in female athletes increase representation?")

joined_data %>%
  ggplot(aes(y = frequency, x = female_num)) +
  geom_point(color = "darkred") +
  labs(title = "Relationship between Number of Female Athletes & 
       Representation on SI Covers",
       x = "Olympic Years",
       y = "Number of Female Athletes/Coaches on SI Covers",
       size = "# Female Athletes") +
  theme(plot.title = element_text(size = 13,face = "bold"))


# Fourth Tab: Female Athletes' Body Image
# For this tab, I want to include more detailed information on different
# body types across sports, more feminine sports and less feminine sports. 

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


