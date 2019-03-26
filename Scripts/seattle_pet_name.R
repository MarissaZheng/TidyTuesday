library(tidyverse)
library(lubridate)
library(treemapify)
seattle_pets_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-26/seattle_pets.csv")

# dog
seattle_pets %>% 
  filter(!is.na(animals_name), 
         year(license_issue_date) == 2018, 
         species == "Dog") %>% 
  group_by(animals_name) %>% 
  summarise(obs_n = n()) %>% 
  top_n(20, wt = obs_n) %>% 
  pull(animals_name) -> top_20_dog


seattle_pets %>% 
  filter(!is.na(animals_name), 
         year(license_issue_date) == 2018, 
         species == "Dog",
         animals_name %in% top_20_dog) %>%
  group_by(animals_name, primary_breed) %>% 
  summarise(number = n()) %>% 
  ggplot(aes(area = number, label = primary_breed, subgroup = animals_name)) +
  geom_treemap(fill = "#E69F00") +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                    grow = TRUE) +
  geom_treemap_subgroup_border(colour = "#009E73") +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                             "#0072B2", fontface = "italic", min.size = 0) +
  labs(title = "2018 Seattle's Most Popular Dog Names and Their Breeds",
       caption = "Plot: @meinizi_z  Data: Seattle.gov") +
  theme(plot.title = element_text(face = "bold", size = 20, lineheight = 0.1),
        plot.caption = element_text(size = 17))


# cat
seattle_pets %>% 
  filter(!is.na(animals_name), 
         year(license_issue_date) == 2018, 
         species == "Cat") %>% 
  group_by(animals_name) %>% 
  summarise(obs_n = n()) %>% 
  top_n(20, wt = obs_n) %>% 
  pull(animals_name) -> top_20_cat


seattle_pets %>% 
  filter(!is.na(animals_name), 
         year(license_issue_date) == 2018, 
         species == "Cat",
         animals_name %in% top_20_cat) %>%
  group_by(animals_name, primary_breed) %>% 
  summarise(number = n()) %>% 
  ggplot(aes(area = number, label = primary_breed, subgroup = animals_name)) +
  geom_treemap(fill = "#CC79A7") +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                    grow = TRUE) +
  geom_treemap_subgroup_border(colour = "#0072B2") +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                             "#0072B2", fontface = "italic", min.size = 0) +
  labs(title = "2018 Seattle's Most Popular Cat Names and Their Breeds",
       caption = "Plot: @meinizi_z  Data: Seattle.gov") +
  theme(plot.title = element_text(face = "bold", size = 20, lineheight = 0.1),
        plot.caption = element_text(size = 17))


