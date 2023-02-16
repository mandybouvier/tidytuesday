## Tidy Tuesday ##
## Week 7: This dataset is about the Hollywood age gaps between two characters in a romantic relationship in some movies ##
## Note from TidyTuesday repo: The age gaps dataset includes "gender" columns, which always contain the values "man" or "woman". These values appear to indicate how the characters in each film identify. Some of these values do not match how the actor identifies. We apologize if any characters are misgendered in the data! ##
## By: Amanda Vieira ##

# Packages --------------------------------------
library(dplyr)
library(ggplot2)

# Dataset ---------------------------------------

# Data
holly_diff <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-14/age_gaps.csv')

# Checking
glimpse(holly_diff)
View(holly_diff)

# QUESTION ---------------------------------------

# Is age difference for heterossexual couples biased for older men than older women?

## Working on the dataset ------------------------

# Checking for non-heterosexual couples first
holly_diff$couple <- NA

for(i in 1:nrow(holly_diff)){

  if(holly_diff$character_1_gender[i] == holly_diff$character_2_gender[i]){
    holly_diff$couple[i] <- "queer"
  }else{
    holly_diff$couple[i] <- "boring"
  }

}


# Checking
glimpse(holly_diff)

# Filter for non-heterosexual couples
queer <- holly_diff |>
  filter(couple == "queer")

# Filter for the heterosexual couples
age_gaps <- holly_diff |>
  filter(couple == "boring")

# The actor_1_name and actor_2_name is both woman and man, I'll add two new columns one for actor_man and another for actor_woman. I'll match the ages too
age_gaps$actor_man <- NA
age_gaps$actor_woman <- NA
age_gaps$age_man <- NA
age_gaps$age_woman <- NA

for(i in 1: nrow(age_gaps)){

## For character 1
  if(age_gaps$character_1_gender[i] == "man"){
    age_gaps$actor_man[i] <- age_gaps$actor_1_name[i]
    age_gaps$age_man[i] <- age_gaps$actor_1_age[i]
  } else{
    age_gaps$actor_woman[i] <- age_gaps$actor_1_name[i]
    age_gaps$age_woman[i] <- age_gaps$actor_1_age[i]
  }

## For character 2
  if(age_gaps$character_2_gender[i] == "man"){
    age_gaps$actor_man[i] <- age_gaps$actor_2_name[i]
    age_gaps$age_man[i] <- age_gaps$actor_2_age[i]
  }else{
    age_gaps$actor_woman[i] <- age_gaps$actor_2_name[i]
    age_gaps$age_woman[i] <- age_gaps$actor_2_age[i]
  }

}

# Checking
glimpse(age_gaps)
View(age_gaps)

# Now, I'll standardize the ages: if the man character is older than the woman, this difference will be positive, otherwise, the difference will be negative
age_gaps <- age_gaps |>
  mutate(std_age_diff = age_man - age_woman)

# Checking
glimpse(age_gaps)


## Data viz ------------------------------------

# Fonts
windowsFonts(Times=windowsFont("Times New Roman"))

age_gaps$nobs <- 1:1132

# Choosing colors
age_gaps$colors <- NA  # For points
age_gaps$colors2 <- NA # For segments


for(i in 1:nrow(age_gaps)){

  # Man is older than woman
  if(age_gaps$std_age_diff[i] > 0){
    age_gaps$colors[i] <- "#FA8128"
    age_gaps$colors2[i] <- "#FFF3e1"

  }

  # Woman is older than man
  if(age_gaps$std_age_diff[i] < 0){
    age_gaps$colors[i] <- "#7f00b2"
    age_gaps$colors2[i] <- "#e1cbee"

  }

  # Man has the same age as woman
  if(age_gaps$std_age_diff[i] == 0){
    age_gaps$colors[i] <- "white"
    age_gaps$colors2[i] <- "grey"

  }

}


# Let's do a lollipop plot!
ggplot(age_gaps, aes(x = std_age_diff, y = nobs)) +
  geom_segment(aes(y=nobs, yend=nobs, x = 0, xend = std_age_diff),
               color=age_gaps$colors2, linewidth = 0.1) +
  geom_point(color = age_gaps$colors, size=2) +
  #geom_point(x = -48, y = 1000, col = "#7f00b2", size = 5) +
  #geom_point(x = -48, y = 930, col = "#FA8128", size = 5) +
  geom_vline(xintercept = 0, color = "gray") +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 12, color = "white", family = "Times"),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 14, color = "white", family = "Times", margin = margin(t = 5)),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill  = "black"),
    panel.background = element_rect(fill = "black"),
    plot.title = element_text(color = "white", family = "Times", size = 35, hjust = 0.5, face = "italic"),
    plot.subtitle = element_text(color = "white", family = "Times", hjust = 0.85),
    plot.caption = element_text(color = "grey", family = "Times")
) +
  labs(x = "Age difference (years)", title = "Age difference between Hollywood movies love interests*", subtitle = "* Heterosexual couples", caption = "TidyTuesday week 07 / 2023 \n Source: https://hollywoodagegap.com/") +
  scale_y_continuous(expand = c(0,0)) +
  annotate("text", x = -30, y = 1000, label= "Woman is older than man", color = "#7f00b2", family = "Times", size = 6) +
  annotate("text", x = -30, y = 950, label= "It represents 19% of the couples", color = "#7f00b2", family = "Times", size = 4.5) +
  annotate("text", x = -30, y = 900, label= "Average difference: 5.5 years", color = "#7f00b2", family = "Times", size = 4) +
  annotate("text", x = 30, y = 1000, label= "Man is older than woman", color = "#FA8128", family = "Times", size = 6) +
  annotate("text", x = 30, y = 950, label= "It represents 81% of the couples", color = "#FA8128", family = "Times", size = 4.5) +
  annotate("text", x = 30, y = 900, label= "Average difference: 11.7 years", color = "#FA8128", family = "Times", size = 4)




## Descriptive statistics ---------------------------

# When man is older, the average age difference if 11.7 years
age_gaps |>
  filter(std_age_diff > 0) |>
  summarise(mean(std_age_diff),
            median(std_age_diff))

# When Woman is older, the average age difference if 5.5 years
age_gaps |>
  filter(std_age_diff < 0) |>
  summarise(mean(std_age_diff),
            median(std_age_diff))

# What is the proportion of couples with older man?
age_gaps |>
  filter(std_age_diff > 0) |>
  count() / nrow(age_gaps)

# QUESTION ------------------------------------------

# What is the age difference between queer couples?

## Data viz QUEER ------------------------------------

# Number of observation
queer$nobs <- 1:23

# Adding a column
queer$letter <- NA

# Add information
for(i in 1:nrow(queer)){

  if(queer$character_1_gender[i] == "man"){
    queer$letter[i] <- "gay"
  } else{
    queer$letter[i] <- "lesbeean"
  }
}


# Gay couples will be on the left side, while lesbeean couples will be on the right
for(i in 1:nrow(queer)){

  if(queer$letter[i] == "gay"){
    queer$age_difference[i] <- queer$age_difference[i] * -1
  }
}

# Choosing colors
queer$colors <- c(rep(c("#FF0000", "#FF7F00", "#FFFF00", "#00FF00", "#0000FF", "#4B0082", "#9400D3"), times = 3), "#FF0000", "#FF7F00")
queer$colors2 <- NA # For segments



names(queer)
# Let's do a lollipop plot!
ggplot(queer, aes(x = age_difference, y = nobs)) +
  geom_segment(aes(y=nobs, yend=nobs, x = 0, xend = age_difference),
               color=queer$colors, linewidth = 0.1) +
  geom_point(color = queer$colors, size=2) +
  geom_vline(xintercept = 0, color = "gray") +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 12, color = "white", family = "Times"),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 14, color = "white", family = "Times", margin = margin(t = 5)),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill  = "black"),
    panel.background = element_rect(fill = "black"),
    plot.title = element_text(color = "white", family = "Times", size = 35, hjust = 0.5, face = "italic"),
    plot.subtitle = element_text(color = "white", family = "Times", hjust = 0.85)
  ) +
  labs(x = "Age difference (years)", title = "Age difference between Hollywood movies love interests*", subtitle = "* Queer couples") +
  scale_y_continuous(expand = c(0,0)) +
  annotate("text", x = -25, y = 18, label= "Lesbeean", color = "white", family = "Times", size = 6) +
  annotate("text", x = 25, y = 18, label= "Gay", color = "white", family = "Times", size = 6)

