## Tidy Tuesday ##
## Week 5: dataset on the track of 925 cats ##
## By: Amanda Vieira


# Packages ----------------------
library(tidytuesdayR)
library(dplyr)
library(ggplot2)
library(grid)

# Data -------------------------
tt_data <- tt_load(2023, week=5)

# Dataset 1
tib1 <- tt_data[[1]]
tib2 <- tt_data[[2]]
rm(tt_data)

# Let's check it
glimpse(tib1)
summary(tib1)

glimpse(tib2)
summary(tib2)

# Age histogram
ggplot(tib2, aes(x = age_years)) +
  geom_density(col = "black") +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  labs(x = "Age (years)", y = "Density")

# animal_sex x hours indoors -----------------

# Does the sex affect how many hours they spent outside?
tib2 |> 
  mutate(animal_sex = as.factor(animal_sex)) |> 
  group_by(animal_sex) |> 
  summarise(mean(hrs_indoors))



# Adding an image
cat1 <- magick::image_read("https://images.phylopic.org/images/23cd6aa4-9587-4a2e-8e26-de42885004c9/raster/1024x833.png")

g1 <- ggplot(tib2, aes(x = animal_sex, y = hrs_indoors)) +
  geom_jitter(alpha = 0.4, width = 0.25, col = "#9FE7F5") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult=1), col = "dodgerblue") +
  stat_summary(fun = mean, geom = "point", size = 3, col = "black") +
  stat_summary(fun = mean, geom = "point", size = 2, col = "dodgerblue") +
  scale_y_continuous(expand = c(0,0), limits = c(0,25)) +
  theme_classic() +
  labs(x = "Sex of the cat", y = "Time indoor (hours)", caption = "Pet Cats UK") +
  scale_x_discrete(labels = c("Female", "Male")) +
  theme(
    axis.text = element_text(color = "black", size = 11),
    axis.title = element_text(size = 12),
    plot.caption = element_text(color = "gray")
  ) +
  annotation_custom(rasterGrob(cat1),ymin = 0, ymax = 7, xmin = 0, xmax = 1)

ggsave("ttw5a.png", g1)


# Reproductive condition x indoors -------------------
glimpse(tib2)

# Checking reproductive condition
tib2 <- tib2 |> 
  mutate(animal_reproductive_condition = as.factor(animal_reproductive_condition))

# Assuming neutered and spayed as fixing
tib2 <- tib2 |> 
  mutate(rep_condition = case_when(
    animal_reproductive_condition == "Neutered" | animal_reproductive_condition == "Spayed" ~ "fixed")
)

# Problem Houston: only two not fixed cats 
sum(tib2$animal_reproductive_condition == "Not fixed", na.rm = TRUE)



  
