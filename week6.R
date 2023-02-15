## Tidy Tuesday ##
## Week 6: This dataset consists of the daily stock prices and volume of 14 different tech companies, including Apple (AAPL), Amazon (AMZN), Alphabet (GOOGL), and Meta Platforms (META) and more! ##
## By: Amanda Vieira ##

# Packages ------------------------------

library(dplyr)
library(lubridate)
library(ggplot2)
library(ggimage)

# Data ----------------------------------

# Getting the data
big_tech_stock_prices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')

big_tech_companies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')

# Joining the dataset
tech_data <- big_tech_stock_prices |>
  full_join(big_tech_companies, by = "stock_symbol")

# Removing the datasets
rm(big_tech_companies)
rm(big_tech_stock_prices)

# Checking the dataset
glimpse(tech_data)

# Chaging the data to month-year
names(tech_data)

tech_data <- tech_data |>
  mutate(
    date = ymd(date),
    month_yr = format_ISO8601(date, precision = "ym"),
    yr = format_ISO8601(date, precision = "y"),
    stock_symbol = as.factor(stock_symbol),
    company = as.factor(company),
    yr = as.factor(yr),
    diff = open - close
  )

# Questions ---------------------------------

## Price through time ---------------------

# How the price of these companies has changed through time (chaos)
ggplot(tech_data, aes(month_yr,company)) +
  geom_tile(aes(fill = adj_close),colour = "white") +
  scale_fill_gradient(low = "yellow", high = "red")

# Let's summarise the information
prices <- tech_data |>
  group_by(company, yr) |>
  summarise(mean_adj_close = mean(adj_close),
            mean_diff = mean(diff))

# Let's try to plot it again
ggplot(prices, aes(yr,company)) +
  geom_tile(aes(fill = mean_diff),colour = "white") +
  scale_fill_gradient(low = "yellow", high = "red")

# For most companies, we do not have info for 2023, let's remove this year
prices <- prices |>
  filter(yr != 2023)

# Let's try to plot it again
ggplot(prices, aes(yr,reorder(company, +mean_diff))) +
  geom_tile(aes(fill = mean_diff), color = "white") +
  scale_fill_gradient2(low = "orange", mid = "#EEEEEE", high = "purple") +
  theme_minimal()

# Rename companies names
prices <- prices |>
  mutate(
  company_name = case_when(
    company == "Adobe Inc." ~ "Adobe",
    company == "Tesla, Inc." ~ "Tesla",
    company == "Amazon.com, Inc." ~ "Amazon",
    company == "Salesforce, Inc." ~ "Salesforce",
    company == "NVIDIA Corporation" ~ "NVIDIA",
    company == "Alphabet Inc." ~ "Alphabet",
    company == "Cisco Systems, Inc." ~ "Cisco",
    company == "Netflix, Inc." ~ "Netflix",
    company == "Intel Corporation" ~ "Intel",
    company == "Apple Inc." ~ "Apple",
    company == "Microsoft Corporation" ~ "Microsoft",
    company == "Meta Platforms, Inc." ~ "Meta",
    company == "International Business Machines Corporation" ~ "IBM",
    company == "Oracle Corporation" ~ "Oracle")
)


# Let's try to plot it again
png(filename = "week6.png", width = 2100, height = 1600, res = 300)
ggplot(prices, aes(yr, reorder(company_name, +mean_diff))) +
  geom_tile(aes(fill = mean_diff), color = "white") +
  scale_fill_gradient2(low = "orange", mid = "#EEEEEE", high = "purple", name = "Difference") +
  labs(x = "Year", y = "") +
  coord_cartesian(clip = "off") +
  geom_image(mapping = aes(x = 0, y = 14, image = paste0("tesla.jpg"))) +
  geom_image(mapping = aes(x = 0, y = 13, image = paste0("amazon.jpg"))) +
  geom_image(mapping = aes(x = 0, y = 12, image = paste0("salesforce.png"))) +
  geom_image(mapping = aes(x = 0, y = 11, image = paste0("nvidia.png"))) +
  geom_image(mapping = aes(x = 0, y = 10, image = paste0("adobe.png"))) +
  geom_image(mapping = aes(x = 0, y = 9,  image = paste0("alphabet.png"))) +
  geom_image(mapping = aes(x = 0, y = 8,  image = paste0("cisco.png"))) +
  geom_image(mapping = aes(x = 0, y = 7,  image = paste0("netflix.png"))) +
  geom_image(mapping = aes(x = 0, y = 6,  image = paste0("intel.png"))) +
  geom_image(mapping = aes(x = 0, y = 5,  image = paste0("oracle.png"))) +
  geom_image(mapping = aes(x = 0, y = 4,  image = paste0("apl_logo.png"))) +
  geom_image(mapping = aes(x = 0, y = 3,  image = paste0("microsoft.jpg"))) +
  geom_image(mapping = aes(x = 0, y = 2,  image = paste0("meta.png"))) +
  geom_image(mapping = aes(x = 0, y = 1,  image = paste0("IBM.png"))) +
  theme_minimal() +
  theme(axis.text.y = element_text(margin = margin(r = 15), color = "grey"),
        axis.text.x = element_text(color = "black", size = 8))
dev.off()

