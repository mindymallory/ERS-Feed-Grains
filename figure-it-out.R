library(tidyverse)
library(readxl)
library(httr)
library(tidyr)

tmp <- tempfile(fileext = ".xls")

httr::GET(url = "https://www.ers.usda.gov/webdocs/DataFiles/50048/Feed%20Grains%20Yearbook%20Tables-All%20Years.xls",
          write_disk( tmp) )

data_xl <- read_excel(tmp, sheet = "FGYearbookTable04-Full", 
                      skip = 4, 
                      col_names = c("MktYear", "Quarter", "BeginningStocks", "Production", "Imports", 
                                    "TotalSupply", "FoodAlcoholInd", "Seed", "FeedandResidual", 
                                    "TotalDomestic", "Exports", "TotalUse", "EndingStocks")) %>%
  fill(MktYear) %>% 
  # Make columns for more standard date handling
  mutate(month = ifelse(Quarter =="Q1 Sep-Nov", "09-01", 
                        ifelse(Quarter == "Q2 Dec-Feb", "12-01", 
                               ifelse(Quarter == "Q3 Mar-May", "03-01", 
                                      ifelse(Quarter == "Q4 Jun-Aug", "06-01",
                                             ifelse(Quarter == "MY Sep-Aug", "09-01", NA)))))) %>%
  mutate(year = ifelse(Quarter =="Q1 Sep-Nov" | Quarter == "Q2 Dec-Feb" | Quarter == "MY Sep-Aug", as.numeric(str_sub(MktYear, 1,4)), 
                       as.numeric(str_sub(MktYear, 1,4)) +1 )) %>%
  mutate(date = as.Date(paste0(as.character(year), "-", month)))

# Make tidy getting ready for ggplot
data_tidy <- data_xl %>% 
  select(c(-MktYear, -month, -year)) %>%
  pivot_longer(c(-date, -Quarter), names_to = "UseCategory", values_to = "Value")

# Plot the Marketing Year Data
data_tidy %>% filter(Quarter == "MY Sep-Aug") %>%
  filter(UseCategory %in% c("FoodAlcoholInd", "Seed", "FeedandResidual", "Exports", "EndingStocks")) %>%
  ggplot(aes(x = date, y = Value, fill = UseCategory)) + 
  geom_area() + 
  theme_bw() +
  labs(x = "", y = "Millions of bushels", title = "Corn Use Categories Since 1975")

