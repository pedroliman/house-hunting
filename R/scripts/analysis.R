


library(here)
library(dplyr)
library(janitor)
library(readxl)
library(lubridate)
library(stringr)
library(stringi)
library(ggplot2)
library(randplot)
library(showtext)
library(ggh4x)
library(gmapsdistance)
library(dotwhisker)


# plot settings
font_add_google("Noto Sans")

width <- unit(10, "cm")

base_theme <- theme_rand(font = "Noto Sans") +  
  theme(legend.key.width=unit(3,"cm"))

showtext_auto()


# read data
data <- read_xlsx(here("data/redfin/redfin-results.xlsx"),sheet = "results") %>%
  distinct()


# clean up data -----------------------------------------------------------

data <- data %>%
  janitor::clean_names()


# date needs formatting

months_char <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
months_num <- stringr::str_pad(as.character(1:12), width = 2, pad = "0")  

data$sold_date <- stringi::stri_replace_all_fixed(data$sold_date, months_char, months_num, vectorize_all=FALSE)

# split date into a data.frame
sold_dates <- str_split(data$sold_date, pattern = "-", simplify = T) %>% as.data.frame()

names(sold_dates) <- c("m", "d", "y")

# pad day
sold_dates$d <- stringr::str_pad(sold_dates$d, width = 2, pad = "0") 

sold_dates$date <- paste(sold_dates$y, sold_dates$m, sold_dates$d, sep = "/")

data$sold_date <- as.Date(sold_dates$date)


# Distance to desired location --------------------------------------------

data$latlong <- paste(data$latitude, data$longitude, sep = "+")

data$destination <- "35.7321975+-78.7318912"

drive_distance_results = gmapsdistance(
                          origin = data$latlong, 
                          destination = data$destination, 
                          mode = "driving", 
                          combinations = "pairwise",
                          key=Sys.getenv("GOOGLE_API_KEY")
                          ) %>% as.data.frame()


data$mins_to_church <- drive_distance_results$Time.Time / 60

data$km_to_church <- drive_distance_results$Distance.Distance / 1000

# Write results to spreadsheet:
writexl::write_xlsx(data,path = here("data/redfin/formatted-results.xlsx"))

data <- read_xlsx(here("data/redfin/formatted-results.xlsx"))

# sold date vs price

data %>%
  filter(!is.na(sold_date)) %>%
  filter(!is.na(beds)) %>%
  filter(!(beds == 4 & property_type == "Townhouse")) %>%
  filter(price <= 7.5e5) %>%
  filter(beds %in% 3:4) %>%
  ggplot(mapping = aes(x = sold_date, y = price / 1e3, color = property_type)) + 
  geom_point() + 
  geom_smooth() + 
  geom_vline(xintercept = as.POSIXct(as.Date("2022-10-10")),colour="black") + 
  #geom_quantile() + 
  #geom_hline(yintercept = 450000) + 
  base_theme + 
  ylab("Price (thousands)") + 
  xlab("Sold date") + 
  #labs(title = "Prices peaked in Garner when rates reached 7%") + 
  scale_y_continuous(labels = ~paste0("$ ",.x)) + 
  #annotate("text", x = as.POSIXct(as.Date("2022-11-10")), y = 650, label = "Oct 10th")+
  facet_wrap(~beds, labeller = "label_both", ncol = 1, scales = "free")  
  



# mins to church vs price
data %>%
  filter(property_type %in% c("Single Family Residential", "Townhouse")) %>%
  filter(price <= 7.5e5) %>%
  filter(!is.na(sold_date)) %>%
  mutate(year_sold = lubridate::year(sold_date)) %>%
  filter(mins_to_church >=10) %>%
  mutate(within_budget = price < 400000) %>%
  filter(!is.na(beds)) %>%
  ggplot(mapping = aes(x = mins_to_church, y = price, color = within_budget)) + 
  geom_point() + 
  #geom_smooth() + 
  #scale_color_binned(type = "viridis") + 
  base_theme +
  facet_wrap(facets = ~property_type)


#year built vs price
data %>%
  filter(!is.na(beds)) %>%
  ggplot(mapping = aes(x = year_built, y = mins_to_church, color = price)) + 
  geom_point() + 
  geom_smooth() + 
  #geom_quantile() + 
  #geom_hline(yintercept = 450000) + 
  base_theme + 
  scale_color_viridis_b() + 
  facet_wrap(facets = ~property_type)


# Can we control for stuff and do a time-varying estimation:

reg_data <- data %>%
  mutate(m. = paste0(year(sold_date), ".", str_pad(month(sold_date), width = 2, pad = "0") )) %>%
  filter(beds %in% 3:4) %>%
  # Only "new" homes - sold within 1 year of year_built
  filter(year(sold_date) <= year_built + 1) %>%
  mutate(sq_ft.250 =  (square_feet - mean(square_feet)) / 250) %>%
  mutate(beds = beds - 2)

m0 <- lm(price ~ sq_ft.250, data = reg_data)
  
m1 <- update(m0, . ~ . + beds)

m2 <- update(m1, . ~ . +  + property_type)

m3 <- update(m2, . ~ . +  + m.)


dwplot(list(m0, m1, m2, m3)) + base_theme

# parallel coordinates plots:
# library(GGally)
# 
# par_columns <- which(names(data) %in% c("price", "mins_to_church", "year_built")) 
# 
# ggparcoord(data,
#            columns = par_columns,
#            groupColumn = "city",
#            scale = "uniminmax") +
#   scale_color_brewer(palette = "Set2")
