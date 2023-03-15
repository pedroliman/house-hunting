
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
library(googleway)


source("./R/funs/pareto_frontier.R")

# plot settings
font_add_google("Noto Sans")

width <- unit(10, "cm")

base_theme <- theme_rand(font = "Noto Sans") +  
  theme(legend.key.width=unit(3,"cm"))

showtext_auto()


# read data
data <- read_xlsx(here("data/redfin/redfin-results.xlsx"),sheet = "results") %>%
  distinct() %>%
  clean_names()


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


data <- data %>%
  filter(search_id == 2)


# Distance to desired locations --------------------------------------------

locations <- readxl::read_xlsx(path = "./data/redfin/redfin-results.xlsx", sheet = "locations")

data$latlong <- paste(data$latitude, data$longitude, sep = "+")

data$full_address <- paste(gsub(" Unit.*","",data$address), data$city, data$state_or_province, sep = ", ")

total_time <- rep(0, nrow(data))

total_distance <- rep(0, nrow(data))

use_google_lat_long = T
reference_lat = 35.7304679328978
reference_lon = -78.6833089686251


# Find locations of interest ----------------------------------------------

# install and load required packages

location_queries <- read_xlsx(here("data/redfin/redfin-results.xlsx"),sheet = "location_queries")

location_query_results <- list()

for (i in location_queries$search.id) {
  
  print(paste0("Querying #1: ", location_queries$name_search[i]))
  
  # search for Walmart stores near the defined location
  first_results <- google_places(#search_string = "Walmart",
    name = location_queries$name_search[i],
    key = Sys.getenv("GOOGLE_API_KEY"),
    location = c(reference_lat, reference_lon),
    #radius = 30000, # search radius in meters (50 km)
    rankby = "distance"
    #place_type = "department_store"
  )
  
  first_res_df <- cbind(first_results$results[,c("name", "vicinity")], first_results$results$geometry$location)
  
  print(first_results$status)
  
  print(paste0("Querying #2: ", location_queries$name_search[i]))
  
  Sys.sleep(time = 3)
  
  second_results <- google_places(
    name = location_queries$name_search[i],
    key = Sys.getenv("GOOGLE_API_KEY"),
    location = c(reference_lat, reference_lon),
    #radius = 30000, # search radius in meters (50 km)
    rankby = "distance", 
    page_token = first_results$next_page_token)

  print(second_results$status)
  
  second_res_df <- cbind(second_results$results[,c("name", "vicinity")], second_results$results$geometry$location)
  
  location_query_results[[i]] <- bind_rows(first_res_df , second_res_df) %>%
    distinct(vicinity, .keep_all = T) %>%
    mutate(search.id = i) %>%
    left_join(location_queries, by = "search.id")
  
}

all_interest_locations <- do.call(rbind, location_query_results) %>%
  distinct(vicinity, .keep_all = T)


# Geocode addresses using google ------------------------------------------

ggmap::register_google(key = Sys.getenv("GOOGLE_API_KEY"))

if(use_google_lat_long) {
  google_latlong <- ggmap::geocode(location = data$full_address)
  data$google_lat_lon <- paste(google_latlong$lat, google_latlong$lon, sep="+")
}


# Calculate distances -----------------------------------------------------

for(l in locations$location_id) {
  
  l_name <- locations$location_name[l]
  
  print(paste0("Computing distance from ", l_name))
  
  l_lat_long <- rep(locations$location_lat_lon[l], nrow(data))
  l_address <- rep(locations$address[l], nrow(data))
  
  if(use_google_lat_long) {
    origins <- data$google_lat_lon
  } else {
    origins <- data$latlong
  }
  
  # It might compute distance matrices relative to multiple locations:
  
  drive_distance_results <- gmapsdistance(
    origin = origins,
    destination = l_lat_long,
    mode = "driving",
    combinations = "pairwise",
    key=Sys.getenv("GOOGLE_API_KEY")
  ) %>% as.data.frame()
  
 # distance_results <- google_distance(
 #    origins = origins,
 #    destinations = destinations,
 #    mode = "driving",
 #    units = "metric",
 #    key = Sys.getenv("GOOGLE_API_KEY")
 #  )
  
  data[,paste0("mins_to_", l_name)] <- drive_distance_results$Time.Time / 60
  
  data[,paste0("miles_to_", l_name)] <- 0.621371 * drive_distance_results$Distance.Distance / 1000
  
  total_time <- total_time + data[,paste0("mins_to_", l_name)] * locations$weight[l] %>% unname()
  
  total_distance <- total_distance + data[,paste0("miles_to_", l_name)] * locations$weight[l] %>% unname()
  
}

data$total_driving_time <- unname(total_time)

data$total_driving_distance <- unname(total_distance)

# Write results to spreadsheet:
writexl::write_xlsx(data,path = here("data/redfin/formatted-results.xlsx"))



# Read processed output ---------------------------------------------------

data <- read_xlsx(here("data/redfin/formatted-results.xlsx"))

# sold date vs price

price_plot <- data %>%
  filter(!is.na(sold_date)) %>%
  filter(!is.na(beds)) %>%
  filter(beds %in% 3:4) %>%
  filter(year(sold_date) <= year_built + 1) %>%
  filter(price <= 8e5) %>%
  ggplot(mapping = aes(x = sold_date, y = (price / 1e3))) + #/ 484)) + 
  geom_point() + 
  geom_smooth() + 
  geom_vline(xintercept = as.POSIXct(as.Date("2022-10-10")),colour="black") + 
  #geom_quantile(method = "rqss", lambda = 0.1) + 
  #geom_hline(yintercept = 450000) + 
  geom_hline(yintercept = 484) + 
  base_theme + 
  ylab("Price (thousands)") + 
  xlab("Sold date") + 
  scale_y_continuous(labels = ~paste0("$ ",.x))

plotly::ggplotly(price_plot)


# pareto frontier calculations:

pf_data <- data %>%
  #mutate(category = ifelse(!is.na(sold_date),paste0("Sold.",year(sold_date),".",quarter(sold_date)),"Not Sold")) %>%
  mutate(category = ifelse(!is.na(sold_date),paste0("Sold.",year(sold_date)),status)) %>%
  mutate(commute_time = (1*mins_to_Church)) %>% 
  mutate(benefit = -commute_time) %>%
  filter(!is.na(commute_time)) %>%
  filter(commute_time <= 100, price <= 700000) %>%
  #filter(year(sold_date) <= year_built + 1) %>%
  mutate(Strategy = row_number()) %>%
  as.data.frame()

categories.df <- data.frame(category = unique(pf_data$category)) %>%
  arrange(category) %>%
  mutate(cat.id = row_number())

pf_data <- pf_data %>%
  left_join(categories.df) %>%
  # Exclude certain addresses:
  filter(!(address %in% c("435 Diversity Way")))

pf <- pareto_frontier_by_scenario(pf_data, cost_var = "price", 
                                  benefit_var = "benefit", 
                                  epsilon = c(0,0),
                                  scenario_var = "cat.id")



pf %>%
  filter(pareto_frontier %in% c("Efficient", "Weakly dominated")) %>%
  filter(status == "Active") %>%
  mutate(location = paste(location, city, sep = ", ")) %>%
  ggplot(aes(x = price, y = commute_time, color = category, label = location)) +
  geom_point() + 
  geom_line() + 
  geom_label() + 
  geom_point(data = pf_data, alpha = 0.2) + 
  theme_rand()


# Non-Dominated homes to look at:

pareto_frontier_active_houses <- pf %>%
        filter(pareto_frontier %in% c("Efficient", "Weakly dominated")) %>%
        filter(category == "Active") %>%
        select(price, commute_time, sale_type, address, location, city, year_built, beds, square_feet, lot_size, url_see_https_www_redfin_com_buy_a_home_comparative_market_analysis_for_info_on_pricing)


# Open Redfin pages:

for(i in 1:nrow(pareto_frontier_active_houses)) {
  browseURL(pareto_frontier_active_houses$url_see_https_www_redfin_com_buy_a_home_comparative_market_analysis_for_info_on_pricing[i])  
}




# Sales per month:

sales_per_month <- data %>%
  mutate(month = paste0(year(sold_date),".", str_pad(month(sold_date), 2,pad =  "0"))) %>%
  group_by(month) %>%
  summarize(n = n(),
            value = sum(price / 1e6))%>%
  ggplot(mapping = aes(x = month, y = n)) +
  geom_path()



# driving time
data %>%
  filter(!is.na(sold_date)) %>%
  filter(!is.na(beds)) %>%
  filter(beds %in% 3:4) %>%
  filter(year(sold_date) <= year_built + 1) %>%
  filter(price <= 8e5) %>%
  ggplot(mapping = aes(x = sold_date, y = mins_to_Church)) + 
  geom_point() + 
  geom_smooth() + 
  geom_vline(xintercept = as.POSIXct(as.Date("2022-10-10")),colour="black") + 
  #geom_quantile(method = "rqss", lambda = 0.1) + 
  #geom_hline(yintercept = 450000) + 
  base_theme + 
  ylab("Driving Time to Church") + 
  xlab("Sold date")  
  #scale_y_continuous(labels = ~paste0("$ ",.x))












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


# Can we control for stuff and do a time-varying estimation:

reg_data <- data %>%
  mutate(m. = paste0(year(sold_date), ".", str_pad(month(sold_date), width = 2, pad = "0") )) %>%
  #filter(beds %in% 3:4) %>%
  # Only "new" homes - sold within 1 year of year_built
  filter(year(sold_date) <= year_built + 1) %>%
  mutate(sq_ft.250 =  (square_feet - mean(square_feet)) / 250) %>%
  mutate(beds = beds - 2)

m0 <- lm(price ~ sq_ft.250, data = reg_data)
  
m1 <- update(m0, . ~ . + beds)

m2 <- update(m1, . ~ . +  + property_type)

m3 <- update(m2, . ~ . +  + m.)

m4 <- update(m2, . ~ . + location)

reg_data$est.price <- predict(m4, newdata = reg_data)

reg_data$value <- reg_data$price - reg_data$est.price

writexl::write_xlsx(reg_data, path = "./data/reg_resuts.xlsx")

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
