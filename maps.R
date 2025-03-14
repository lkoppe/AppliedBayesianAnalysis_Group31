
rm(list = ls())

# Load libraries
library(ggplot2)
library(dplyr)
library(maps)
library(mapdata)
library(ggrepel)

# Step 1: Load the dataset -----------------------------------------
source("loadData.R")


# Step 2: Summarize number of communities per state
# length(unique(data$state)) # 46 states
community_summary <- data %>%
  filter(!is.na(state)) %>% # filters non-NA data rows only
  group_by(state) %>%
  summarise(num_communities = n_distinct(communityname)) %>%
  ungroup()

head(community_summary, 10)
# length(unique(community_summary$state))


# Step 2.5: Determine which states exist and how they are encoded ---------
# sort(unique(data$state))
# c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
#   "Connecticut", "Delaware", "Washington D.C.", "Florida", "Georgia", "Idaho", 
#   "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
#   "Massachusetts", "Minnesota", "Mississippi", "Missouri", "Nevada", 
#   "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", 
#   "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
#   "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", 
#   "Virginia", "Washington", "West Virigina", "Wisconsin", "Wyoming")
# no Hawaii, no Illinois, no Michigan, no Montana, no Nebraska

# Step 3: Map state codes to state names ----------------------------
state_codes <- data.frame(
  
  state = sort(unique(data$state)),
  
  # to lower case letters
  region = tolower(c("alabama", "alaska", "arizona", "arkansas", "california", "colorado",
                     "connecticut", "delaware", "district of columbia", "florida", "georgia", 
                     "idaho", "indiana", "iowa", "kansas", "kentucky", "louisiana", "maine", 
                     "maryland", "massachusetts", "minnesota", "mississippi", "missouri", "nevada", 
                     "new hampshire", "new jersey", "new mexico", "new york", "north carolina", 
                     "north dakota", "ohio", "oklahoma", "oregon", "pennsylvania", "rhode island", 
                     "south carolina", "south dakota", "tennessee", "texas", "utah", "vermont", 
                     "virginia", "washington", "west virginia", "wisconsin", "wyoming"))
) # 46

head(state_codes, 10)

# Merge state_codes with the community_summary
# both having common column "state" with unique entries
community_summary <- left_join(community_summary, # 46 x 2
                               state_codes,       # 46 x 2
                               by = "state")      # --> 46 x 3
head(community_summary)


# Step 4: Calculate relative percentages -------------------------
total_communities <- sum(community_summary$num_communities, na.rm = TRUE) #1994

# add column of relative frequency of community (in percentage)
# 46 × 4
community_summary <- community_summary %>%
  mutate(relative_percentage = round((num_communities / total_communities) * 100, 2))
head(community_summary)



# Step 5: Prepare state map data ------------------------------
#> Provides the geographical data needed to plot the map for usa (its states) 
state_map <- map_data("state") 

# Merge state map with the community summary
# region is the common column in both data set and is used to join them
state_map <- left_join(state_map, community_summary, by = c("region" = "region"))

# Step 6: Calculate state centroids for labels -----------------------
# Calculate the centroid for each region
state_centroids <- state_map %>%
  group_by(region) %>%
  summarise(long = mean(range(long)), lat = mean(range(lat)),
            label = paste0(region, "\n", relative_percentage, "%"), # State name + percentage
            num_communities = first(num_communities)) %>%
  filter(!is.na(num_communities))  # Remove states with no data

# Step 7: Plot the map with relative values and state names -------------
ggplot() +
  # Base map with fill based on relative percentage
  geom_polygon(data = state_map, 
               aes(x = long, y = lat, group = group, fill = relative_percentage), 
               color = "white") +
  # Add state names and percentages at centroids
  geom_text(data = state_centroids, aes(x = long, y = lat, label = label),
            color = "black", size = 3, fontface = "bold", lineheight = 0.9) +
  # Customize color palette
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", 
                      na.value = "gray90", 
                      name = "Relative %") +
  theme_minimal() +
  labs(title = "Relative Number of Communities Represented in Each State",
       subtitle = "States filled based on the relative percentage of communities in the dataset",
       x = "Longitude", y = "Latitude") +
  theme(legend.position = "right")

# regression variables that exhibit strong correlation with our target variable
# PctKids2Par -- percentage of kids in family housing with two parents
# (there are other variables similar to this one, but we shouldn't pick more 
# than one or multicollinearity)
# PctImmigRec10 -- percentage of _immigrants_ who immigated within last 10 years
# PctPopUnderPov -- percentage of people under the poverty level
# medFamInc -- median family income
# racePctWhite -- percentage of population that is caucasian
# (the others for blacks and so on will also be correlated)

# I made this dataframe so we can easily see which number belongs to which state
lookup_table <- data.frame(
  original_fips = unique_states,
  new_id = seq_along(unique_states),
  name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
           "Connecticut", "Delaware", "Washington D.C.", "Florida", "Georgia", "Idaho", 
           "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
           "Massachusetts", "Minnesota", "Mississippi", "Missouri", "Nevada", 
           "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", 
           "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
           "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", 
           "Virginia", "Washington", "West Virigina", "Wisconsin", "Wyoming")
)

########################### Map of California ########################

# community data
comm_code <- read.delim("data/communitycodes.txt")
# head(comm_code)

#Since, state code of California = 6
# state_codes[state_codes$state == 6, ]
comm_code$state <- rep(6, 278)


# remove unnecessary addage
data$communityname <- gsub("city", "", data$communityname)
data$communityname <- gsub("town", "", data$communityname)
data$communityname <- gsub("-MorongoValle", "", data$communityname)

# merge the data and replace NAs for California
merged_data <- data %>%
  left_join(comm_code, by = c("communityname", "state"), suffix = c("", "_ref")) %>%
  mutate(
    county = ifelse(is.na(county), county_ref, county),
    community = ifelse(is.na(community), community_ref, community)
  ) %>%
  select(-ends_with("_ref"))
# head(merged_data, 10)[,1:6]

# we need the latitude and longitude of all our Californian cities
cal_cities <- read.csv("data/cal_cities_lat_long.csv")
cal_cities$Name <- gsub("\\s+", "", cal_cities$Name) # removing white spaces
names(cal_cities)[1] <- "communityname" # for merging

# differences between new dataset and our comm_code
comm_code$communityname[51] <- "LaCañadaFlintridge"
comm_code$communityname[59] <- "SouthSanFrancisco"
comm_code$communityname[105] <- "GroverBeach"

# merging
merged_cali <- comm_code %>%
  left_join(cal_cities, by = c("communityname"), suffix = c("", "_ref"))

# Response and Group variables only
datax <- data %>%
  select(communityname, ViolentCrimesPerPop) # 1994 x 2

merged_cali <- merged_cali %>%
  left_join(datax, by = c("communityname"))

# missing data in the merged dataset
merged_cali[which(is.na(merged_cali$Longitude) == TRUE),1]
# I looked these up
# Danville 37.822578 -122.000839
# Paradise 39.752079 -121.621971
# LosGatos 37.221340 -121.979637
# Hillsborough 37.565159 -122.363487
# AppleValley 34.523930 -117.216927
# Benicia 38.053928 -122.155571

# latitude first, then longitude  ???????? 
merged_cali[which(is.na(merged_cali$Longitude) == TRUE),7] <- 
  c(37.822578, 39.752079, 37.221340, 37.565159, 34.523930, 38.053928)
merged_cali[which(is.na(merged_cali$Longitude) == TRUE),8] <- 
  c(-122.000839, -121.621971, -121.979637, -122.363487, -117.216927, -122.155571)

ca_map <- map_data("county", "california")


## alternative : the More Crime, The darker the centroids -------
ggplot() +
  geom_polygon(data = ca_map, aes(x = long, y = lat, group = group), 
               fill = "gold", color = "black") +
  geom_point(data = merged_cali, aes(x = Longitude, y = Latitude, color = ViolentCrimesPerPop),
             size = 2) +
  geom_text_repel(data = merged_cali, aes(x = Longitude, y = Latitude, 
                                          label = communityname), 
                  size = 3, color = "black", max.overlaps = 10) +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  coord_fixed(1.3) +
  labs(title = "Map of Californian Communities", 
       x = "Longitude", y = "Latitude") +
  theme_minimal()


# for presentation -------------------------------------------------------------

state_map_modified <- state_map %>%
  left_join(data %>% 
              select(state,ViolentCrimesPerPop) %>%
              group_by(state) %>%
              summarise(mn = mean(ViolentCrimesPerPop)))
            

map_crime_rates <- ggplot() +
  geom_polygon(data = state_map_modified, 
               aes(x = long, y = lat, group = group, fill = mn), 
               color = "white") +
  scale_fill_gradient(low = "darkgreen", high = "red", 
                      na.value = "gray90", 
                      name = "Stand. Crime Rate") +
  theme_bw() +
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(color = "grey20", size = 10),
        axis.text.y = element_text(color = "grey20", size = 10),  
        axis.title.x = element_text(color = "grey20", size = 20),
        axis.title.y = element_text(color = "grey20", size = 20))

ggsave(plot = map_crime_rates, filename = "ImagesForPresentation/map_crime_rates.png")

