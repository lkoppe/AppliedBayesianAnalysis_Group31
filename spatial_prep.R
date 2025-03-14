

# Samuel's fancy magic
# ==============================================================================
attribute_names <- readLines("data/communities.names")
attribute_names <- gsub(pattern = "@attribute ", replacement = "", 
                        # grepl: returns logical values for matches
                        # extract lines starting with @attribute only 
                        attribute_names[grepl("@attribute", attribute_names)])

## split the string wrt the blank space and use 1st element
attribute_names <- sapply(strsplit(attribute_names, " "), FUN = `[`, 1)
data <- read.csv("data/communities.data", 
                 header = FALSE, 
                 na.strings = "?", # ? as a NA
                 col.names = attribute_names # assign column names 
)
data$ViolentCrimesPerPop <- pmin(pmax(data$ViolentCrimesPerPop, 1e-6), 1 - 1e-6)
unique_states <- sort(unique(data$state))
new_state_ids <- setNames(seq_along(unique_states), unique_states)
data$statenew <- new_state_ids[as.character(data$state)]

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

comm_code <- read.delim("data/communitycodes.txt")
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

# we need the latitude and longitude of all our Californian cities
cal_cities <- read.csv("data/cal_cities_lat_long.csv")
cal_cities$Name <- gsub("\\s+", "", cal_cities$Name)
names(cal_cities)[1] <- "communityname" # for merging

# differences between new dataset and our comm_code
comm_code$communityname[51] <- "LaCañadaFlintridge"
comm_code$communityname[59] <- "SouthSanFrancisco"
comm_code$communityname[105] <- "GroverBeach"

# I realized that the names are just different in our main dataset
which(data$communityname == "GroverCity")
# 764
data$communityname[764] <- "GroverBeach"
which(data$communityname == "SouthSanFranciscodivision")
# 439
data$communityname[439] <- "SouthSanFrancisco"
which(data$communityname == "LaCanadaFlintridge")
# 376
data$communityname[376] <- "LaCañadaFlintridge"

# merging
merged_cali <- comm_code %>%
  left_join(cal_cities, by = c("communityname"), suffix = c("", "_ref"))

# missing data in the merged dataset
merged_cali[which(is.na(merged_cali$Longitude) == TRUE),1]
# I looked these up
# Danville 37.822578 -122.000839
# Paradise 39.752079 -121.621971
# LosGatos 37.221340 -121.979637
# Hillsborough 37.565159 -122.363487
# AppleValley 34.523930 -117.216927
# Benicia 38.053928 -122.155571

# latitude first, then longitude
merged_cali[which(is.na(merged_cali$Longitude) == TRUE),7] <- 
  c(37.822578, 39.752079, 37.221340, 37.565159, 34.523930, 38.053928)
merged_cali[which(is.na(merged_cali$Longitude) == TRUE),8] <- 
  c(-122.000839, -121.621971, -121.979637, -122.363487, -117.216927, -122.155571)

# ==============================================================================
data = data %>%
  filter(state == 6) %>%
  select(all_of(c(ten_features, "state", "communityname", "ViolentCrimesPerPop"))) %>%
  right_join(merged_cali)

# transform lat and lon to [0,1]
minLat = min(data$Latitude)
minLon = min(data$Longitude)
maxLat = max(data$Latitude)
maxLon = max(data$Longitude)

# Scale Latitude and Longitude to [0, 1]
data <- data %>%
  mutate(
    Latitude_scaled = (Latitude - min(Latitude)) / (max(Latitude) - min(Latitude)),
    Longitude_scaled = (Longitude - min(Longitude)) / (max(Longitude) - min(Longitude))
  )

