library(tidyverse)
library(readxl)
library(readr)
library(ggplot2)
library(dplyr)
library(countrycode)

translation_dict <- c(
  "Repubblica Ceca" = "Czech Republic",
  "Finlandia" = "Finland",
  "Italia" = "Italy",
  "Lituania" = "Lithuania",
  "Paesi Bassi" = "Netherlands",
  "Slovenia" = "Slovenia",
  "Inghilterra e Galles" = "England and Wales",
  "Scozia" = "Scotland",
  "Irlanda del Nord" = "Northern Ireland",
  "Austria" = "Austria",
  "Croazia" = "Croatia",
  "Francia" = "France",
  "Germania" = "Germany",
  "Grecia" = "Greece",
  "Islanda" = "Iceland",
  "Lettonia" = "Latvia",
  "Liechtenstein" = "Liechtenstein",
  "Lituania" = "Lithuania",
  "Malta" = "Malta",
  "Montenegro" = "Montenegro",
  "Paesi Bassi" = "Netherlands",
  "Repubblica Ceca" = "Czech Republic",
  "Romania" = "Romania",
  "Slovacchia" = "Slovakia",
  "Slovenia" = "Slovenia",
  "Spagna" = "Spain",
  "Svezia" = "Sweden",
  "Svizzera" = "Switzerland",
  "Ungheria" = "Hungary",
  "Unione Europea" = "European Union"
)


relationship_graph <- function(gender) {
  file_path <- './data/omicidi-relazione.xlsx'
  
  if (gender == 'male') {
    df <- readxl::read_excel(file_path, sheet = 2, skip = 2)
    title <- "Total Number of Murderers by Relationship Type (2002-2021, Male Victim, Italy)"
  } else {
    df <- readxl::read_excel(file_path, sheet = 3, skip = 2)
    title <- "Total Number of Murderers by Relationship Type (2002-2021, Female Victim, Italy)"
  }
  
  # remove the last row ("Total")
  df <- df[-nrow(df), ]
  
  # reshape the data using tidyr::gather
  df_reshaped <- df %>%
    gather(key = "Year", value = "Value", -`RELAZIONE DELLA VITTIMA CON L'OMICIDA`)
  
  # calculate the total number of murderers for each relationship type
  totals <- df_reshaped %>%
    group_by(`RELAZIONE DELLA VITTIMA CON L'OMICIDA`) %>%
    summarise(Total = sum(Value)) %>%
    arrange(Total)
  
  # reorder the levels of the factor based on the totals
  #df_reshaped$`RELAZIONE DELLA VITTIMA CON L'OMICIDA` <- 
    fct_relevel(df_reshaped$`RELAZIONE DELLA VITTIMA CON L'OMICIDA`, totals$`RELAZIONE DELLA VITTIMA CON L'OMICIDA`)
  
  # convert Year to numeric for proper ordering
  df_reshaped$Year <- as.numeric(df_reshaped$Year)
  
  # create a bar plot using ggplot2 with fixed y-axis scale
  ggplot(df_reshaped, aes(x = Year, y = Value, fill = `RELAZIONE DELLA VITTIMA CON L'OMICIDA`)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = title,
         x = "Year",
         y = "Total Number",
         fill = "Relationship Type") +
    theme_minimal() +
    theme(legend.position = "top") + 
    guides(fill = guide_legend(title = "Relationship Type")) +
    ylim(0, 600)  # Set the desired y-axis scale
}
relationship_graph('female')
relationship_graph('male')



# suicides graph
suicide_graph <- function() {
  # read the data from the Excel file
  file_path <- "./data/suicidi.xlsx"
  df <- read_excel(file_path, skip = 2, sheet = 1)
  
  # plotting
  ggplot(df, aes(x = Years)) +
    geom_line(aes(y = `Males`, color = "Males"), size = 1.5) +
    geom_line(aes(y = `Females`, color = "Females"), size = 1.5) +
    geom_line(aes(y = `Males+Females`, color = "Males+Females"), size = 1.5) +
    labs(x = "Year", y = "Sucides", title = "Suicide Data Over Time 1994-2015, Italy") +
    scale_color_manual(values = c("Males" = "blue", "Females" = "red", "Males+Females" = "purple")) +
    theme_minimal()
}
suicide_graph()


murders_europe_gender <- function(){
  # Read the data from the CSV file
  file_path <- "./data/dataset_eurostat.csv"
  df <- read_csv(file_path)
  
  # Filter rows for the year 2021 and "Valori per centomila abitanti"
  df_filtered <- df %>% 
    filter(Anno == 2021, `Unità` == "Valori per centomila abitanti", `Sesso della vittima` != "T")
  
  # Create a stacked bar plot
  ggplot(df_filtered, aes(x = Nazione, y = Omicidi, fill = `Sesso della vittima`)) +
    geom_bar(stat = "identity") +
    labs(x = "Country", y = "Number of Homicides", title = "Homicides by Gender and Country (2021, per 100,000 inhabitants)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
    scale_fill_manual(values = c("F" = "red", "M" = "blue"), name = "Gender of Victim")  # Specify colors and legend title
}
murders_europe_gender()




murders_europe_murderer <- function(gender, year){
  # read the data from the CSV file
  file_path <- "./data/dataset_eurostat.csv"
  df <- read_csv(file_path)
  
  # filter rows for the year 2021, "Valori per centomila abitanti", and only females
  df_filtered <- df %>% 
    filter(Anno == year, `Unità` == "Valori per centomila abitanti", `Sesso della vittima` == gender, `Sesso della vittima` != "T")
  
 
  # translate country names
  df_filtered$Nazione <- translation_dict[df_filtered$Nazione]
  
  # create a stacked bar plot based on the column "Omicida"
  ggplot(df_filtered, aes(x = Nazione, y = Omicidi, fill = Omicida)) +
    geom_bar(stat = "identity") +
    labs(x = "Country", y = paste("Number of", ifelse(gender == "F", "Female", "Male"), "Homicides"),
         title = paste(ifelse(gender == "F", "Female", "Male"), "Homicides by Relatives and Partner (", year, ", per 100.000 inhabitants)")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
    scale_fill_manual(
      values = c("Familiare" = "red", "Altro" = "blue", "Partner" = "green", "Partner o familiare" = "purple"),
      name = "Method of Homicide",
      labels = c("Familiare" = "Family", "Altro" = "Other", "Partner" = "Partner", "Partner o familiare" = "Partner or Family")
    ) + # specify colors and legend title
    ylim(0, 6)  # Set y-axis limits
}

murders_europe_murderer("F", 2021)



murders_time_series <- function() {
  # read the data from the CSV file
  file_path <- "./data/dataset_eurostat.csv"
  df <- read_csv(file_path)
  
  # filter rows for "Valori per centomila abitanti" and exclude rows where gender is "T"
  df_filtered <- df %>% 
    filter(`Unità` == "Valori per centomila abitanti", `Sesso della vittima` != "T")
  
  # filter specific countries (Italia and Unione europea) and years post-2015
  countries_to_include <- c("Italia", "Unione Europea")
  df_summed <- df_filtered %>%
    filter(Nazione %in% countries_to_include, Anno >= 2015) %>%
    group_by(Nazione, Anno) %>%
    summarise(Total_Murders = sum(Omicidi))
  
  # create a time series plot with legend
  ggplot(df_summed, aes(x = Anno, y = Total_Murders, group = Nazione, color = Nazione)) +
    geom_line(size = 1.5) +
    labs(x = "Year", y = "Total Murders", title = "Total Murders committed by Partner or Relative (2015-2021, per 100.000 inhabitants)") +
    theme_minimal() +
    theme(legend.position = "bottom") +  # Show legend at the bottom
    scale_color_manual(values = setNames(scales::hue_pal()(length(unique(df_summed$Nazione))), unique(df_summed$Nazione)))  # Set colors and legend names
}

murders_time_series()





murders_map <- function(year) {
  # read the data from the CSV file
  file_path <- "./data/dataset_eurostat.csv"
  df <- read.csv(file_path, check.names = FALSE)  # Use check.names = FALSE to preserve column names
  
  # filter rows for "Valori per centomila abitanti" and exclude rows where gender is "T" and select data for 2021
  df_filtered <- df %>% 
    filter(`Unità` == "Valori per centomila abitanti", `Sesso della vittima` != "T", Anno == year)
  
  # group by country and year, then calculate the total murders
  df_summed <- df_filtered %>%
    group_by(Nazione) %>%
    summarise(Total_Murders = sum(Omicidi))

  
  # replace Italian names with English names in the df_summed data frame
  df_summed$Nazione <- translation_dict[as.character(df_summed$Nazione)]
  
  # get world map data
  world_map <- map_data("world")
  
  # define bounding box coordinates for Europe
  europe_bbox <- c(-20, 35, 40, 70)
  
  # filter the world map data to include only European countries
  europe_map_data <- subset(world_map, long >= europe_bbox[1] & long <= europe_bbox[3] & lat >= europe_bbox[2] & lat <= europe_bbox[4])
  
  # merge with the total murders data
  europe_map_data <- left_join(europe_map_data, df_summed, by = c("region" = "Nazione"))
  
  # create a map with a red gradient for better category differentiation
  ggplot(europe_map_data) +
    geom_map(aes(map_id = region, fill = Total_Murders), map = europe_map_data, color = "grey") +
    expand_limits(x = europe_map_data$long, y = europe_map_data$lat) +
    scale_fill_gradientn(
      colors = c("#fbeacb","#e6313d", "#a20f1f", "#321054"),
      na.value = "white",
      name = "Total Murders"
    ) +
    theme_void() +
    labs(title = paste("Total Partner or Relatives committed Murders by Country (Europe, per 100,000 inhabitants),", year))
}

murders_map(2021)





