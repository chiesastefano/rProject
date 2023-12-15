library(tidyverse)
library(readxl)
library(readr)
library(ggplot2)
library(dplyr)




#male graph relationship with the victim trough the years
# load the data from the first sheet of the Excel file


relationship_graph <- function(gender) {
file_path <- './data/omicidi-relazione.xlsx'

if (gender == 'male') {
  df <- readxl::read_excel(file_path, sheet = 2, skip = 2)
  title <- "Total Number of Murderers by Relationship Type (2002-2021, Male Victim)"
} else {
  df <- readxl::read_excel(file_path, sheet = 3, skip = 2)
  title <- "Total Number of Murderers by Relationship Type (2002-2021, Female Victim)"
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

# Reorder the levels of the factor based on the totals
df_reshaped$`RELAZIONE DELLA VITTIMA CON L'OMICIDA` <- 
  fct_relevel(df_reshaped$`RELAZIONE DELLA VITTIMA CON L'OMICIDA`, totals$`RELAZIONE DELLA VITTIMA CON L'OMICIDA`)

# convert Year to numeric for proper ordering
df_reshaped$Year <- as.numeric(df_reshaped$Year)

# create a bar plot using ggplot2
ggplot(df_reshaped, aes(x = Year, y = Value, fill = `RELAZIONE DELLA VITTIMA CON L'OMICIDA`)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = title,
       x = "Year",
       y = "Total Number",
       fill = "Relationship Type") +
  theme_minimal() +
  theme(legend.position = "top") + 
  guides(fill = guide_legend(title = "Relationship Type"))
}





# suicides graph
suicide_graph <- function() {
  # Read the data from the Excel file
  file_path <- "./data/suicidi.xlsx"
  df <- read_excel(file_path, skip = 2, sheet = 1)
  
  # Plotting
  ggplot(df, aes(x = Years)) +
    geom_line(aes(y = `Males`, color = "Males"), size = 1) +
    geom_line(aes(y = `Females`, color = "Females"), size = 1) +
    geom_line(aes(y = `Males+Females`, color = "Males+Females"), size = 1) +
    labs(x = "Year", y = "Sucides", title = "Suicide Data Over Time 1994-2015, Italy") +
    scale_color_manual(values = c("Males" = "blue", "Females" = "red", "Males+Females" = "purple")) +
    theme_minimal()
}



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





murders_europe_murderer <- function(gender, year){
  # Read the data from the CSV file
  file_path <- "./data/dataset_eurostat.csv"
  df <- read_csv(file_path)
  
  # Filter rows for the year 2021, "Valori per centomila abitanti", and only females
  df_filtered <- df %>% 
    filter(Anno == year, `Unità` == "Valori per centomila abitanti", `Sesso della vittima` == gender, `Sesso della vittima` != "T")
  
  # Create a stacked bar plot based on the column "Omicida"
  ggplot(df_filtered, aes(x = Nazione, y = Omicidi, fill = Omicida)) +
    geom_bar(stat = "identity") +
    labs(x = "Country", y = paste("Number of", ifelse(gender == "F", "Female", "Male"), "Homicides"),
         title = paste(ifelse(gender == "F", "Female", "Male"), "Homicides by Method and Country (", year, ", per 100,000 inhabitants)")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
    scale_fill_manual(
      values = c("Familiare" = "red", "Altro" = "blue", "Partner" = "green", "Partner o familiare" = "purple"),
      name = "Method of Homicide",
      labels = c("Familiare" = "Family", "Altro" = "Other", "Partner" = "Partner", "Partner o Familiare" = "Partner or Family")
    ) + # Specify colors and legend title
   ylim(0, 6)  # Set y-axis limits
}




murders_time_series <- function() {
  # Read the data from the CSV file
  file_path <- "./data/dataset_eurostat.csv"
  df <- read_csv(file_path)
  
  # Filter rows for "Valori per centomila abitanti" and exclude rows where gender is "T"
  df_filtered <- df %>% 
    filter(`Unità` == "Valori per centomila abitanti", `Sesso della vittima` != "T")
  
  # Filter specific countries (Italia and Unione europea) and years post-2015
  countries_to_include <- c("Italia", "Unione Europea")
  df_summed <- df_filtered %>%
    filter(Nazione %in% countries_to_include, Anno >= 2015) %>%
    group_by(Nazione, Anno) %>%
    summarise(Total_Murders = sum(Omicidi))
  
  # Create a time series plot with legend
  ggplot(df_summed, aes(x = Anno, y = Total_Murders, group = Nazione, color = Nazione)) +
    geom_line() +
    labs(x = "Year", y = "Total Murders", title = "Total Murders committed by Partner or Relative (2015-2021)") +
    theme_minimal() +
    theme(legend.position = "bottom") +  # Show legend at the bottom
    scale_color_manual(values = setNames(scales::hue_pal()(length(unique(df_summed$Nazione))), unique(df_summed$Nazione)))  # Set colors and legend names
}
