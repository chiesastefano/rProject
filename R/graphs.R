# Install and load the required packages if not already installed
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)
library(readxl)
library(ggplot2)





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




