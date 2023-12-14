# Install and load the required packages if not already installed
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

# Load the data from the first sheet of the Excel file
file_path <- './data/omicidi-relazione.xlsx'
df <- readxl::read_excel(file_path, sheet = 2, skip = 2)

# remove the last row ("Totale")
df <- df[-nrow(df), ]

# reshape the data using tidyr::gather
df_reshaped <- df %>%
  gather(key = "Year", value = "Value", -`RELAZIONE DELLA VITTIMA CON L'OMICIDA`)

# convert Year to numeric for proper ordering
df_reshaped$Year <- as.numeric(df_reshaped$Year)

# create a bar plot using ggplot2
ggplot(df_reshaped, aes(x = Year, y = Value, fill = `RELAZIONE DELLA VITTIMA CON L'OMICIDA`)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Number of Murderers by Relationship Type (2002-2021)",
       x = "Year",
       y = "Total Number",
       fill = "Relationship Type") +
  theme_minimal() +
  theme(legend.position = "top")  # Optional: Adjust legend position
