install.packages(c('gtools', 'readr', 'ggplot2', 'tidyr', 'choroplethr', 'usmap', 'lessR', 'shiny','tidyverse'))

library(shiny)
library(dplyr)
library(tidyr)
library(tidyverse)
library(usmap)
library(ggplot2)
library(RColorBrewer)
library(lessR)

setwd("C:/Users/SafaY/repos\\R\ Programming\\R\ CW")

##############################################################################################################
# LOAD DATA
##############################################################################################################
message("Loading Dataset")
hospital_data <- read.csv(file="hospitals.csv", sep=",", head=TRUE)
rate_data <- read.csv(file="standard-charges.csv", sep=",", head=TRUE)

##############################################################################################################
# DATA CLEANING
##############################################################################################################
message("Cleaning Dataset")
hospital_data <- hospital_data %>%
  mutate(mrf_url = na_if(mrf_url, ""))

hospital_data <- hospital_data %>%
  mutate(control_type = na_if(control_type, ""))

rate_data <- rate_data %>%
  mutate(Procedure.Code = na_if(Procedure.Code, NA))

##############################################################################################################
# DATA MANIPULATION
##############################################################################################################
message("Manipulating Dataset")
# Separate control_type to ownership and affiliation
hospital_data$control_type <- gsub("FOR PROFIT", "FORPROFIT", hospital_data$control_type)
hospital_data$control_type <- gsub(" - ", " ", hospital_data$control_type)
hospital_data <- hospital_data %>%
  separate(control_type, into = c("ownership", "affiliation"), sep = " ", remove = TRUE)
hospital_data$ownership[is.na(hospital_data$ownership)] <- "Undetermined"
hospital_data$affiliation[is.na(hospital_data$affiliation)] <- "Undetermined"

# Separate medicate_termination_status to medicare_type and termination_reason
hospital_data <- hospital_data %>%
  separate(medicare_termination_status, into = c("medicare_type", "termination_reason"), sep = ", ", remove = TRUE)

##############################################################################################################
# DATA FILTERING
##############################################################################################################
message("Filtering Dataset")

# Remove irrelevant columns
hospital_data <- select(hospital_data, -termination_reason)
hospital_data <- select(hospital_data, -ein, -alt_name, -system_name, -addr, -phone, last_updated, -file_name, -permalink, -transparency_page, -additional_notes, -last_updated)

# Remove adder charges
rate_data <- rate_data %>%
  filter(!is.na(Procedure.Code))

# Remove hospital domain specific words
words_to_remove <- c("US", "(ea)", "(pr)", "(cpt)") 
for (word in words_to_remove) {
  rate_data$Bill.Description <- gsub(paste0("\\b", word, "\\b"), "", rate_data$Bill.Description, ignore.case = TRUE)
}

# Remove long words so its more user friendly
rate_data <- rate_data %>%
  filter(str_count(Bill.Description, "\\w+") <= 2)

##############################################################################################################
# BASIC ANALYSIS AND VISUALIZATION
##############################################################################################################
# Question 1
message("Q1: How many hospitals are there in each state according to the dataset??")

# Count the number of hospitals in each state
state_hospitalcount <- hospital_data %>%
  group_by(state) %>%
  summarise(hospital_count = n())

# Plot graph
plot_usmap(data = state_hospitalcount, values = "hospital_count", regions = "states", color = "blue") + 
  scale_fill_continuous(low = "white", high = "blue", name = "Quantity of Hospitals", label = scales::comma) + 
  labs(title = "U.S. States by Amount of Hospitals",
       subtitle = "Color intensity shows the amount of hospitals in each state") + 
  theme(legend.position = "right")

# Question 2
message("Q2: Which states have the most transparent hospital pricing?")

# Calculate transparent hospital percentage by pricing page availability
state_transparentWebsiteCount <- hospital_data %>%
  group_by(state) %>%
  filter(!is.na(mrf_url)) %>%
  summarise(numberOfTransparentHospitals = n())
head(state_transparentWebsiteCount)

state_website_ratio <- left_join(state_hospitalcount, state_transparentWebsiteCount, by = "state") %>%
  mutate(website_percentage = numberOfTransparentHospitals / hospital_count) %>%
  select(state, website_percentage)

# Plot graph
plot_usmap(data = state_website_ratio, values = "website_percentage", regions = "state", color = "darkgreen") +
  scale_fill_continuous(low = "white", high = "darkgreen", name = "Quantity of Transparent Hospitals", label = scales::percent_format()) +
  labs(title = "U.S. States by Normalized Ratio of Transparent Hospitals",
       subtitle = "Color intensity shows the normalized ratio of hospitals with transparent pricing") +
  theme(legend.position = "right")

# Question 3
message("Q3: How transparent are the hospitals, and are there variations based on ownership types (e.g., government, nonprofit, for-profit)?")

# Calculate transparency by ownership type
ownership_quantity <- hospital_data %>%
  group_by(ownership) %>%
  summarise(total_quantity = n()) %>%
  select(ownership, total_quantity)

ownership_quantity_transparent <- hospital_data %>%
  filter(!is.na(mrf_url)) %>%
  group_by(ownership) %>%
  summarise(transparent_quantity = n()) %>%
  select(ownership, transparent_quantity)

ownership_data <- left_join(ownership_quantity, ownership_quantity_transparent, by = "ownership") %>%
  filter(total_quantity >= 3) %>% # Remove small sample size biases
  mutate(transparency_percentage = (transparent_quantity / total_quantity)) %>%
  arrange(desc(transparency_percentage))

# Plot graph
ggplot(ownership_data, aes(x = "", y = transparency_percentage, fill = ownership)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = scales::percent(transparency_percentage), y = 1), color = "black", size = 3) +  coord_polar(theta = "y") +
  facet_grid(~ownership) +
  labs(title = "Percentage of Transparency by Ownership Type",
       x = NULL,
       y = "Transparency amount shaded for each ownership type",
       fill = "Ownership Type") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")

# Question 4
message("Q4: What are the cost ranges of different operations and how do they relate to each other?")

# Create shiny UI
ui <- fluidPage(selectInput(inputId = "operation_type",
                            label = "Operation Name:",
                            choices = setNames(rate_data$Bill.Description, rate_data$Bill.Description)),
                actionButton(inputId = "add_button", label = "Add Operation"),
                plotOutput(outputId = "operationplot"),
                textOutput(outputId = "total_price")
)

# Create shiny back end
server <- function(input, output) {
  selected_operations <- reactiveVal(NULL)
  
  observeEvent(input$add_button, {
    if (!is.null(input$operation_type)) {
      selected_operations(c(selected_operations(), input$operation_type))
    }
  })
  
  # Create Graph
  output$operationplot <- renderPlot({
    if (!is.null(selected_operations())) {
      selected_data <- rate_data %>%
        filter(Bill.Description %in% selected_operations())
      
      if (n_distinct(selected_data$Current.Charge) > 1) {
        ggplot(selected_data, aes(x = Bill.Description, y = Current.Charge, label = sprintf("$%.2f", Current.Charge))) +
          geom_line(aes(group = 2), size = 1) +
          geom_point(size = 3, color = "black") +
          geom_text(vjust = -0.5, size = 5) +
          theme_minimal()
      } else {
        ggplot(selected_data, aes(x = Bill.Description, y = Current.Charge, label = sprintf("$%.2f", Current.Charge))) +
          geom_point(size = 5, color = "black") +
          geom_text(vjust = -0.5, size = 5) +
          theme_minimal()
      }
    } else {
      plot(NULL, xlim = c(1, 2), ylim = c(0, 1), main = "Add operations to see the cost comparison below")
    }
  })
  
  # Calculate total sum of rate charges
  output$total_price <- renderText({
    if (!is.null(selected_operations())) {
      selected_data <- rate_data %>%
        filter(Bill.Description %in% selected_operations())
      
      if (n_distinct(selected_data$Current.Charge) > 1) {
        message("More than 1 price exists")
        total_sums <- selected_data %>%
          group_by(Bill.Description) %>%
          summarize(min_price = min(Current.Charge), max_price = max(Current.Charge))
        price_range <- sprintf("$%.2f - $%.2f", sum(total_sums$min_price), sum(total_sums$max_price))
        paste("Total Price: ", as.character(price_range))
      } else {
        total_price <- sum(selected_data$Current.Charge)
        paste("Total Price: $", sprintf("%.2f", total_price))
      }
    } else {
      "Total Price: $0.00"
    }
  })
}

shinyApp(ui=ui, server=server)