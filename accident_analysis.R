library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)
library(scales)
library(tidyr)

u_red = "#CC0000"
grey_background = "#f4f4f0ff"
  

# Load and combine data ---------------------------------------------------------------
## TODO : 

ad <- read_csv("~/Github/climbing-accidents/_github-AAC_accidents_tagged_data.csv") %>% 
  clean_names() %>% 
  mutate(word_count_report = sapply(strsplit(text, "\\s+"), length))


# 1. Season:
#   Determine the season of the accident based on the month columns.

ad <- ad %>%
  mutate(
    season = case_when(
      january == 1 | february == 1 | december == 1 ~ "Winter",
      march == 1 | april == 1 | may == 1 ~ "Spring",
      june == 1 | july == 1 | august == 1 ~ "Summer",
      september == 1 | october == 1 | november == 1 ~ "Fall",
      TRUE ~ "Unknown"
    )
  )


# 2. Severity:
#   Categorize the accidents as "Deadly," "Serious," or "Minor" based on the corresponding columns.

ad <- ad %>%
  mutate(
    severity = case_when(
      deadly == 1 ~ "Deadly",
      serious == 1 ~ "Serious",
      minor == 1 ~ "Minor",
      TRUE ~ "Unknown"
    )
  )


# 3. Climber Experience:
#   Summarize experience levels into a single variable.

ad <- ad %>%
  mutate(
    experience_level = case_when(
      no_little == 1 ~ "No/Little Experience",
      moderate == 1 ~ "Moderate Experience",
      experienced == 1 ~ "Experienced",
      unknown == 1 ~ "Unknown"
    )
  )


# 4. Activity Type:
#   Create a broader categorization of climbing activity based on tags.

ad <- ad %>%
  mutate(
    activity_type = case_when(
      alpine_mountaineering == 1 ~ "Alpine/Mountaineering",
      sport == 1 ~ "Sport Climbing",
      trad_climbing == 1 ~ "Trad Climbing",
      top_rope == 1 ~ "Top Rope",
      ice_climbing == 1 ~ "Ice Climbing",
      bouldering == 1 ~ "Bouldering",
      ski_related == 1 ~ "Skiing",
      non_climbing == 1 ~ "Non-Climbing",
      TRUE ~ "Other"
    )
  )


# 5. Error Type:
#   Summarize common accident causes into categories.

ad <- ad %>%
  mutate(
    error_type = case_when(
      belay_error == 1 ~ "Belay Error",
      rappel_error == 1 ~ "Rappel Error",
      lowering_error == 1 ~ "Lowering Error",
      knot_tie_in_error == 1 ~ "Knot/Error",
      no_backup_or_end_knot == 1 ~ "Backup/Error",
      anchor_failure_error == 1 ~ "Anchor Failure",
      gear_broke == 1 ~ "Gear Failure",
      object_dropped_dislodged == 1 ~ "Falling Object",
      inadequate_protection_pulled == 1 ~ "Protection Pulled",
      inadequate_equipment == 1 ~ "Inadequate Equipment",
      TRUE ~ "Other"
    )
  )


# 6. Environmental Factors:
#   Combine weather or natural hazards into a single variable.

ad <- ad %>%
  mutate(
    environmental_factor = case_when(
      avalanche == 1 ~ "Avalanche",
      severe_weather == 1 ~ "Severe Weather",
      natural_rockfall == 1 ~ "Rockfall",
      poor_cond_seasonal_risk == 1 ~ "Poor Conditions",
      visibility == 1 ~ "Low Visibility",
      TRUE ~ "None"
    )
  )

  
  
# Analyze sport vs. trad accidents ----------------------------------------

# Sport vs. trad accidents over the years
ad %>%
  # filter(is.na(inadequate_protection_pulled)) %>% 
  filter(activity_type %in% c("Sport Climbing", "Trad Climbing")) %>% # Filter for Sport and Trad climbing activities
  group_by(publication_year, activity_type) %>% # Group by year and activity type
  summarise(count = n(), .groups = "drop") %>% # Count the number of accidents per type per year
  ggplot(aes(x = publication_year, y = count, fill = activity_type)) +
  geom_col(position = 'dodge') +
  geom_text(aes(label = count), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, color = "black") +
  scale_y_continuous(labels = scales::comma_format()) +
  theme_classic() +
  labs(
    title = "Sport vs. Trad Climbing Activities Over the Years",
    x = "Year",
    y = "Number of Activities",
    fill = "Activity Type"
  )



# 1. Severity Comparison ----------------------------------------------------
ad %>%
  filter(activity_type %in% c("Trad Climbing", "Sport Climbing")) %>%
  group_by(activity_type, severity) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = severity, y = count, fill = activity_type)) +
  geom_col(position = "dodge") +
  labs(
    title = "Severity Levels: Trad vs. Sport Climbing",
    x = "Severity",
    y = "Number of Incidents",
    fill = "Activity Type"
  ) +
  theme_classic()

# 2. Experience Level Analysis ----------------------------------------------
ad %>%
  filter(activity_type == "Trad Climbing") %>%
  group_by(experience_level) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = experience_level, y = count, fill = experience_level)) +
  geom_col() +
  labs(
    title = "Experience Levels in Trad Climbing Accidents",
    x = "Experience Level",
    y = "Number of Accidents"
  ) +
  theme_classic()

# 3. Error Type Contribution ------------------------------------------------
ad %>%
  filter(activity_type == "Trad Climbing") %>%
  group_by(error_type) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count)) %>%
  ggplot(aes(x = reorder(error_type, count), y = count, fill = error_type)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Common Error Types in Trad Climbing",
    x = "Error Type",
    y = "Number of Accidents"
  ) +
  theme_classic()

# 4. Environmental Factors Comparison ---------------------------------------
ad %>%
  filter(activity_type %in% c("Trad Climbing", "Sport Climbing")) %>%
  group_by(activity_type, environmental_factor) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = environmental_factor, y = count, fill = activity_type)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Environmental Factors: Trad vs. Sport Climbing",
    x = "Environmental Factor",
    y = "Number of Incidents",
    fill = "Activity Type"
  ) +
  theme_classic()

# 5. Seasonality ------------------------------------------------------------
ad %>%
  filter(activity_type == "Trad Climbing") %>%
  group_by(season) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = season, y = count, fill = season)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Seasonality of Trad Climbing Accidents",
    x = "Season",
    y = "Number of Accidents"
  ) +
  theme_classic()

# 6. Accident Causes Over Time ----------------------------------------------
ad %>%
  filter(activity_type == "Trad Climbing") %>%
  group_by(publication_year, error_type) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = publication_year, y = count, color = error_type)) +
  geom_line(size = 1) +
  labs(
    title = "Error Trends in Trad Climbing Over the Years",
    x = "Year",
    y = "Number of Accidents",
    color = "Error Type"
  ) +
  theme_classic()

# 7. Severity vs. Experience ------------------------------------------------
ad %>%
  filter(activity_type == "Trad Climbing") %>%
  group_by(severity, experience_level) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = severity, y = count, fill = experience_level)) +
  geom_col(position = "dodge") +
  labs(
    title = "Severity vs. Experience in Trad Climbing",
    x = "Severity",
    y = "Number of Accidents",
    fill = "Experience Level"
  ) +
  theme_classic()



ad %>%
  filter(activity_type == "Trad Climbing") %>%
  select(poor_cond_seasonal_risk:miscommunication) %>% # Select contributing factor columns
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "factor", values_to = "count") %>%
  arrange(desc(count)) %>%
  ggplot(aes(x = reorder(factor, count), y = count, fill = factor)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Individual Contributions to Trad Climbing Accidents",
    x = "Contributing Factor",
    y = "Number of Accidents"
  ) +
  theme_classic()


ad %>%
  filter(activity_type == "Trad Climbing") %>%
  select(poor_cond_seasonal_risk:miscommunication) %>%
  mutate(row_id = row_number()) %>% # Create a unique row identifier for joining
  pivot_longer(cols = poor_cond_seasonal_risk:miscommunication, names_to = "factor_1", values_to = "present_1") %>%
  filter(present_1 == 1) %>%
  select(row_id, factor_1) %>%
  left_join(
    ad %>%
      filter(activity_type == "Trad Climbing") %>%
      select(poor_cond_seasonal_risk:miscommunication) %>%
      mutate(row_id = row_number()) %>%
      pivot_longer(cols = poor_cond_seasonal_risk:miscommunication, names_to = "factor_2", values_to = "present_2") %>%
      filter(present_2 == 1) %>%
      select(row_id, factor_2),
    by = "row_id"
  ) %>%
  group_by(factor_1, factor_2) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = factor_1, y = factor_2, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(
    title = "Co-occurrence of Contributing Factors in Trad Climbing Accidents",
    x = "Factor 1",
    y = "Factor 2",
    fill = "Count"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ad %>%
  filter(activity_type == "Trad Climbing") %>%
  select(severity, poor_cond_seasonal_risk:miscommunication) %>%
  pivot_longer(cols = poor_cond_seasonal_risk:miscommunication, names_to = "factor", values_to = "present") %>%
  filter(present == 1) %>%
  group_by(severity, factor) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = factor, y = count, fill = severity)) +
  geom_col(position = "stack") +
  coord_flip() +
  labs(
    title = "Severity by Contributing Factor in Trad Climbing Accidents",
    x = "Contributing Factor",
    y = "Number of Accidents",
    fill = "Severity"
  ) +
  theme_classic()



# Analysis of "Inadequate Protection Pulled" in Trad Climbing Accidents


# 1. Severity of Accidents with "Inadequate Protection Pulled" -----------------
ad %>%
  filter(activity_type == "Trad Climbing", inadequate_protection_pulled == 1) %>%
  group_by(severity) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = severity, y = count, fill = severity)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Severity of Accidents with Inadequate Protection Pulled",
    x = "Severity",
    y = "Number of Accidents"
  ) +
  theme_classic()

# 2. Experience Level in Accidents with "Inadequate Protection Pulled" ----------
ad %>%
  filter(activity_type == "Trad Climbing", inadequate_protection_pulled == 1) %>%
  group_by(experience_level) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = experience_level, y = count, fill = experience_level)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Experience Level in Accidents with Inadequate Protection Pulled",
    x = "Experience Level",
    y = "Number of Accidents"
  ) +
  theme_classic()

# 3. Environmental Factors in Accidents with "Inadequate Protection Pulled" -----
ad %>%
  filter(activity_type == "Trad Climbing", inadequate_protection_pulled == 1) %>%
  select(avalanche:miscommunication) %>% # Select relevant environmental factor columns
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "environmental_factor", values_to = "count") %>%
  arrange(desc(count)) %>%
  ggplot(aes(x = reorder(environmental_factor, count), y = count, fill = environmental_factor)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Environmental Factors in Accidents with Inadequate Protection Pulled",
    x = "Environmental Factor",
    y = "Number of Accidents"
  ) +
  theme_classic()

# 4. Trends Over Time for "Inadequate Protection Pulled" ------------------------
ad %>%
  filter(activity_type == "Trad Climbing", inadequate_protection_pulled == 1) %>%
  group_by(publication_year) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = publication_year, y = count)) +
  geom_line(size = 1, color = "blue") +
  geom_point(size = 2, color = "red") +
  labs(
    title = "Trends Over Time for Inadequate Protection Pulled",
    x = "Year",
    y = "Number of Accidents"
  ) +
  theme_classic()

# 5. Joint Analysis of Severity and Experience Level ----------------------------
ad %>%
  filter(activity_type == "Trad Climbing", inadequate_protection_pulled == 1) %>%
  group_by(severity, experience_level) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = severity, y = count, fill = experience_level)) +
  geom_col(position = "dodge") +
  labs(
    title = "Severity vs. Experience Level in Inadequate Protection Pulled",
    x = "Severity",
    y = "Number of Accidents",
    fill = "Experience Level"
  ) +
  theme_classic()




# GPT text analysis -------------------------------------------------------

#protection pulled
pp <- ad %>% filter(inadequate_protection_pulled == 1) %>% 
  select(id, text) %>% 
  mutate(
    Gear_Type_Involved = NA,  # New column for "Gear Type Involved in Incident"
    Gear_Placement_Quality = NA,  # New column for "Gear Placement Quality"
    Causes_of_Gear_Failure = NA,  # New column for "Causes of Gear Failure"
    Lessons_for_Placing_Good_Gear = NA  # New column for "Lessons for Placing Good Gear"
  )

# Split the dataset into N files with 10 rows each
split_size <- 10  # Number of rows per split
n_files <- ceiling(nrow(pp) / split_size)  # Calculate the number of split files

# Create directory to save split files (if it does not already exist)
split_dir <- "~/Github/climbing-accidents/protection_pulled/"
dir.create(split_dir, showWarnings = FALSE)

# Split and save the files
for (i in 1:n_files) {
  start_row <- (i - 1) * split_size + 1
  end_row <- min(i * split_size, nrow(pp))
  split_data <- pp[start_row:end_row, ]
  split_file_path <- file.path(split_dir, paste0("incident_data_part_", i, ".csv"))
  write.csv(split_data, split_file_path, row.names = FALSE)
}


Objective: You are provided with a CSV containing trad climbing accident reports. For each report, complete the missing columns by categorizing the incident according to the specified categories and tags. You will be filling in as many rows as possible given memory constraints. Please read the report carefully to ensure the tags are correct.

Instructions:
  Input CSV Structure: The CSV file contains the following columns:

  Incident Report: A description of a climbing incident.
Gear Type Involved in Incident: (Fill in with applicable tags)
Gear Placement Quality: (Fill in with applicable tags)
Causes of Gear Failure: (Fill in with applicable tags)
Lessons for Placing Good Gear: (Fill in with applicable tags)
Tag Lists:

  Gear Type Involved in Incident: Cam, Nut, Hex, Tricam, Anchor Sling/Webbing, Carabiner, Rope/Rope System, Anchor System
Gear Placement Quality: Shallow Placement, Poor Surface Contact, Undercamming, Overcammed, Improper Angle, Walking Gear, Flared Crack, Rotational Instability
Causes of Gear Failure: Directional Misalignment, Rock Quality, Rope Drag, Insufficient Redundancy, Gear Size Mismatch, Load Dynamics, Poor Crack Assessment, Improper Extension
Lessons for Placing Good Gear: Assess Rock Quality, Correct Size & Fit, Alignment with Fall Direction, Redundancy, Use Extenders, Avoid Flared Placements, Test Stability, Understand Load Dynamics
How to Fill In the Data:

  Gear Type Involved in Incident: Identify the type(s) of gear mentioned in the report and tag accordingly.
Gear Placement Quality: Identify any mention of issues with the quality of the gear placement. Select all applicable tags.
Causes of Gear Failure: Identify reasons why the gear failed and assign relevant tags.
Lessons for Placing Good Gear: Identify the key lessons that can be learned from the report and assign tags accordingly.
Format: The data should be returned in CSV format with completed rows, maintaining the same structure as the input file.

