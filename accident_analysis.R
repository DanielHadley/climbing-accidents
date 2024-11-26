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





# Text analysis -----------------------------------------------------------


library(tidytext)

# Sample incident reports data (replace with your actual data)
incident_data <- ad %>% 
  filter(inadequate_protection_pulled ==1) %>% 
  mutate(incident_description = text)

# Define keywords for each category
tags <- list(
  gear_type = c(
    "cam", "nut", "hex", "tricam", "anchor sling", "webbing", "carabiner", "rope", "anchor system"
  ),
  placement_quality = c(
    "shallow placement", "poor surface contact", "undercamming", "overcammed", "improper angle", 
    "walking gear", "flared crack", "rotational instability"
  ),
  gear_failure = c(
    "directional misalignment", "rock quality", "rope drag", "insufficient redundancy", "gear size mismatch", 
    "load dynamics", "poor crack assessment", "improper extension"
  )
)

# Function to create tags based on keywords
create_tags <- function(text, tags) {
  text_lower <- tolower(text)
  sapply(tags, function(keywords) {
    any(sapply(keywords, function(keyword) str_detect(text_lower, keyword)))
  })
}

# Apply the function to each row and create new columns for tags
incident_tags <- incident_data %>%
  rowwise() %>%
  mutate(
    cam = create_tags(incident_description, tags$gear_type)["cam"],
    nut = create_tags(incident_description, tags$gear_type)["nut"],
    hex = create_tags(incident_description, tags$gear_type)["hex"],
    tricam = create_tags(incident_description, tags$gear_type)["tricam"],
    anchor_sling_webbing = create_tags(incident_description, tags$gear_type)["anchor sling"],
    carabiner = create_tags(incident_description, tags$gear_type)["carabiner"],
    rope = create_tags(incident_description, tags$gear_type)["rope"],
    anchor_system = create_tags(incident_description, tags$gear_type)["anchor system"],
    shallow_placement = create_tags(incident_description, tags$placement_quality)["shallow placement"],
    poor_surface_contact = create_tags(incident_description, tags$placement_quality)["poor surface contact"],
    undercamming = create_tags(incident_description, tags$placement_quality)["undercamming"],
    overcammed = create_tags(incident_description, tags$placement_quality)["overcammed"],
    improper_angle = create_tags(incident_description, tags$placement_quality)["improper angle"],
    walking_gear = create_tags(incident_description, tags$placement_quality)["walking gear"],
    flared_crack = create_tags(incident_description, tags$placement_quality)["flared crack"],
    rotational_instability = create_tags(incident_description, tags$placement_quality)["rotational instability"],
    directional_misalignment = create_tags(incident_description, tags$gear_failure)["directional misalignment"],
    rock_quality = create_tags(incident_description, tags$gear_failure)["rock quality"],
    rope_drag = create_tags(incident_description, tags$gear_failure)["rope drag"],
    insufficient_redundancy = create_tags(incident_description, tags$gear_failure)["insufficient redundancy"],
    gear_size_mismatch = create_tags(incident_description, tags$gear_failure)["gear size mismatch"],
    load_dynamics = create_tags(incident_description, tags$gear_failure)["load dynamics"],
    poor_crack_assessment = create_tags(incident_description, tags$gear_failure)["poor crack assessment"],
    improper_extension = create_tags(incident_description, tags$gear_failure)["improper extension"]
  ) %>%
  ungroup()

# View the tagged data
print(incident_tags)

# Save the tagged data to a new CSV file
write_csv(incident_tags, "tagged_incident_data.csv")

