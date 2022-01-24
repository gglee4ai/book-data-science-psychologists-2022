library(tidyverse)

sw <- dplyr::starwars

sw

dim(sw)
names(sw)
glimpse(sw)
sum(is.na(sw))
sum(colSums(is.na(sw)))
sum(!is.na(sw))

n_missing <- sum(is.na(sw))
n_present <- sum(!is.na(sw))

n_rows <- dim(sw)[1]
n_cols <- dim(sw)[2]
n_all <- n_rows * n_cols

prod(dim(sw))



sw %>% slice(1:10)

sw %>% slice_head(prop = .5)

sw %>% slice_sample(n = 3)

sw %>% slice_max(mass)

sw %>% select(where(is.numeric))

x <- sw %>% select(height)
y <- sw$height

str(x)
str(y)

sw %>% select(starts_with("h"))
even_col <- ((1:ncol(sw) %% 2) == 0)
even_col
sw %>% select(across(everything(), even_col))
sw %>% select(even_col)

?select_if


sw %>% select(seq(2, ncol(sw), 2))
sw %>% select_if(even_col)

sw %>% select_if(is.numeric)
sw %>% select(where(is.numeric))

sw[,sapply(sw, is.numeric)]

sws <- select(sw, name:mass, birth_year:species) 
sws

cm_to_feet <- 3.28084 / 100
sws %>% rowid_to_column() %>% mutate(id = 1:nrow(sw), height_feet = cm_to_feet * height)

age_yoda <- sws %>% 
  filter(name == "Yoda") %>% 
  pull(birth_year)

sws %>% 
  mutate(mass_pound = mass/.45,
         age_since_yoda = age_yoda - birth_year) %>%
  select(name, mass, mass_pound, birth_year, age_since_yoda)

sw %>%
  summarise(n_mass = sum(!is.na(mass)), 
            mn_mass = mean(mass, na.rm = TRUE),
            md_mass = median(mass, na.rm = TRUE),
            sd_mass = sd(mass, na.rm = TRUE),
            max_mass = max(mass, na.rm = TRUE),
            big_mass = any(mass > 1000)
  )


sw %>%
  summarise(# Descriptives of height:  
    n_height = sum(!is.na(height)), 
    mn_height = mean(height, na.rm = TRUE),
    sd_height = sd(height, na.rm = TRUE), 
    # Descriptives of mass:
    n_mass = sum(!is.na(mass)), 
    mn_mass = mean(mass, na.rm = TRUE),
    sd_mass = sd(mass, na.rm = TRUE),
    # Counts of character variables:
    n_names = n(), 
    n_species = n_distinct(species),
    n_worlds = n_distinct(homeworld)
  )


group_by(sws, species)

sws %>% 
  group_by(species) %>% 
  mutate(n_individuals = n(),
         mn_height = mean(height, na.rm = TRUE))


sws %>%
  mutate(mn_height_1 = mean(height, na.rm = TRUE)) %>%  # aggregates over ALL cases
  group_by(species) %>%
  mutate(mn_height_2 = mean(height, na.rm = TRUE)) %>%  # aggregates over current group (species)
  group_by(gender) %>%
  mutate(mn_height_3 = mean(height, na.rm = TRUE)) %>%  # aggregates over current group (gender)
  group_by(name) %>%
  mutate(mn_height_4 = mean(height, na.rm = TRUE)) %>%  # aggregates over current group (name)
  select(name, height, mn_height_1:mn_height_4)


sws %>% 
  group_by(species) %>% 
  summarise(n_individuals = n(),
            mn_height = mean(height, na.rm = TRUE),
            mn_mass = mean(mass, na.rm = TRUE)
  ) %>% 
  arrange(desc(mn_height))

sw %>% 
  group_by(hair_color, eye_color) %>% 
  count() %>% 
  arrange(desc(n))

sw %>% 
  group_by(hair_color, eye_color) %>% 
  summarize(n = n()) %>% 
  arrange()


sw %>%
  filter(species == "Human" | species == "Droid") %>%
  group_by(species) %>%
  summarise(n_cases = n(), 
            # Name length:
            mn_name_len = mean(nchar(name)), 
            sd_name_len = sd(nchar(name)),
            # Descriptives of mass (from above):
            n_mass = sum(!is.na(mass)), 
            mn_mass = mean(mass, na.rm = TRUE),
            sd_mass = sd(mass, na.rm = TRUE))

sw %>%
  select(name, height, mass) %>%
  mutate(md_height = median(height, na.rm = TRUE),
         is_tall = height > md_height) %>%
  group_by(is_tall) %>%
  summarise(n = n(),
            mn_mass = mean(mass, na.rm = TRUE),
            sd_mass = sd(mass, na.rm = TRUE))

sw_tall <- sw %>%
  select(name, height, mass) %>%
  mutate(md_height = median(height, na.rm = TRUE),
         is_tall = height > md_height) 

# All individuals:
ggplot(sw_tall, aes(x = height, y = mass)) + 
  geom_point(aes(color = is_tall), size = 2) + 
  geom_text(aes(label = name), hjust = -.2, angle = 45, size = 2, alpha = 2/3) + 
  coord_cartesian(ylim = c(0, 1700)) +
  scale_color_manual(values = c("firebrick", "steelblue", "gold")) +
  labs(title = "Individual's mass by height",
       x = "Height (in cm)", y = "Mass (in kg)") + 
  theme_classic() +
  coord_cartesian(ylim = c(0, 170))
  
# Only showing mass values from 0 to 180: 
ggplot(sw_tall, aes(x = height, y = mass)) + 
  geom_abline(aes(intercept = lm(mass ~ height)$coefficients[1], slope = lm(mass ~ height)$coefficients[2]),
              linetype = 2, col = "orange") +
  # stat_ellipse(aes(color = is_tall), alpha = .5) + 
  geom_point(aes(color = is_tall), size = 2) + 
  geom_text(aes(label = name), hjust = -.2, angle = 45, size = 2, alpha = 2/3) + 
  coord_cartesian(ylim = c(0, 170)) +
  scale_color_manual(values = c("firebrick", "steelblue", "gold")) +
  labs(title = "Individual's mass by height without outlier",
       x = "Height (in cm)", y = "Mass (in kg)") + 
  theme_classic()


## 3.3 ----

sws %>% 
  filter(homeworld == "Tatooine") %>% 
  group_by(species, gender) %>% 
  summarize(n = n(), 
            mn_height = mean(height),
            mn_mass = mean(mass, na.rm = TRUE))


sws %>% 
  filter(species == "Human") %>%
  mutate(mn_height = mean(height, na.rm = TRUE),
         taller = height > mn_height + 5) %>%
  filter(taller == TRUE)

sws %>% 
  filter(species == "Human") %>%
  group_by(gender) %>% 
  mutate(mn_height_2 = mean(height, na.rm = TRUE),
         taller_2 = height > mn_height_2 + 5) %>%
  filter(taller_2 == TRUE)

## Load data: ----- 
sw <- dplyr::starwars
# ?dplyr::starwars  # codebook of variables

## Basic data properties: ---- 
# sw     # print tibble
dim(sw)  # 87 rows (denoting individuals) x 13 columns (variables) 
#> [1] 87 14



sw %>% 
  is.na() %>% 
  sum()

colMeans(is.na(sw)) %>% round(digits = 2)

sw %>% 
  filter(species == "Human") %>% 
  group_by(gender) %>% 
  summarize(n = n())

sw %>% 
  group_by(species) %>% 
  mutate(n_gender = length(unique(gender))) %>% 
  relocate(n_gender) %>% 
  arrange(desc(n_gender))

sw %>% 
  group_by(species, gender) %>% 
  count() %>% 
  group_by(species) %>% 
  count() %>% 
  filter(n > 1)

sw %>% 
  group_by(species) %>% 
  summarize(n_gender = n_distinct(gender)) %>% 
  filter(n_gender >= 2)

sw %>% 
  group_by(homeworld) %>% 
  count() %>% 
  arrange(desc(n))
