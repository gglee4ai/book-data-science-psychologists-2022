## R basics | ds4psy
## Gyeong-Geun LEE | 2022 January 20
## -------------------------------------

## Preparations: --------


# load additional packages for this session:
library(tidyverse)
library(ds4psy)

## Topic: ------


ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy, color = class))

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy), color = "steelblue", size = 3)


# Define a color palette (as a vector): 
my_colors <- c("steelblue", "gold", "firebrick", "forestgreen")

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy, color = factor(cyl)), size = 3) +
  scale_color_manual(values = my_colors)  # use the manual color palette my_colors 


my_palette <- c("red3", "white", "black", "blue")
ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy, color = factor(cyl)), size = 3) +
  scale_color_manual(values = my_palette)



# Jittering the position of points: 
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(color = "steelblue4", alpha = 1/3, size = 2, 
             position = "jitter") +
  theme_classic()

# Using geom_jitter:
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_jitter(color = "steelblue4", alpha = 1/3, size = 2) +
  theme_classic()






ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(color = unikn::Seeblau, linetype = 1) +
  geom_point(shape = 21, color = "black", fill = "steelblue4", alpha = 1/2, size = 2) + 
  labs(title = "Fuel usage by engine displacement", 
       x = "Engine displacement (in liters)", y = "Miles per gallon (on highway)", 
       caption = "Data from ggplot2::mpg") +
  theme_ds4psy()




ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(color = unikn::Seeblau, linetype = 1) +
  geom_point(mapping = aes(fill = class), shape = 21, color = "black", alpha = 1/2, size = 2) + 
  labs(title = "Fuel usage by engine displacement", 
       x = "Engine displacement (in liters)", y = "Miles per gallon (on highway)", 
       caption = "Data from ggplot2::mpg") +
  theme_ds4psy()


ggplot(mpg, aes(x = cty)) +
  geom_histogram(binwidth = 2)


ggplot(mpg, aes(cty)) +
  geom_histogram(binwidth = 2, fill = unikn::pal_petrol[[1]], color = "black") +
  labs(title = "Distribution of fuel economy",
       x = "Miles per gallon (in city)", y = "Frequency",
       caption = "Data from ggplot2::mpg") +
  theme_ds4psy(col_title = unikn::Petrol, col_bgrnd = "lightgrey", col_brdrs = "black")

library(ds4psy)


ggplot(mpg) +
  geom_bar(aes(x = class, y = ..count..), stat = "count")

ggplot(mpg) +
  geom_bar(aes(class, fill = class, color = "black")) +
  labs(title = "counts of cars by class",
       x = "Class of car", y = "Frequency", fill = "Class:") +
  scale_fill_manual(values = unikn::usecol(unikn::pal_unikn_light)) +
  theme_ds4psy()


ggplot(mpg) +
  geom_bar(aes(class, y = ..prop.., group = 1), fill = unikn::Seeblau)


ggplot(mpg) +
  geom_bar(aes(class, y = ..count../sum(..count..), group = 1), fill = unikn::Seeblau)

library(tidyverse)
de_org <- tibble(
  party = c("CDU/CSU", "SPD", "Others"),
  share_2013 = c((.341 + .074), .257, (1 - (.341 + .074) - .257)), 
  share_2017 = c((.268 + .062), .205, (1 - (.268 + .062) - .205))
)
de_org$party <- factor(de_org$party, levels = c("CDU/CSU", "SPD", "Others"))  # optional

de <- de_org %>%
  gather(share_2013:share_2017, key = "election", value = "share") %>%
  separate(col = "election", into = c("dummy", "year")) %>%
  select(year, party, share)

de_org %>% 
  pivot_longer(share_2013:share_2017, values_to = "share") %>% 
  separate(name, into = c("dummy", "year"), sep = "_") %>% 
  select(year, party, share) %>% 
  arrange(year, party)


de

de$year <- parse_integer(de$year)

bp_1 <- ggplot(de, aes(year, share, fill = party)) +
  geom_bar(stat = "identity", position = "dodge", color = "black")
bp_1


bp_1 +
  geom_text(aes(label = paste0(round(share * 100, 1), "%"), y = share + .015),
            position = position_dodge(width = 1),
            fontface = 2,
            color = "black"
            ) +
  scale_fill_manual(name = "Party:", values = c("black", "red3", "gold")) + 
  # Titles and labels: 
  labs(title = "Partial results of the German general elections 2013 and 2017", 
       x = "Year of election", y = "Share of votes", 
       caption = "Data from www.bundeswahlleiter.de.") + 
  # coord_flip() + 
  theme_bw()

bp_1 + 
  ## prettier plot: 
  geom_text(aes(label = paste0(round(share * 100, 1), "%"), y = share + .015), 
            position = position_dodge(width = 1), 
            fontface = 2, color = "black")


bp_1 <- ggplot(de, aes(x = year, y = share, fill = party)) +
  ## (A) 3 bars per election (position = "dodge"):  
  geom_bar(stat = "identity", position = "dodge", color = "black") # 3 bars next to each other
bp_1

bp_1 + 
  ## prettier plot: 
  geom_text(aes(label = paste0(round(share * 100, 1), "%"), y = share + .015), 
            position = position_dodge(width = 1), 
            fontface = 2, color = "black") + 
  # Some set of high contrast colors: 
  scale_fill_manual(name = "Party:", values = c("black", "red3", "gold")) + 
  # Titles and labels: 
  labs(title = "Partial results of the German general elections 2013 and 2017", 
       x = "Year of election", y = "Share of votes", 
       caption = "Data from www.bundeswahlleiter.de.") + 
  # coord_flip() + 
  theme_bw()



## (a) minimal version: 
bp_2 <- ggplot(de, aes(x = year, y = share, fill = party)) +
  ## (B) 1 bar per election (position = "stack"):
  geom_bar(stat = "identity", position = "stack") # 1 bar per election
bp_2

bp_2 + 
  ## prettier plot: 
  geom_text(aes(label = paste0(round(share * 100, 1), "%")), 
            position = position_stack(vjust = .5),
            color = rep(c("white", "white", "black"), 2), # vary text color 
            fontface = 2) + 
  # Some set of high contrast colors: 
  scale_fill_manual(name = "Party:", values = c("black", "red3", "gold")) + 
  # Titles and labels: 
  labs(title = "Partial results of the German general elections 2013 and 2017", 
       x = "Year of election", y = "Share of votes", 
       caption = "Data from www.bundeswahlleiter.de.") + 
  # coord_flip() + 
  theme_classic()


n_cat <- 6
set.seed(101)

df <- tibble(
  name = LETTERS[1:n_cat],
  value = sample(seq(25, 50), n_cat),
  sd = rnorm(n = n_cat, mean = 0, sd = 8)
)

ggplot(df) +
  geom_bar(aes(name, value), stat = "identity", fill = unikn::pal_karpfenblau[[1]]) +
  geom_errorbar(aes(name, ymin = value - sd, ymax = value + sd),
                width = 0.3, color = unikn::Pinky, size = 1) +
  labs(title = "Bar plot with error bars")


ggplot(df) +
  geom_bar(aes(name, value), stat = "identity", fill = unikn::pal_seegruen[[1]]) +
  geom_linerange(aes(name, ymin = value - sd, ymax = value + sd),
                color = unikn::Bordeaux, size = 2) +
  labs(title = "Bar plot with error bars") +
  theme_ds4psy()


ggplot(df) +
  geom_bar(aes(name, value), stat = "identity") +
  geom_crossbar(aes(name, value, ymin = value - sd, ymax = value + sd),
                width = 0.2)


# (d) pointrange: 
ggplot(df) +
  geom_bar(aes(x = name, y = value), stat = "identity", fill = unikn::pal_seeblau[[2]]) +
  geom_pointrange(aes(x = name, y = value, ymin = value - sd, ymax = value + sd), 
                  color = unikn::Bordeaux, size = 1) +
  labs(title = "Bar plot with point ranges") + 
  theme_ds4psy()


otrees <- as_tibble(datasets::Orange)
otrees

ggplot(otrees) +
  geom_line(aes(age, circumference, group = Tree)) +
  labs(title = "Growth of orange trees") +
  theme_bw()


ggplot(otrees, aes(age, circumference, group = Tree, color = Tree)) +
  geom_line(size = 1.5, alpha = 2/3) +
  geom_point(size = 3, alpha = 2/3) +
  labs(title = "Growth of orange trees") +
  theme_bw()


cw <- as_tibble(ChickWeight)
ggplot(cw) +
  geom_line(aes(Time, weight, group = Chick, color = Chick)) +
  facet_wrap(~Diet)

ggplot(cw, aes(Time, weight, Diet)) +
  facet_wrap(~Diet) +
  geom_point(alpha = 1/2) +
  geom_line(aes(group = Chick)) +
  geom_smooth(aes(color = Diet)) +
  labs(title = "Chick weight by time for different diets", 
       x = "Time (number of days)", y = "Weight (in gm)",
       caption = "Data from datasets::ChickWeight.") +
  theme_bw()


ggplot(cw, aes(x = Time, fill = Diet)) +
  geom_bar(position = "dodge") +
  labs(title = "Number of chicks per diet over time", x = "Time (number of days)", y = "Number", 
       caption = "Data from datasets::ChickWeight.") +
  theme_bw()
