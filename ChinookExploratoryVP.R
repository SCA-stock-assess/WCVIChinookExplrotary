library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)
#Length data
lenDataRC<- read.csv("AgeLengthBioSamplingChinookRC.csv")
filtereddata<- lenDataRC |> 
  filter(Poh.Length.Mm <1200 & Scale.Total.Age.Yrs<7)


avgPohDF<- filtereddata |> 
  group_by( Spawning.Year, Scale.Total.Age.Yrs  ) |> 
  summarise(avgPOH= mean(Poh.Length.Mm, na.rm = TRUE), n=n()) 

avgPohDF$Scale.Total.Age.Yrs <- factor(avgPohDF$Scale.Total.Age.Yrs)

POHFig<- ggplot(avgPohDF, aes(Spawning.Year, avgPOH, color=Scale.Total.Age.Yrs )) +geom_point(size=3) + geom_line() +
  xlab("") + ylab("POH in mm") + scale_color_brewer(palette="Set1") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=15))

#################### 2025 Run reconstruction results 
df <- tribble(
  ~fishery, ~age2, ~age3, ~age4, ~age5, ~age6,
  "FSC/Treaty", 0, 1073, 692, 168, 0,
  "First Nation Comm", 0, 16182, 7871, 1727, 0,
  "Test", 1, 492, 423, 104, 0,
  "Alberni Inlet REC", 498, 3428, 2195, 529, 0,
  "Barkley Sound REC", 164, 6449, 5905, 1492, 33,
  "Commercial GN", 43, 10223, 5909, 1335, 0,
  "Commercial SN", 237, 4801, 2902, 982, 0,
  "Escapement", 12273, 24117, 17928, 3600, 0
)

df_long <- df |>
  pivot_longer(starts_with("age"),
               names_to = "age",
               values_to = "count")
#1) Stacked bar chart 
df_totals <- df_long |>
  group_by(fishery) |>
  summarise(total = sum(count), .groups = "drop")

ggplot(df_long, aes(x = fishery, y = count, fill = age)) +
  geom_col() +
  geom_text(
    data = df_totals,
    aes(x = fishery, y = total, label = scales::comma(total)),
    hjust = -0.1,
    size = 3.5,
    inherit.aes = FALSE) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Somass River system Age Composition by Fishery - Table 3",
    x = "Fishery",
    y = "Count",
    fill = "Age"
  ) +
  theme_minimal()


# ---------------------------
# Create dataset - TOTAL CATCH - AREA 23
# ---------------------------
df <- tribble(
  ~origin, ~location, ~age_2, ~age_3, ~age_4, ~age_5, ~age_6,
  
  "Hatchery", "ROBERTSON", 847, 38788, 23555, 5795, 29,
  "Natural",  "ROBERTSON", 97, 3859, 2343, 543, 5,
  
  "Hatchery", "OTHER AREA 23", 9, 308, 214, 50, 1,
  "Natural",  "OTHER AREA 23", 7, 314, 228, 61, 0,
  
  "Hatchery", "CONUMA", 12, 654, 678, 150, 3,
  "Natural",  "CONUMA", 13, 310, 202, 67, 0,
  
  "Hatchery", "OTHER AREA 25", 1, 127, 119, 27, 1,
  "Natural",  "OTHER AREA 25", 0, 29, 22, 4, 0,
  
  "Hatchery", "NITINAT", 0, 6, 8, 2, 0,
  "Natural",  "NITINAT", 0, 1, 1, 0, 0,
  
  "Hatchery", "OTHER WCVI", 82, 3914, 2708, 707, 4,
  "Natural",  "OTHER WCVI", 5, 81, 73, 23, 0,
  
  "Hatchery", "NON-WCVI", 113, 3803, 3518, 647, 16,
  "Natural",  "NON-WCVI", 0, 275, 134, 29, 0
)

# ---------------------------
# Reshape to long format
# ---------------------------
df_long <- df |>
  pivot_longer(
    starts_with("age"),
    names_to = "age",
    values_to = "count"
  )
df_longRob <- df_long %>% 
  filter(location=="ROBERTSON")
# ---------------------------
# Total per group (for labels)
# ---------------------------
df_totals <- df_long |>
  group_by(origin, location) |>
  summarise(total = sum(count), .groups = "drop")
df_totalsRob <- df_totals %>% 
  filter(location=="ROBERTSON")
# ---------------------------
# Plot: stacked bar + totals
# ---------------------------
ggplot(df_longRob , aes(x = location, y = count, fill = age)) +
  geom_col() +
  geom_text(
    data = df_totalsRob,
    aes(x = location, y = total, label = comma(total)),
    hjust = -0.1,
    size = 3.5,
    inherit.aes = FALSE
  ) +
  facet_wrap(~ origin) +
  #coord_flip() +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Age Composition by Location and Origin",
    x = "Location",
    y = "Count",
    fill = "Age"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )
############## Escapement
# Create a data frame for Total Escapement - Somass River and RCH
escapement_somass <- tibble::tibble(
  type = c(
    "Hatchery - otolith + ad (cwt)",
    "Hatchery - CWT",
    "Natural"
  ),
  age_2 = c(11425, 10639, 847),
  age_3 = c(19472, 21019, 4645),
  age_4 = c(14587, 10326, 3341),
  age_5 = c(1819, 2207, 1781),
  age_6 = c(NA, NA, NA)
)
escapement_somassLong <- pivot_longer(
  escapement_somass,  
    starts_with("age"),
    names_to = "age",
    values_to = "count"
  )
  

# plot of escapement
escapement_somassLong %>% 
  ggplot(aes(x=type, y=count, fill = age)) +
  geom_col() +
  labs(
    title = "Somass River and RCH escapement age composition",
    x = "",
    y = "Count",
    fill = "Age"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )
  
