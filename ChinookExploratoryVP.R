library(dplyr)
library(ggplot2)
library(scales)
library(ggthemes)
library(MetBrewer)
library(tidyverse); theme_set(theme_bw(base_size = 14))
library(here)
library(readxl)
library(janitor)
library(ggpmisc)
library(ggrepel)
library(lubridate)
library(stringr)


setwd("C:/Users/POURFARAJV/Documents/GitHub/WCVIChinookExplrotaryVahab")
#Length data
lenDataRC<- read.csv("AgeLengthBioSamplingChinookRC.csv")
filtereddata<- lenDataRC |> 
  filter(Poh.Length.Mm <1200 & Scale.Total.Age.Yrs<6)


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


# ══════════════════════════════════════════════════════════════════════════════
# NON-ROBERTSON CATCH ANALYSIS 2026 _ Action item
# ══════════════════════════════════════════════════════════════════════════════

# ── Load & subset ─────────────────────────────────────────────────────────────
CrestDump <- read_xlsx("Biological_Data_With_Results_Chinook_April2026.xlsx",
                       sheet = "Biological_Data_With")

SubsetCrestDump <- CrestDump |>
  select(YEAR, MONTH, SAMPLE_TYPE, SUBAREA, HATCHERY_ORIGIN,
         RESOLVED_STOCK_ORIGIN, RESOLVED_STOCK_ROLLUP) |>
  filter(RESOLVED_STOCK_ORIGIN != "NA")

# ── Shared prep function ──────────────────────────────────────────────────────
prep_summary <- function(data, min_n = 0) {
  data |>
    count(YEAR, RESOLVED_STOCK_ORIGIN, RESOLVED_STOCK_ROLLUP, HATCHERY_ORIGIN) |>
    filter(n >= min_n) |>
    mutate(
      bar_group = ifelse(
        RESOLVED_STOCK_ORIGIN %in% c("Robertson Creek", "Gold River"),
        "Robertson Creek & Gold River", "Other Stocks"
      ),
      stock_label = case_when(
        RESOLVED_STOCK_ORIGIN %in% c("Robertson Creek", "Gold River")
        ~ "Robertson Creek & Gold River  (SWVI)",
        TRUE                ~ paste0(RESOLVED_STOCK_ORIGIN,
                                     "  (", RESOLVED_STOCK_ROLLUP, ")")
      )
    ) |>
    arrange(RESOLVED_STOCK_ROLLUP, RESOLVED_STOCK_ORIGIN) |>
    mutate(
      stock_label = factor(stock_label, levels = unique(stock_label)),
      bar_group   = factor(bar_group,
                           levels = c("Robertson Creek & Gold River", "Other Stocks"))
    )
}

# ── Shared theme ──────────────────────────────────────────────────────────────
catch_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title         = element_text(face = "bold", size = 13,
                                        margin = margin(b = 4)),
      plot.subtitle      = element_text(size = 10, color = "grey40",
                                        margin = margin(b = 10)),
      strip.text         = element_text(size = 12, face = "bold"),
      strip.background   = element_rect(fill = "grey92", color = NA),
      panel.spacing      = unit(2, "lines"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.x        = element_text(size = 11),
      legend.title       = element_text(face = "bold", size = 10),
      legend.text        = element_text(size = 9),
      legend.key.height  = unit(0.5, "cm"),
      plot.margin        = margin(12, 12, 12, 12)
    )
}

# ── Shared plot function ──────────────────────────────────────────────────────
catch_plot <- function(summary_data, title, subtitle = NULL) {
  ggplot(summary_data,
         aes(x = factor(YEAR), y = n, fill = stock_label)) +
    geom_col(width = 0.65) +
    facet_wrap(~ bar_group, nrow = 1, scales = "free_y") +
    scale_fill_manual(
      values = master_colours,
      name   = "Stock origin (Rollup)",
      drop   = TRUE
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(title = title, subtitle = subtitle, x = "", y = "Count") +
    catch_theme()
}

# ── Filter data ───────────────────────────────────────────────────────────────
AlberniSport <- SubsetCrestDump |>
  filter(SAMPLE_TYPE == "Sport",
         SUBAREA %in% c("23A", "23B", "23C"),
         MONTH %in% c("August", "September")) #only including ISBM

BarkleySport <- SubsetCrestDump |>
  filter(SAMPLE_TYPE == "Sport",
         SUBAREA %in% c("23D", "23E", "23F", "23K", "23M", "23J", "230", "23Q"),
         MONTH %in% c("August", "September")) #only including ISBM

CommercialFishery <- SubsetCrestDump |>
  filter(SAMPLE_TYPE %in% c("SEINE, PURSE, SALMON", "GILL NET, SALMON", "GILL NET"))

# ── Summarise all three ───────────────────────────────────────────────────────
AlberniSportSummary      <- prep_summary(AlberniSport)
BarkleySportSummary      <- prep_summary(BarkleySport, min_n = 10)
CommercialFisherySummary <- prep_summary(CommercialFishery)

# ── Build master colour palette ───────────────────────────────────────────────
# Done AFTER summaries so label strings are guaranteed to match
all_labels <- c(
  levels(AlberniSportSummary$stock_label),
  levels(BarkleySportSummary$stock_label),
  levels(CommercialFisherySummary$stock_label)
) |> unique()

ROBERTSON_COLOUR <- "#8B9DAF"   # muted slate — fixed across all plots
other_labels     <- setdiff(all_labels, "Robertson Creek & Gold River")

p1    <- tableau_color_pal("Color Blind")(10)
p2    <- tableau_color_pal("Tableau 10")(10)
p3    <- tableau_color_pal("Tableau 20")(20)

colrs <- tibble(hex = c(p1, p2, p3)[4:26]) |>
  mutate(
    r         = col2rgb(hex)[1, ] / 255,
    g         = col2rgb(hex)[2, ] / 255,
    b         = col2rgb(hex)[3, ] / 255,
    luminance = 0.299 * r + 0.587 * g + 0.114 * b
  ) |>
  filter(luminance <= 0.85) |>
  pull(hex)



master_colours <- c(
  setNames(ROBERTSON_COLOUR, "Robertson Creek & Gold River  (SWVI)"),
  setNames(colrs[seq_along(other_labels)], other_labels)
)

# ── Plots & export ────────────────────────────────────────────────────────────
catch_plot(AlberniSportSummary,
           title = "Stock of origin for fish sampled from Alberni Inlet Rec Fishery",
           subtitle = "August and September Only")
ggsave("AlberniRecCatch.png", width = 10, height = 6, dpi = 400, bg = "white")

catch_plot(BarkleySportSummary,
           title = "Stock of origin for fish sampled from Barkley Sound Rec Fishery",
           subtitle = "August and September Only")
ggsave("BarkleyRecCatch.png", width = 10, height = 6, dpi = 400, bg = "white")

catch_plot(CommercialFisherySummary,
           title    = "Stock of origin for fish sampled from Barkley Seine and Gillnet Fisheries")
ggsave("CommercialCatch.png", width = 10, height = 6, dpi = 400, bg = "white")



#############################------------ Age and Length Shift (Updated on 5/25/2026 with last year's data: Term_Area_23.xlsx)
AgeFilePath <- "Chinook age composition through time (1995-2025).xlsx"
AgeData<- read_excel(AgeFilePath, sheet = "Somass terminal run") 

#Data cleaning for % of age comp
AgeDataCleaned <- AgeData |> 
  select(year, `Somass age 2 return`:`Somass age 6 return`, `Age 2 %`:`Age 6%`) |> 
  rename(
    pct_2 = `Age 2 %`, pct_3 = `Age 3%`, pct_4 = `Age 4%`, pct_5 = `Age 5%`, pct_6 = `Age 6%`,
    count_2 = `Somass age 2 return`, count_3 = `Somass age 3 return`,
    count_4 = `Somass age 4 return`, count_5 = `Somass age 5 return`,
    count_6 = `Somass age 6 return`
  ) |>
  pivot_longer(
    cols = -year,
    names_to  = c(".value", "age"),
    names_pattern = "(pct|count)_(\\d)"
  ) |>
  mutate(
    count = as.integer(count),
    pct   = round(pct, 2)
  ) |>
  rename(percent = pct) %>% 
  filter(age != "6")

##________ Plotting
count_breaks <- round(quantile(AgeDataCleaned$count, c(0.25, 0.5, 0.75, 0.95), na.rm = TRUE), -3)

ChinookAgeCompositionPlot <- ggplot(
  AgeDataCleaned,
  aes(
    x     = as.numeric(year),
    y     = factor(age),
    size  = count,
    color = ifelse(year == 2025, "2025", "Other")
  )
) +
  geom_point(alpha = 0.6, shape = 16) +
  scale_color_manual(
    values = c("2025" = "#5b7fa6", "Other" = "#bdbdbd")
  ) +
  scale_size_area(
    max_size = 20,
    name     = "Count",
    breaks   = count_breaks,
    labels   = scales::label_comma()
  ) +
  scale_x_continuous(
    breaks       = seq(1995, 2025, by = 5),
    minor_breaks = seq(1995, 2025, by = 1),
    name         = ""
  ) +
  labs(
    title = "Temporal shift in Somass Chinook Age composition",
    y     = ""
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.6),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey85"),
    panel.grid.minor.y = element_blank(),
    axis.text          = element_text(size = 14),
    legend.position    = "right"
  ) +
  guides(
    color = "none",
    size  = guide_legend(override.aes = list(color = "#bdbdbd", alpha = 0.8))
  )
ggsave("ChinookAgeCompositionPlot95-2025.png", plot = ChinookAgeCompositionPlot, width = 12, height = 3, dpi = 300)
#-------------------------------------- Sockeye Age comp bubble plot
soxsum_path <- "Y:/WCVI/SOCKEYE/SOMASS/SOXSUM/SOXSUM_2025/Soxsum2025.xlsx"
Sockeye2025<- read_excel(soxsum_path, sheet = "2025")

#Just keep what is needed


trimmedData <- Sockeye2025 %>%
  slice(472:474) %>%      
  select(32:38) 


# Rename columns 
trimmedDataLong <- trimmedData %>%
  rename_with(~ c("Stock", "Age3.2", "Age4.2","Age4.3","Age5.2","Age5.3","Age6s"), .cols = 1:7) |> 
  pivot_longer(cols = c(2:7), names_to = "Age",values_to = "Count")
#covert from character fo factors
trimmedDataLong %>%
  mutate(across(where(is.character), as.factor))
trimmedDataLong$Count <- as.numeric(trimmedDataLong$Count) 

trimmedDataLong <- trimmedDataLong |> 
  group_by(Stock) |> 
  mutate(StockSum = sum(Count, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(AgeClassPercentage=round(Count/StockSum,3)) |> 
  mutate(Stock = factor(Stock, levels= c("Henderson Lake", "Sproat Lake",  "Great Central Lake")))


###ploting

Sockeye2025AgeComp <-  ggplot(trimmedDataLong, aes(x = Age, y = Stock, size = AgeClassPercentage)) +
  geom_point( color="#17BECF", alpha=0.6) + 
  
  geom_text(aes(label = scales::percent(AgeClassPercentage, accuracy = 0.1)),
            vjust = -0.8, size = 4, color = "grey20", check_overlap = TRUE) +
  
  scale_size_area(max_size = 34)  +
  labs(
    title = "2025 Barkley Sockeye Age composition",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 14) , # adjust font size
    legend.position= "none"
  )

setwd("Y:/WCVI/CHINOOK/CHINOOK_MGT/2025/A23/Exploratory")  

ggsave("Sockeye2025AgeComp.png", plot = Sockeye2025AgeComp, width = 8, height = 6, dpi = 300)

#-------------------------------------- Somass Sockeye Age class timeseries


# ------------------------------------------------------------
# Helper function: collapse fine age classes into brood groups
# ------------------------------------------------------------
collapse_age <- function(age) {
  case_when(
    age %in% c("Age3.2")           ~ "Age 3",
    age %in% c("Age4.2", "Age4.3") ~ "Age 4s",
    age %in% c("Age5.2", "Age5.3") ~ "Age 5s",
    age %in% c("Age6.3")           ~ "Age 6",
    TRUE                           ~ NA_character_
  )
}

# ------------------------------------------------------------
# 1) Base dataset (No aggregation yet)
# ------------------------------------------------------------
AgeTimeseriesPath <- "Y:/WCVI/SOCKEYE/SOMASS/SOCKEYE_MGMT/2025_MGT/SomassSockeyeAgeTimeseries.csv"

ageclass_base <- read_csv(AgeTimeseriesPath) |>
  select(1:4) |>
  filter(Year >= 2000, Stock=="GCL")

#-----------------------Plot of Individual stock (NOT Aggregated)


IndividualLakeAgeClassPlot<- ggplot(
  ageclass_base,
  aes(
    x = as.numeric(Year),
    y = factor(Age, levels = c("Age3.2", "Age4.2", "Age4.3", "Age5.2", "Age5.3", "Age6.3")),
    size = Count
  )
) +
  geom_point(
    alpha = 0.5,
    color = "#E15759"
  ) +
  scale_size_area(max_size = 20) +
  scale_x_continuous(
    breaks = seq(2000, 2025, by = 5),        # labeled years
    minor_breaks = seq(2000, 2025, by = 1)   # grid every year
  ) +
  labs(
    title = "Timeseries of GCL Sockeye Age class",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(
      color = "grey80",
      linewidth = 0.6
    ),
    panel.grid.minor.x = element_line(
      color = "grey90",
      linewidth = 0.3
    ),
    panel.grid.major.y = element_line(color = "grey85"),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(size = 14),
    legend.position = "none"
  )

# ------------------------------------------------------------
# 2) Somass age composition (fine ages, stocks aggregated)
#    - One row per Year × Age
#    - Used when original age detail is needed
# ------------------------------------------------------------
ageclass_somass_fine <- ageclass_base |>
  group_by(Year, Age) |>
  summarise(
    SomassCount = sum(Count, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(Year) |>
  mutate(
    SomassReturn = sum(SomassCount),
    Proportion   = SomassCount / SomassReturn
  ) |>
  ungroup()

# ------------------------------------------------------------
# 3) Somass age composition (collapsed ages) Aggregating 42 and 43 52 nad 53 and the stocks
#    - Fine ages collapsed using collapse_age()
#    - One row per Year × age_group
#    - Used for brood-year / simplified analyses
# ------------------------------------------------------------
ageclass_somass_collapsed <- ageclass_base |>
  mutate(
    age_group = collapse_age(Age)
  ) |>
  group_by(Year, age_group) |>
  summarise(
    SomassCount = sum(Count, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(Year) |>
  mutate(
    SomassReturn = sum(SomassCount),
    Proportion   = SomassCount / SomassReturn
  ) |>
  ungroup()

# ------------------------------------------------------------
# 4) Proportion validation helper
#    - Confirms proportions sum to 1 within each year
#    - Can be used on fine or collapsed datasets
# ------------------------------------------------------------
validate_proportions <- function(df) {
  df |>
    group_by(Year) |>
    summarise(
      prop_sum = sum(Proportion, na.rm = TRUE),
      .groups = "drop"
    )
}

# Example checks
validate_proportions(ageclass_somass_fine)
validate_proportions(ageclass_somass_collapsed)
#################################------------------The plots

ageclass_somass_collapsedPlot <- ggplot(
  ageclass_somass_collapsed,
  aes(
    x = as.numeric(Year),
    y = factor(
      age_group,
      levels = c("Age 3", "Age 4s", "Age 5s", "Age 6")
    ),
    size = SomassCount
  )
) +
  geom_point(
    alpha = 0.5,
    color = "#E15759"
  ) +
  scale_size_area(max_size = 20) +
  scale_x_continuous(
    breaks = seq(2000, 2025, by = 5),
    minor_breaks = seq(2000, 2025, by = 1)
  ) +
  labs(
    title = "Timeseries of Somass Sockeye Aggregated Age Classes",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.6),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey85"),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(size = 14),
    legend.position = "none"
  )



ggsave("SomassAgrregatedAgetimeseries.png", plot = ageclass_somass_collapsedPlot, width = 10, height = 3, dpi = 300)


###____________________________________CREST DATA WRANGLING
curr_year <- 2025

#set the working directory:
setwd("Y:/WCVI/CHINOOK/CHINOOK_MGT/2025/A23/Exploratory")

#Set the data location:
file_path_biodata <- "Biological_Data_With_Results_Chinook_Only (Saturday, December 6, 2025 6 22 PM).xlsx"
file_path_interview_summary <- "Interview_Summ"

#Read in the data:
bio_data <- read_excel(file_path_biodata , sheet = "Biological_Data_With")
interview_data <- read_excel(file_path_interview_summary , sheet = "Interview_Summary")


#-----------------------------GATHER BIO DATA -----------------------------------

#0) TO PULL DATA:
# A) Open CREST switchboard
# B) Click "View Reports"
# C) Select the following:
# Year: 2000 - the current year, Months: 7,8,9, Project: Barkley Sound, Drop-down menu: *Biological Data With Results Chinook Only, Click: Creel and VTSL
# D) Output to Excel - it might take an hour to run this if you are not connected to a VPN

#1) BIODATA FILE: Get the proportion of RCH for each stat week:

# Add stat_week:
bio_filtered_data <- bio_data %>%
  mutate(
    # Step 2: Convert COLLECTION_DATE to Date if not already
    COLLECTION_DATE = as.Date(COLLECTION_DATE),
    shifted_date = COLLECTION_DATE, # - days(3),                  #Shift so Thursday is week start (stat weeks begin on a Thursday and run until the next Wednesday)
    stat_month = month(shifted_date),                          #Extract month from shifted date
    stat_week_of_month = ceiling(day(shifted_date) / 7),       #Calculate week number in the month
    stat_week = stat_month * 10 + stat_week_of_month,           #Format: MM + week (e.g. 83)
    year = year(COLLECTION_DATE)   # <--- ADD THIS LINE
  )


### BY STAT WEEK:

#Filter to only logbook landings (this is just the raw Creel logbook data - not extrapolated or anything)
bio_filtered_data <- bio_filtered_data %>%
  filter(str_detect(tolower(LANDING_SITE), "logbook"))

# Step 4: Calculate % Robertson Creek Hatchery per stat_week
bio_result <- bio_filtered_data %>%
  group_by(year, stat_week) %>%
  summarise(
    total = n(),
    robertson_count = sum(str_detect(tolower(RESOLVED_STOCK_ORIGIN), "robertson creek"), na.rm = TRUE),
    percent_robertson = round((robertson_count / total), 2)) %>%
  ungroup()

# View result
print(bio_result)


#Summarize the percent RCH by statweek:
rch_by_statweek <- bio_result %>%
  group_by(stat_week) %>%
  summarise(
    avg_percent_robertson = round(mean(percent_robertson, na.rm = TRUE), 2),
    n_years = n()
  ) %>%
  arrange(stat_week)


#Print the results:
rch_by_statweek

### BY MONTH:

#Filter to only logbook landings (this is just the raw Creel logbook data - not extrapolated or anything)
bio_filtered_data <- bio_filtered_data %>%
  filter(str_detect(tolower(LANDING_SITE), "logbook"))

# Step 4: Calculate % Robertson Creek Hatchery per stat_week
bio_result <- bio_filtered_data %>%
  group_by(year, MONTH) %>%
  summarise(
    total = n(),
    robertson_count = sum(str_detect(tolower(RESOLVED_STOCK_ORIGIN), "robertson creek"), na.rm = TRUE),
    percent_robertson = round((robertson_count / total), 2)) %>%
  ungroup()


#Summarize the percent RCH by statweek:
rch_by_statweek <- bio_result %>%
  group_by(MONTH) %>%
  summarise(
    avg_percent_robertson = round(mean(percent_robertson, na.rm = TRUE), 2),
    n_years = n()
  ) %>%
  arrange(MONTH)


#Print the results:
rch_by_statweek #Note July = 7, Aug = 8, SEPT = 9



#-----------------------------INTERVIEW SUMMARY ------------------------------------

# Add stat_week:
interview_data_filtered <- interview_data %>%
  mutate(
    # Step 1: Construct full date (assuming DATE is day of month)
    full_date = as.Date(paste(YEAR, MONTH, DATE, sep = "-")),
    shifted_date = full_date, #- days(3), #shift back date to start on a Thursday
    stat_month = month(shifted_date),
    stat_week_of_month = ceiling(day(shifted_date) / 7),
    statweek = stat_month * 10 + stat_week_of_month
  )
