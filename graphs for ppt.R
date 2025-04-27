# install.packages("readr")  # uncomment if you haven’t already installed readr
library(readr)

# 1. Read the CSV into a data frame
df <- read_csv("Efficiency_Markets_Access.csv")

# 2. Print the column names
print(colnames(df))

# Create histogram for PPT -----------
# install.packages("ggplot2")  # if you haven’t already
library(ggplot2)

# Assuming df is already loaded:
# df <- readr::read_csv("Efficiency_Market_Access.csv")

p <- ggplot(df, aes(x = `Efficiency Score`)) +
  geom_histogram(
    fill   = "darkorange",
    color  = "black",
    bins   = 30
  ) +
  # Add count labels above bars, using the new after_stat syntax
  stat_bin(
    geom   = "text",
    bins   = 30,
    aes(label = after_stat(count)),
    vjust  = -0.5
  ) +
  # Expand the y‐axis so the top label isn’t clipped
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Histogram of Efficiency Score",
    x     = "Efficiency Score",
    y     = "Count"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    # Center & bold title
    plot.title       = element_text(hjust = 0.5, face = "bold")
  )

# Display
print(p)

# Save with transparent background
ggsave(
  "hist_efficiency_score.png",
  plot = p,
  bg   = "transparent",
  width  = 6,
  height = 4
)

# Market structure box plot ------
# Required libraries
library(ggplot2)
library(dplyr)
library(forcats)

# 2. Re‐order the Market Structure factor
df$`Market Structure` <- factor(
  df$`Market Structure`,
  levels = c(
    "Vertically Integrated Utility (VIU)",
    "Single Buyer Model (SBM)",
    "Wholesale-Retail Competition"
  )
)

# 3. Compute counts for each group
counts <- df %>%
  group_by(`Market Structure`) %>%
  summarise(n = n(), .groups = "drop")

# 4. Build the box‐plot with larger labels
p <- ggplot(df, aes(x = `Market Structure`, y = `Efficiency Score`)) +
  geom_boxplot(fill  = "darkorange", color = "black") +
  # mean marker with legend key
  stat_summary(
    aes(shape = "Mean"),
    fun     = mean,
    geom    = "point",
    size    = 4,
    fill    = "white",
    stroke  = 1
  ) +
  # median marker with legend key
  stat_summary(
    aes(shape = "Median"),
    fun     = median,
    geom    = "point",
    size    = 4,
    fill    = "black"
  ) +
  # add count labels above each box, with increased text size
  geom_text(
    data = counts,
    aes(
      x     = `Market Structure`,
      y     = max(df$`Efficiency Score`) * 1.05,
      label = n
    ),
    vjust = 0,
    size  = 5  # increase count label size
  ) +
  # expand y‐axis so nothing is cut off
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  # define shapes and legend title
  scale_shape_manual(
    name   = "Statistic",
    values = c(Mean = 23, Median = 21)
  ) +
  guides(
    shape = guide_legend(override.aes = list(fill = c("white", "black")))
  ) +
  labs(
    title = "Efficiency Score by Market Structure",
    x     = "Market Structure",
    y     = "Efficiency Score"
  ) +
  theme_minimal() +
  theme(
    # transparent backgrounds
    panel.background  = element_rect(fill = "transparent", color = NA),
    plot.background   = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key        = element_rect(fill = "transparent", color = NA),
    # larger, bold title
    plot.title        = element_text(hjust = 0.5, face = "bold", size = 16),
    # larger axis titles
    axis.title        = element_text(size = 14),
    # larger axis text (tick labels)
    axis.text         = element_text(size = 12),
    # larger legend text
    legend.title      = element_text(size = 12),
    legend.text       = element_text(size = 11),
    legend.position   = "top"
  )

# 5. Display
print(p)

# 6. Save with transparent background
ggsave(
  "boxplot_efficiency_market_structure.png",
  plot = p,
  bg   = "transparent",
  width  = 6,
  height = 4
)



# GOVERNANCE GRAPHS ------
# 1. Load and prepare your data
df <- read_csv("Efficiency_Markets_Access.csv") %>%
  filter(!is.na(`Efficiency Score`)) %>%
  mutate(
    Regulator                 = factor(Regulator,                 levels = c(TRUE, FALSE)),
    `Private IPP Established` = factor(`Private IPP Established`, levels = c(TRUE, FALSE)),
    Unbundling                = factor(Unbundling,                levels = c(TRUE, FALSE))
  )

# 2. Pivot to long format
df_long <- df %>%
  pivot_longer(
    cols      = c(Regulator, `Private IPP Established`, Unbundling),
    names_to  = "Category",
    values_to = "Value"
  )

# 3. Compute counts & max y
counts <- df_long %>% count(Category, Value, name = "n")
max_y   <- max(df_long$`Efficiency Score`, na.rm = TRUE)

# 4. Define alternating panel fill colors
panel_colors <- c(
  "Regulator"                 = "white",
  "Private IPP Established"   = "gray",
  "Unbundling"                = "darkgray"
)

# 5. Build the faceted box‐plot with shaded backgrounds
p <- ggplot(df_long, aes(x = Value, y = `Efficiency Score`)) +
  # shaded background rectangles
  geom_rect(
    data = data.frame(Category = names(panel_colors)),
    aes(fill = Category, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    alpha = 0.1, inherit.aes = FALSE
  ) +
  # the boxplots
  geom_boxplot(fill = "darkorange", color = "black") +
  # mean and median points
  stat_summary(aes(shape = "Mean"), fun = mean, geom = "point", size = 4, fill = "white", stroke = 1) +
  stat_summary(aes(shape = "Median"), fun = median, geom = "point", size = 4, fill = "black") +
  # count labels
  geom_text(
    data = counts,
    aes(x = Value, y = max_y * 1.05, label = n),
    vjust = 0,
    size  = 5
  ) +
  # facets side by side
  facet_grid(. ~ Category) +
  # scales and guides
  scale_fill_manual(values = panel_colors) +
  guides(fill = "none",
         shape = guide_legend(override.aes = list(fill = c("white", "black")))) +
  scale_shape_manual(name   = "Statistic",
                     values = c(Mean = 23, Median = 21)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  # labels
  labs(
    title = "Efficiency Score by Regulator, IPP Status, and Unbundling",
    x     = NULL,
    y     = "Efficiency Score"
  ) +
  # transparent minimal theme
  theme_minimal() +
  theme(
    panel.background  = element_rect(fill = "transparent", color = NA),
    plot.background   = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key        = element_rect(fill = "transparent", color = NA),
    plot.title        = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title        = element_text(size = 14),
    axis.text         = element_text(size = 12),
    strip.text        = element_text(size = 14),
    legend.position   = "top",
    legend.title      = element_text(size = 12),
    legend.text       = element_text(size = 11)
  )

# 6. Display
print(p)

# 7. Save with transparent background
ggsave(
  "boxplots_governance_with_shading.png",
  plot = p,
  bg   = "transparent",
  width  = 9,
  height = 4
)

## country level -------

# 1. Load your data (if not already)
df <- read_csv("Efficiency_Markets_Access.csv")

# 2. Aggregate at the country level:
#    – compute mean Efficiency Score
#    – take the (constant) Access 2022 value per country
df_country <- df %>%
  filter(!is.na(`Efficiency Score`)) %>%
  group_by(Country) %>%
  summarise(
    Mean_Efficiency_Score = mean(`Efficiency Score`, na.rm = TRUE),
    Access_2022           = mean(`Access 2022`,      na.rm = TRUE),
    .groups = "drop"
  )

# 3. Inspect the new data frame
print(df_country)

# accesss box plot ------

# 1. Create efficiency groups on df_country
df_country <- df_country %>%
  mutate(
    Efficiency_Group = case_when(
      Mean_Efficiency_Score == 1                           ~ "1",
      Mean_Efficiency_Score >= 0.66 & Mean_Efficiency_Score < 1  ~ "0.66–0.99",
      Mean_Efficiency_Score >= 0.33 & Mean_Efficiency_Score < 0.66 ~ "0.33–0.66",
      Mean_Efficiency_Score >= 0 & Mean_Efficiency_Score < 0.33     ~ "0–0.33"
    ),
    Efficiency_Group = factor(
      Efficiency_Group,
      levels = c("0–0.33", "0.33–0.66", "0.66–0.99", "1")
    )
  )

# 2. Compute counts and max y for label placement
counts <- df_country %>%
  count(Efficiency_Group, name = "n")
max_y <- max(df_country$Access_2022, na.rm = TRUE)

# 3. Build the box‐plot
p <- ggplot(df_country, aes(x = Efficiency_Group, y = Access_2022)) +
  geom_boxplot(fill = "orange", color = "black") +
  # mean marker
  stat_summary(aes(shape = "Mean"),
               fun    = mean,
               geom   = "point",
               size   = 4,
               fill   = "white",
               stroke = 1) +
  # median marker
  stat_summary(aes(shape = "Median"),
               fun    = median,
               geom   = "point",
               size   = 4,
               fill   = "black") +
  # count labels above each box
  geom_text(data = counts,
            aes(x = Efficiency_Group,
                y = max_y * 1.05,
                label = n),
            vjust = 0,
            size  = 5) +
  # expand y so nothing is cut off
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  # legend for mean/median
  scale_shape_manual(name   = "Statistic",
                     values = c(Mean = 23, Median = 21)) +
  guides(shape = guide_legend(override.aes = list(fill = c("white", "black")))) +
  labs(
    title = "Access 2022 by Efficiency Score Group",
    x     = "Efficiency Score Group",
    y     = "Access 2022"
  ) +
  theme_minimal() +
  theme(
    panel.background  = element_rect(fill = "transparent", color = NA),
    plot.background   = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key        = element_rect(fill = "transparent", color = NA),
    plot.title        = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title        = element_text(size = 14),
    axis.text         = element_text(size = 12),
    legend.position   = "top",
    legend.title      = element_text(size = 12),
    legend.text       = element_text(size = 11)
  )

# 4. Display and save
print(p)
ggsave("boxplot_access_by_eff_group.png",
       plot = p,
       bg   = "transparent",
       width  = 6,
       height = 4)



# map ---------
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")

library(sf)
library(ggplot2)
library(dplyr)
library(readr)
library(rnaturalearth)
library(rnaturalearthdata)

# 1. Load your CSV
df <- readr::read_csv("Efficiency_Markets_Access.csv", show_col_types = FALSE)

# 2. Pull world sf and flag which countries have data
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(has_data = admin %in% df$Country)

# 3. Build the map
p <- ggplot() +
  # all countries, transparent fill, light grey borders
  geom_sf(data = world, fill = "transparent", color = "grey80", size = 0.2) +
  # highlight those with data in dark orange
  geom_sf(data = filter(world, has_data),
          fill  = "darkorange",
          color = "black",
          size  = 0.2) +
  labs(title = "Countries with Data in Efficiency_Markets_Access.csv") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(hjust = 0.5, face = "bold", size = 16)
  )

# 4. Display
print(p)

# 5. (Optional) Save with transparent background
ggsave("map_countries_with_data.png",
       plot = p,
       bg   = "transparent",
       width  = 8,
       height = 4)