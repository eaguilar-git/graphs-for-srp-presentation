# install.packages(c("rnaturalearth", "rnaturalearthdata", "sf", "ggplot2", "dplyr"))
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(dplyr)
library(ggplot2)

# map -----

# 1. List of countries to highlight
highlight_countries <- c(
  "Brazil", "Bolivia", "Peru", "Marshall Islands", "Pakistan",
  "Federated States of Micronesia", "Kyrgyzstan", "Bangladesh",
  "Colombia", "Sudan", "Namibia", "Uganda", "Argentina",
  "Saint Lucia", "Dominica", "Armenia", "Thailand", "Ivory Coast",
  "Ecuador", "Benin", "Kazakhstan", "Indonesia", "Ghana",
  "Cameroon", "Mozambique", "Senegal", "Burkina Faso", "Angola",
  "Niger", "Central African Republic", "Paraguay",
  "Bosnia and Herzegovina", "Zimbabwe", "Malaysia", "Zambia",
  "South Africa", "Democratic Republic of the Congo"
)

# 2. Load world map and flag
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(has_data = admin %in% highlight_countries)

# 3. Plot: non-data countries in gray, data countries in dark orange
p <- ggplot(world) +
  geom_sf(aes(fill = has_data),
          color = "white",    # country borders
          size  = 0.2) +
  scale_fill_manual(
    values = c("TRUE"  = "darkorange",
               "FALSE" = "gray80"),
    guide  = FALSE
  ) +
  labs(title = "Highlighted Countries from Your List") +
  theme_void() +
  theme(
    plot.title       = element_text(hjust = 0.5, face = "bold", size = 16),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA)
  )

# 4. Display
print(p)

# 5. (Optional) Save PNG with transparent background
ggsave(
  "highlighted_countries_map.png",
  plot = p,
  bg   = "transparent",
  width  = 8,
  height = 5
)
