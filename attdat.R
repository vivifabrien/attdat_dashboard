#devtools::install_github("favstats/dashboardr")

pacman::p_load(dashboardr, tidyverse)

# Read data
dat <- read_csv("data/raw_att.csv") %>% 
  filter(MAXPAGE == 21 & country == "DE" | MAXPAGE == 20 & !country == "DE") %>%
  janitor::clean_names() %>%
  mutate(across(everything(), ~ifelse(.x < 0, NA, .x))) %>%
  mutate(
    q004_recoded = case_when(
      # DE
      q004 < 3 & country == "DE" ~ 1,     # low
      q004 %in% c(3,4) & country == "DE"  ~ 2,     # middle
      q004 > 4 & country == "DE" ~ 3 ,     # high
      # ES
      q004 < 4 & country == "ES" ~ 1,              # low
      q004  == 4 & country == "ES" ~ 2,     # middle
      q004 > 4 & country == "ES"  ~ 3,              # high
      # PL
      q004 <= 3 & country == "PL"  ~ 1,                        # low
      q004  %in% c(4:8) & country == "PL" ~ 2,         # middle
      q004 >= 9  & country == "PL" ~ 3,                       # high
      # NL
      q004  < 3 & country == "NL" ~ 1,                         # low
      q004 %in% c(3,4) & country == "NL" ~ 2,          # middle
      q004 > 4 & country == "NL" ~ 3,                        # high
      TRUE ~ NA_real_
    ),
    q001 = factor(q001,levels = c(1:2), labels = c("Male","Female") ),
    q002 = factor(q002,levels = c(1:6), labels = c("18-24 years","25-34 years", "35-44 years", "45-54 years", "55-64 years","65+") ),
    q004_recoded = factor(q004_recoded, levels = c(1:3), labels = c("Low","Middle","High")),
  ) %>%
  # Label locations
  mutate(q003 = case_when(
    # Germany
    country == "DE" & q003 == 1 ~ "Baden-Württemberg",
    country == "DE" & q003 == 2 ~ "Bayern",
    country == "DE" & q003 == 3 ~ "Berlin",
    country == "DE" & q003 == 4 ~ "Brandenburg",
    country == "DE" & q003 == 5 ~ "Bremen",
    country == "DE" & q003 == 6 ~ "Hamburg",
    country == "DE" & q003 == 7 ~ "Hessen",
    country == "DE" & q003 == 8 ~ "Mecklenburg-Vorpommern",
    country == "DE" & q003 == 9 ~ "Niedersachsen",
    country == "DE" & q003 == 10 ~ "Nordrhein-Westfalen",
    country == "DE" & q003 == 11 ~ "Rheinland-Pfalz",
    country == "DE" & q003 == 12 ~ "Saarland",
    country == "DE" & q003 == 13 ~ "Sachsen",
    country == "DE" & q003 == 14 ~ "Sachsen-Anhalt",
    country == "DE" & q003 == 15 ~ "Schleswig-Holstein",
    country == "DE" & q003 == 16 ~ "Thüringen",
    country == "DE" & q003 == -9 ~ "Not answered",
    
    # Netherlands
    country == "NL" & q003 == 1 ~ "Groningen",
    country == "NL" & q003 == 2 ~ "Friesland",
    country == "NL" & q003 == 3 ~ "Drenthe",
    country == "NL" & q003 == 4 ~ "Overijssel",
    country == "NL" & q003 == 5 ~ "Gelderland",
    country == "NL" & q003 == 6 ~ "Flevoland",
    country == "NL" & q003 == 7 ~ "Utrecht",
    country == "NL" & q003 == 8 ~ "Noord-Holland",
    country == "NL" & q003 == 9 ~ "Zuid-Holland",
    country == "NL" & q003 == 10 ~ "Zeeland",
    country == "NL" & q003 == 11 ~ "Noord-Brabant",
    country == "NL" & q003 == 12 ~ "Limburg",
    country == "NL" & q003 == -9 ~ "Not answered",
    
    # Spain
    country == "ES" & q003 == 1 ~ "Andalucía",
    country == "ES" & q003 == 2 ~ "Aragón",
    country == "ES" & q003 == 3 ~ "Cantabria",
    country == "ES" & q003 == 4 ~ "Castilla y León",
    country == "ES" & q003 == 5 ~ "Castilla-La Mancha",
    country == "ES" & q003 == 6 ~ "Cataluña",
    country == "ES" & q003 == 7 ~ "Ceuta",
    country == "ES" & q003 == 8 ~ "Comunidad Foral de Navarra",
    country == "ES" & q003 == 9 ~ "Comunidad Valenciana",
    country == "ES" & q003 == 10 ~ "Comunidad de Madrid",
    country == "ES" & q003 == 11 ~ "Extremadura",
    country == "ES" & q003 == 12 ~ "Galicia",
    country == "ES" & q003 == 13 ~ "Islas Baleares",
    country == "ES" & q003 == 14 ~ "Islas Canarias",
    country == "ES" & q003 == 15 ~ "La Rioja",
    country == "ES" & q003 == 16 ~ "Melilla",
    country == "ES" & q003 == 17 ~ "País Vasco",
    country == "ES" & q003 == 18 ~ "Principado de Asturias",
    country == "ES" & q003 == 19 ~ "Región de Murcia",
    country == "ES" & q003 == -9 ~ "Not answered",
    
    # Poland
    country == "PL" & q003 == 1 ~ "Dolnośląskie",
    country == "PL" & q003 == 2 ~ "Kujawsko-Pomorskie",
    country == "PL" & q003 == 3 ~ "Lubelskie",
    country == "PL" & q003 == 4 ~ "Lubuskie",
    country == "PL" & q003 == 5 ~ "Łódzkie",
    country == "PL" & q003 == 6 ~ "Małopolskie",
    country == "PL" & q003 == 7 ~ "Mazowieckie",
    country == "PL" & q003 == 8 ~ "Opolskie",
    country == "PL" & q003 == 9 ~ "Podkarpackie",
    country == "PL" & q003 == 10 ~ "Podlaskie",
    country == "PL" & q003 == 11 ~ "Pomorskie",
    country == "PL" & q003 == 12 ~ "Śląskie",
    country == "PL" & q003 == 13 ~ "Świętokrzyskie",
    country == "PL" & q003 == 14 ~ "Warmińsko-Mazurskie",
    country == "PL" & q003 == 15 ~ "Wielkopolskie",
    country == "PL" & q003 == 16 ~ "Zachodniopomorskie",
    country == "PL" & q003 == -9 ~ "Not answered",
    
    TRUE ~ NA_character_
  )) %>%
  mutate(ad03 = factor(ad03, levels = c(1:5),labels = c("Not at all useful", "Slightly useful","Moderately useful","Very useful","Extremely useful")))
  



demographics <- create_content(data = dat, type = "bar", color_palette = c("#d7191c", "#fdae61", "#fee08b", "#e6f598", "#abdda4", "#66c2a5", "#2b83ba")) %>%
  add_text("This is a very preliminary visualization of our data yippie.") %>%
  add_viz(x_var = "q001", title = "Germany", tabgroup = "Demographics/Gender", filter = ~ country == "DE", x_label = "Gender") %>%
  add_viz(x_var = "q001", title = "Netherlands", tabgroup = "Demographics/Gender", filter = ~ country == "NL", x_label = "Gender") %>%
  add_viz(x_var = "q001", title = "Spain", tabgroup = "Demographics/Gender", filter = ~ country == "ES", x_label = "Gender") %>%
  add_viz(x_var = "q001", title = "Poland", tabgroup = "Demographics/Gender", filter = ~ country == "PL", x_label = "Gender") %>%
  add_viz(x_var = "q002", title = "Germany", tabgroup = "Demographics/Age", , filter = ~ country == "DE", x_label = "Age") %>%
  add_viz(x_var = "q002", title = "Netherlands", tabgroup = "Demographics/Age", , filter = ~ country == "NL", x_label = "Age") %>%
  add_viz(x_var = "q002", title = "Spain", tabgroup = "Demographics/Age", , filter = ~ country == "ES", x_label = "Age") %>%
  add_viz(x_var = "q002", title = "Poland", tabgroup = "Demographics/Age", , filter = ~ country == "PL", x_label = "Age") %>%
  add_viz(x_var = "q003", title = "Germany", tabgroup = "Demographics/State", filter = ~ country == "DE", x_label = "State") %>%
  add_viz(x_var = "q003", title = "Netherlands", tabgroup = "Demographics/State", filter = ~ country == "NL", x_label = "State") %>%
  add_viz(x_var = "q003", title = "Spain", tabgroup = "Demographics/State", filter = ~ country == "ES", x_label = "State") %>%
  add_viz(x_var = "q003", title = "Poland", tabgroup = "Demographics/State", filter = ~ country == "PL", x_label = "State") %>%
  add_viz(x_var = "q004_recoded", title = "Germany", tabgroup = "Demographics/Education", filter = ~ country == "DE", x_label = "Education") %>%
  add_viz(x_var = "q004_recoded", title = "Netherlands", tabgroup = "Demographics/Education", filter = ~ country == "NL", x_label = "Education") %>%
  add_viz(x_var = "q004_recoded", title = "Spain", tabgroup = "Demographics/Education", filter = ~ country == "ES", x_label = "Education") %>%
  add_viz(x_var = "q004_recoded", title = "Poland", tabgroup = "Demographics/Education", filter = ~ country == "PL", x_label = "Education")

print(demographics)
preview(demographics)

#Attitudes
attitudes <- create_content(data = dat, type = "stackedbar", tooltip_suffix = "%") %>%
  add_viz(x_var = "country", stack_var = "ad03", title = "Gen-AI Use", tabgroup = "Attitudes", stacked_type = "percent",
          tooltip_suffix = "%",
          color_palette = c("#d7191c", "#fdae61", "#fee08b", "#e6f598", "#abdda4")) %>%
  add_viz(x_var= "country", stack_var = "g112_01", title = "Left-Right Attitude", tabgroup = "Attitudes", 
          stacked_type = "percent", tooltip_suffix = "%",
          color_palette = c("darkred","#d7191c", "#fdae61", "#fee08b", "#ffffbf", "#e6f598", "#abdda4", "#66c2a5", "#2b93ba", "#2b73ba","#2b53ba"))



# Create pages
#home <- create_page("Home", is_landing_page = TRUE) %>%
  #add_text("# AttDat Explorer", "", "Explore the Attitude Survey data.")
analysis <- create_page("Analysis", data = dat, icon = "ph:chart-bar", is_landing_page = TRUE) %>%
  add_content(demographics) %>%
  add_content(attitudes)
about <- create_page("About", navbar_align = "right", icon = "ph:info") %>%
  add_text("## About", "", "Built with [dashboardr](https://github.com/favstats/dashboardr)")

# Assemble dashboard
my_dashboard <- create_dashboard(
  title = "Data Explorer", 
  output_dir = "attdat_dashboard", 
  publish_dir = "../docs", 
  theme = "minty"
) %>%
  add_pages(analysis, about)
# Generate!
my_dashboard %>% generate_dashboard(render = TRUE, open = "browser")
