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
    )
  )


demographics <- create_content(data = dat, type = "bar") %>%
  add_text("This is a very preliminary visualization of our data yippie.") %>%
  add_viz(x_var = "q001", title = "Germany", tabgroup = "Demographics/Gender", filter = ~ country == "DE") %>%
  add_viz(x_var = "q001", title = "Netherlands", tabgroup = "Demographics/Gender", filter = ~ country == "NL") %>%
  add_viz(x_var = "q001", title = "Spain", tabgroup = "Demographics/Gender", filter = ~ country == "ES") %>%
  add_viz(x_var = "q001", title = "Poland", tabgroup = "Demographics/Gender", filter = ~ country == "PL") %>%
  add_viz(x_var = "q002", title = "Germany", tabgroup = "Demographics/Age", , filter = ~ country == "DE") %>%
  add_viz(x_var = "q002", title = "Netherlands", tabgroup = "Demographics/Age", , filter = ~ country == "NL") %>%
  add_viz(x_var = "q002", title = "Spain", tabgroup = "Demographics/Age", , filter = ~ country == "ES") %>%
  add_viz(x_var = "q002", title = "Poland", tabgroup = "Demographics/Age", , filter = ~ country == "PL") %>%
  add_viz(x_var = "q003", title = "Germany", tabgroup = "Demographics/State", filter = ~ country == "DE") %>%
  add_viz(x_var = "q003", title = "Netherlands", tabgroup = "Demographics/State", filter = ~ country == "NL") %>%
  add_viz(x_var = "q003", title = "Spain", tabgroup = "Demographics/State", filter = ~ country == "ES") %>%
  add_viz(x_var = "q003", title = "Poland", tabgroup = "Demographics/State", filter = ~ country == "PL") %>%
  add_viz(x_var = "q004_recoded", title = "Germany", tabgroup = "Demographics/Education", filter = ~ country == "DE") %>%
  add_viz(x_var = "q004_recoded", title = "Netherlands", tabgroup = "Demographics/Education", filter = ~ country == "NL") %>%
  add_viz(x_var = "q004_recoded", title = "Spain", tabgroup = "Demographics/Education", filter = ~ country == "ES") %>%
  add_viz(x_var = "q004_recoded", title = "Poland", tabgroup = "Demographics/Education", filter = ~ country == "PL")

print(demographics)
preview(demographics)

attitudes <- create_content(data = dat, type = "stackedbar") %>%
  add_viz(x_var = "ad03", stack_var = country, title = "Gen-AI Use", tabgroup = "Attitudes") %>%
  add_viz(x_var= "g112_01", stack_var = country, title = "Left-Right Attitude", tabgroup = "Attitudes")



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
  theme = "flatly"
) %>%
  add_pages(analysis, about)
# Generate!
my_dashboard %>% generate_dashboard(render = TRUE, open = "browser")
