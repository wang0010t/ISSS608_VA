packages = c('tidyverse', 'ggdist', 'gghalves', 'treemap', 'rPackedBar')
packages = c('ggiraph', 'plotly', 
             'DT', 'patchwork',
             'gganimate', 'tidyverse',
             'readxl', 'gifski', 'gapminder',
             'treemap', 'treemapify',
             'rPackedBar')
for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
    }
  library(p, character.only = T)
  }

GDP <- read_csv("data/GDP.csv")
WorldCountry <- read_csv("data/WorldCountry.csv")

GDP_selected <- GDP %>%
  mutate(Values = as.numeric(`2020`)) %>% # `` tip not '' quote
  select(1:3, Values) %>%
  pivot_wider(names_from = `Series Name`,
              values_from = `Values`) %>%
  left_join(y=WorldCountry, by = c("Country Code" = "ISO-alpha3 Code"))


treemap(GDP_selected,
        index=c("Continent", "Country Name"),
        vSize="GDP (current US$)",
        vColor="GDP (current US$)",
        title="GDP (current US$) , 2020",
        title.legend = "GDP per capita (current US$)"
)

treemap(GDP_selected,
        index=c("Continent", "Country Name"),
        vSize="GDP (current US$)",
        vColor="GDP per capita (current US$)",
        type = "value",
        title="GDP (current US$) , 2020",
        title.legend = "GDP per capita (current US$)"
)


tm <- treemap(GDP_selected,
        index=c("Continent", "Country Name"),
        vSize="GDP (current US$)",
        vColor="GDP per capita (current US$)",
        type = "value",
        algorithm = "squarified",
        title="GDP (current US$) , 2020",
        title.legend = "GDP per capita (current US$)"
)


library(devtools)
install_github("timelyportfolio/d3treeR")


library(d3treeR)


d3tree(tm, rootname = "World" )


GDP_selected1 <- GDP %>%
  mutate(GDP = as.numeric(`2020`)) %>%
  filter(`Series Name` == "GDP (current US$)") %>%
  select(1:2, GDP) %>%
  na.omit()

library(rPackedBar)
p = plotly_packed_bar(
  input_data = GDP_selected1,
  label_column = "Country Name",
  value_column = "GDP",
  number_rows = 10,
  plot_title = "Tweet Interactions",
  xaxis_label = "Favorites & RTs",
  hover_label = "GDP",
  min_label_width = 0.018,
  color_bar_color = "#00aced",
  label_color = "white")
plotly::config(p, displayModeBar = FALSE)

library(shiny)



exam_data <- read_csv("data/Exam_data.csv")
ggplot(data=exam_data, 
       aes(x = MATHS)) +
  geom_dotplot(binwidth=2.5, 
               dotsize = 0.5) +
  scale_y_continuous(NULL, 
                     breaks = NULL)


p <- ggplot(data=exam_data, 
            aes(x = MATHS)) +
  geom_dotplot_interactive(
    aes(tooltip = ID),
    stackgroups = TRUE,
    binwidth = 1,
    method = "histodot") +
  scale_y_continuous(NULL,               
                     breaks = NULL)
girafe(
  ggobj = p,
  width_svg = 6,
  height_svg = 6*0.618
)

exam_data$tooltip <- c(paste0(
  "Name = ", exam_data$ID,
  "\n Class = ", exam_data$CLASS))
p <- ggplot(data=exam_data, 
            aes(x = MATHS)) +
  geom_dotplot_interactive(
    aes(tooltip = exam_data$tooltip),
    stackgroups = TRUE,
    binwidth = 1,
    method = "histodot") +
  scale_y_continuous(NULL,               
                     breaks = NULL)
girafe(
  ggobj = p,
  width_svg = 8,
  height_svg = 8*0.618
)


p <- ggplot(data=exam_data, 
            aes(x = MATHS,
                y = ENGLISH)) +
  geom_point(dotsize = 1) +
  coord_cartesian(xlim=c(0,100),
                  ylim=c(0,100))
ggplotly(p)



