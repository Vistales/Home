library(lubridate) 
library(tidyverse) 
library(ggplot2)
library(scales)
library(stringr) 
library(plotly) 
library(shiny) 
library(crosstalk)  
library(highcharter)
library(showtext)
library(dplyr)
library(lubridate)

########### typography ###########

font_add_google("Fraunces", "Fraunces")
font_add_google("Poppins", "poppins")
font_add_google("Roboto Slab", "body_font")
showtext_auto()

#############

poppins<- "poppins"
body_font<- "body_font"

df <- read.csv("./dataset.csv") # Place your data transformation code here 


# Create a function to determine fiscal year based on given formula
get_fiscal_year <- function(date) {
  year <- as.integer(format(date, "%Y"))
  ifelse(month(date) >= 7, year + 1, year)
}

# Calculate fiscal year for each observation in the data frame
df$fiscal_year <- get_fiscal_year(as.Date(df$Observation.Date, format = "%d-%b-%Y"))

# Group by country and fiscal year, then calculate sum of remit values
df <- df %>%
  group_by(Series.Display.Name, fiscal_year) %>%
  summarise(remit = sum(Observation.Value)) %>%
  rename(country = Series.Display.Name,
         year = fiscal_year)%>%
  mutate(country = sub(".*?\\.?\\s+(\\b\\w+\\b)", "\\1", country))%>%
  filter(!str_detect(country, "Cash Flow|Other|Total|EU Countries|Encashment|Dubai|Sharjah|Abu Dhabi|Others"))%>%
  arrange(country)%>%
  mutate(remit = round(remit/1000 , 2))



g<- ggplot(df, aes(year, remit, group = country)) + 
  geom_hline(yintercept = seq(from = 0, to = 8, by = 2), col = "gray") +
  geom_line(data = subset(df, country %in% c("Saudi Arabia", "U.A.E.", "U.K.", "USA")), aes(color = country), size = 1.2) +
  geom_line(data = subset(df, !country %in% c("Saudi Arabia", "U.A.E.", "U.K.", "USA")), color = "gray", size = 1) +
  scale_color_manual(values = c("Saudi Arabia" = "#53833B", "U.A.E." = "#C0AD2E", "U.K." = "#3A9AA1", "USA" = "#99599E")) +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = '#F6ECDF', color = '#F6ECDF'),
        panel.border = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y =element_blank(),
        plot.margin = margin(20, 20, 20, 20),
        plot.title = element_text(family = "Fraunces", face = 'bold',
                                  size = 16,color = "black",margin = margin(0, 0, 0, 0)),
        plot.subtitle = element_text(family = "Fraunces",
                                     size = 10,color = "black",margin = margin(10.5, 0, 10, 0)),
        plot.caption = element_text(family = "Fraunces",
                                     size = 8,color = "black",margin = margin(10,-30, 0, 0)),
        axis.text.x = element_text(family = "poppins", face= "bold", size = 8),
        axis.text.y = element_text(family = "poppins", face = "bold",size =8),
        plot.background = element_rect(fill = "#F6ECDF" )) +
  geom_text(data = df[df$country == "Saudi Arabia",], aes(x = 2018, y = 7.00, label = "Saudi Arabia", angle=75), color = "#53833B", family= body_font,size = 4.5)  +
  geom_text(data = df[df$country == "U.A.E.",], aes(x = 2017, y = 3.8, label = "UAE"), color = "#C0AD2E",family= body_font, size = 4.5) +
  geom_text(data = df[df$country == "U.K.",], aes(x = 2014, y = 1.5, label = "UK"), color = "#3A9AA1",family= body_font, size = 4.5) +
  geom_text(data = df[df$country == "USA",], aes(x = 2004, y = 1.550, label = "USA"), color = "#99599E",family= body_font, size = 4.5)+
  scale_y_continuous(position = "right", labels = label_number(suffix = " billion $"))+
  coord_cartesian(expand = FALSE)+
  labs(x = NULL, y = NULL ,title= "Remittance Inflows to Pakistan, 1970 - 2023",
       subtitle = "Saudi Arabia and UAE Stand Out as Top Contributors to Pakistan's Econonomy",
       caption = "Vistales | Data: Staete Bank of Pakistan")
showtext_opts(dpi = 320)
ggsave("g.png", g, dpi=320, width = 6, height = 6)












