library(ggplot2)
library(tidyverse)
library(showtext)

font_add_google("Commissioner", "title_font")
font_add_google("Fraunces", "body_font")
showtext_auto()

title_font<- "title_font"
body_font<- "body_font"
bg<-"#F6F8FA"



data<-read.csv("work-industry.csv")

df_long <- data %>%
  pivot_longer(
    cols = -Year,
    names_to = "Industry",
    values_to = "Percentage"
  )

#Color palette
pal=c("#ffa600",
      "#ff7c43",
      "#f95d6a",
      "#d45087",
      "#a05195",
      "#665191",
      "#003f5c")

# Stacked area chart
ggplot(df_long, aes(x = Year, y = Percentage, fill = Industry)) +
  geom_area(alpha = 0.85) +
  scale_fill_manual(values = pal)+ coord_cartesian(expand = FALSE)+
  geom_text(
    x = 2011,
    y = 75,
    label = 'Agriculture',
    family  = body_font,
    fontface = 'bold', # makes text bold
    size = 4.5, # font size
    show.legend = FALSE,
    color = 'white'
  ) +
  geom_text(
    x = 2011,
    y = 51.5,
    label = 'Construction',
    fontface = 'bold', # makes text bold
    family  = body_font,
    size = 4.5, # font size
    show.legend = FALSE,
    color = 'white'
  ) +
  geom_text(
    x = 2011,
    y = 7,
    label = 'Wholesale, Trade and Retail',
    fontface = 'bold', # makes text bold
    family  = body_font,
    size = 4.5, # font size
    show.legend = FALSE,
    color = 'white'
  ) +
  geom_text(
    x = 2011,
    y = 18,
    label = 'Transport, Storage and Communication',
    fontface = 'bold', # makes text bold
    family  = body_font,
    size = 3, # font size
    show.legend = FALSE,
    color = 'white'
  ) +
  geom_text(
    x = 2011,
    y = 41,
    label = 'Mining and Manufacturing',
    fontface = 'bold', # makes text bold
    family  = body_font,
    size = 4, # font size
    show.legend = FALSE,
    color = 'white'
  ) +
  geom_text(
    x = 2010.5,
    y = 27,
    label = 'Others',
    family  = body_font,
    fontface = 'bold', # makes text bold
    size = 4, # font size
    show.legend = FALSE,
    color = 'white'
  ) +
  labs(
    title = str_wrap("Employment Distribution by Major Industries in Pakistan",40),
    subtitle = "Agriculture sector remains the primary employer across two decades", x="",y="",
    caption = "Data: Pakistan Bureau of Statistics, Economic Survey 2024 - 2025" ) +
  #theme_void()+
  theme(legend.position = "none",
plot.title = element_text(size=22, family  = body_font, face="bold",
                              margin=margin(0,0,0,0)),
plot.subtitle = element_text(size=15, family = title_font, face="plain",color = "grey30",
                                 margin=margin(10,0,20,0)),
plot.caption = element_text(size=8, family = title_font, face="plain",color = "grey20",), 
plot.background = element_rect(color = bg, fill=bg),                            
plot.margin = margin(30,40,30,30),
#panel.grid = element_blank(),
panel.background = element_blank(),
#axis.text.y =element_text(),
axis.ticks = element_blank())                    

showtext_opts(dpi = 320) 

ggsave("workdistribution.png", height = 7,width = 7.5, dpi=320)  


