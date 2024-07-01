library(ggplot2)
library(dplyr)
library(readxl)
library(sf)
library(rvest)
library(plotly)
library(stringr)
library(showtext)



font_add_google("Fraunces", "title_font")
font_add_google("Montserrat", "body_font")
showtext_auto()

title_font<- "title_font"
body_font<- "body_font"

shp2 <-  read_sf("./PAK_adm_2/pak_admbnda_adm2_wfp_20220909.shp")
### District level file #####

data<- read_excel("C:/Users/Pc/Documents/Quarto/pp/Pak-pop-district.xlsx")

df<-data %>%
  rename(Name = "Name of Admin Unit") %>%
  filter(!str_detect(Name, "DIVISION"))%>%
  mutate(Name = gsub(" DISTRICT$", "", Name))%>%
  select("Name", "Growth Rate")%>%
  mutate(Name = str_to_title(tolower(Name)))

df <- df %>%
  mutate(Name = case_when(
    Name == "Shaheed Benazirabad" ~ "Shaheed Benazir Abad" ,
    Name ==  "Malir"~ "Malir Karachi",
    Name ==  "Karachi West"~"West Karachi",
    Name ==  "Karachi East"~"East Karachi" ,
    Name ==  "Karachi Central"~ "Central Karachi",
    Name == "Karachi South"~"South Karachi" ,
    Name ==  "Korangi" ~ "Korangi Karachi",
    Name == "Layyah"~"Leiah" ,
    Name ==  "Dera Ismail Khan" ~"D. I. Khan",
    Name == "Malakand Protected Area"~"Malakand" ,
    Name == "Lower Chitral" ~ "Chitral Lower" ,
    Name == "Upper Chitral" ~ "Chitral Upper" ,
    Name == "Torghar" ~ "Tor Ghar" ,
    Name == "Lower Kohistan" ~ "Kohistan Lower" ,
    Name == "Upper Kohistan" ~ "Kohistan Upper" ,
    TRUE ~ Name
  ))

df<-df %>%
  rename(ADM2_EN = "Name") 
merged_data <- merge(shp2, df, by.x = "ADM2_EN", all.x = TRUE)



sal<-ggplot(merged_data, aes(fill = `Growth Rate`)) +
  geom_sf() +
  #geom_sf_text(data = merged_data ,aes(label = ADM2_EN),
   #         size = 3, color = "black", nudge_y = 0.05)+
  geom_point(aes(x = 70, y = 30, color = "No Data"), size = 0.2) +
  scale_fill_gradient(low = "#F0ECCC", high = "#f25c54", guide = "colorbar", na.value = "#ACACAC") +
  scale_color_manual(values = c("No Data" = "#ACACAC")) +
  theme_void()+
  coord_sf(clip = "off")+
  theme(plot.margin = margin(0,0,0,0),
        plot.title = element_text(family = title_font, face = 'bold',size = 15,color = "black",margin = margin(25, 0, 10, -20)),
        plot.caption = element_text(family = body_font, size = 6,color = "black",margin = margin(10, -20, 5, 0)),
        legend.position = "bottom",
        legend.key.width = unit(1.6,"cm"),
        legend.title = element_text(vjust = 0),
        legend.box.just = "left",
        plot.background = element_rect(fill = "#F4F1DE", colour = "#E2EFD8"),
        panel.background = element_rect(fill = "#F4F1DE", colour = "#E2EFD8"),
        legend.background = element_rect(fill = "#F4F1DE", colour = "#E2EFD8")) +
  labs(title = "District Level Population Growth Rate of Pakistan",
       fill = "values in percent",
       caption = "Vistales | Data: Pakistan Bureau of Statistics",
       color = NULL)+
  guides(color = guide_legend(override.aes = list(shape = "square", size = 3.2)))+
  guides(fill = guide_colorbar(title.position = "top", title.hjust=0.5,))

showtext_opts(dpi = 320)         

ggsave("growthrate.png", sal, dpi=320, width = 6, height = 6)


ggplotly(sal)
