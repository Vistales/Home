library(tidyverse)
library(scales)
library(showtext)

font_add_google("Fraunces", "title_font")
font_add_google("Montserrat", "body_font")
showtext_auto()

title_font<- "title_font"
body_font<- "body_font"
bg <- "#EFEFEF"
bg<-"#F6F8FA"

# Your current data (example structure)
education_data <- data.frame(
  Education_Level = c("Illiterate", "No Formal Education", "Below Matric", 
                     "Matriculation", "Intermediate", "Degree and Above"),
  Employed = c(38.8, 1.2, 31.7, 13.4, 6.7, 8.2),
  Unemployed = c(18.5, 1.4, 24.4, 18.8, 13.3, 23.5),
  Out_of_Labor_Force = c(37.0, 0.7, 43.3, 10.1, 5.2, 3.6)
)
# Transform data to long format
plot_data <- education_data %>%
  pivot_longer(cols = c(Employed, Unemployed, Out_of_Labor_Force),
               names_to = "Employment_Status",
               values_to = "Percentage") %>%
  mutate(Education_Level = factor(Education_Level,
                                 levels = c("Illiterate", "No Formal Education", "Below Matric",
                                           "Matriculation", "Intermediate", "Degree and Above")),
         Employment_Status = factor(Employment_Status,
                                   levels = c("Employed", "Unemployed", "Out_of_Labor_Force")))



custom_order <- c("Out_of_Labor_Force","Unemployed", "Employed")

# Create a new factor with the custom order
plot_data$Employment_Status <- factor(plot_data$Employment_Status, levels = custom_order)


# Stacked bar chart version
ggplot(plot_data,  aes(fill = Education_Level, x= Percentage, y=Employment_Status)) +
  #geom_col(position = "fill") +
  geom_bar(position = "fill", stat = "identity")+
  scale_fill_manual(values = c("Degree and Above" = "#1C375B", 
                              "Illiterate" = "#CD4236",
                              "Intermediate" = "#6F8197",
                            "No Formal Education"= "#D2544A",
                            "Matriculation"= "#83B6C1",
                          "Below Matric" = "#60727E")) +
  scale_x_continuous(labels = percent_format()) +
  labs(title = "Education Level vs Employment Status",
       subtitle = "Distribution across different education levels in Pakistan",
       y = "", x = "") +
  theme_void()+
  theme(legend.position = "none",
plot.title = element_text(size=28, family  = title_font, face="bold",
                              margin=margin(10,0,5,0)),
plot.subtitle = element_text(size=16, family = title_font, face="plain",color = "grey30",
                                 margin=margin(10,0,80,0)),
plot.background = element_rect(color = bg, fill=bg),                            
panel.spacing = unit(1.2, "cm"),
plot.margin = margin(30,50,30,50),                            ) 

showtext_opts(dpi = 320) 
ggsave("education-employment.png", height = 7,width = 7.5, dpi=320)  

