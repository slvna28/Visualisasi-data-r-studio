library(colorspace)
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)    # for generating the visualiations
supermarket <- read_excel("E:/Supermarket Transactions.xlsx", sheet = "Data")

head(supermarket)

city_rev <- supermarket %>%
  group_by(City) %>%
  summarise(Revenue = sum(Revenue, na.rm = TRUE)) %>%
  arrange(Revenue) %>%
  mutate(City = factor(City, levels = .$City))

city_gender_rev <- supermarket %>%
  group_by(City, Gender) %>%
  summarise(Revenue = sum(Revenue, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(City = factor(City, levels = city_rev$City))

head(city_gender_rev)

#plot
right_label <- city_gender_rev %>%
  group_by(City) %>%
  top_n(1)

left_label <- city_gender_rev %>%
  group_by(City) %>%
  arrange(desc(Revenue)) %>%
  slice(2)

big_diff <- city_gender_rev %>%
  spread(Gender, Revenue) %>%
  group_by(City) %>%
  mutate(Max = max(F, M),
         Min = min(F, M),
         Diff = Max / Min -1) %>%
  arrange(desc(Diff)) %>%
  filter(Diff > .2)
# filter the label data frames to only include those cities where the
# difference exceeds 20%

right_label <- filter(right_label, City %in% big_diff$City)

left_label <- filter(left_label, City %in% big_diff$City)

# filter the main data frame to only include those cities where the
# difference exceeds 20%

highlight <- filter(city_gender_rev, City %in% big_diff$City)

plot_label <- big_diff %>%
  select(City, Revenue = Max, Diff) %>%
  right_join(right_label)

p <- ggplot(city_gender_rev, aes(Revenue, City)) +
  geom_line(aes(group = City), alpha = .3) +
  geom_point(aes(color = Gender), size = 1.5, alpha = .3) +
  geom_line(data = highlight, aes(group = City)) +
  geom_point(data = highlight, aes(color = Gender), size = 2) +
  geom_text(data = plot_label, aes(color = Gender,
                                   label = paste0("+", scales::percent(round(Diff, 2)))),
            size = 3, hjust = -.5)

p + scale_color_discrete(labels = c("Female", "Male")) +
  scale_x_continuous(labels = scales::dollar, expand = c(0.02, 0), 
                     limits = c(0, 10500),
                     breaks = seq(0, 10000, by = 2500)) +
  scale_y_discrete(expand = c(.02, 0)) +
  scale_color_manual(name = "Gender",
                     values = darken(c("#F3024A","#002958"))) +
  labs(title = "Jumlah Pendapatan Menurut Kota dan Jenis Kelamin",
       subtitle = "Dari 23 Kota, 8 lokasi mengalami perbedaan 20% atau lebih besar pendapatan \nyang dihasilkan oleh pria dan wanita. Hidalgo mengalami perbedaan terbesar \ndengan wanita menghasilkan pendapatan 86% lebih banyak dari pria",
       tag = "") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.justification = "right", 
        legend.position = "right",
        legend.background = element_blank(),
        legend.direction="vertical",
        text = element_text(family = "Georgia"),
        plot.title = element_text(size = 13, margin = margin(b = 2), face = "bold.italic", color = "#F54D4D"),
        plot.subtitle = element_text(size = 9, color = "#150B0B", margin = margin(b = 10), face = "italic"),
        plot.tag = element_text(size = 10, color = "black"),
  )