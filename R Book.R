# Loading libraries #

library(tidyverse)
library(ggrepel)
library(scales)

# Opening Cars Database #

mpg
summary(mpg)
tibble(mpg)
view(mpg)

# Studying the database #

mpg %>% 
  ggplot(mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(colour = manufacturer)) +
  geom_smooth(se = FALSE) +
  theme_classic()

mpg %>% 
  ggplot(mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(alpha = class), colour = "darkblue") +
  geom_smooth(se = FALSE) +
  theme_classic()

mpg %>% 
  ggplot(mapping = aes(x = displ, y = hwy)) +
  geom_point(shape = 24, colour = "pink", fill = "pink") +
  geom_smooth(se = FALSE) +
  theme_classic()

?geom_point

mpg %>% 
  ggplot(mapping = aes(x = cyl, y = cty)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

mpg %>% 
  ggplot(aes(x = displ, y = hwy, linetype = drv)) + 
  geom_smooth() +
  theme_classic()

mpg %>% 
  ggplot(aes(x = displ, y = hwy)) + 
  geom_point() +
  facet_wrap(~drv) +
  theme_classic()

mpg %>% 
  ggplot(aes(x = displ, y = hwy)) + 
  geom_point(aes(color = class)) + 
  geom_smooth() +
  theme_classic()

mpg %>% 
  ggplot(aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_point(data = mpg %>% filter(class == "2seater"), color = "red") +
  geom_point(data = mpg %>% filter(class == "compact"), color = "blue") +
  theme_classic()

mpg %>% 
  ggplot(mapping = aes(x = hwy)) +
  geom_density() +
  theme_bw()

mpg %>% 
  ggplot(aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_wrap(~cyl)

mpg %>% 
  ggplot(aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_wrap(~cyl, scales = "free")

mpg %>% 
  ggplot(aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_grid(cyl ~ manufacturer, scales ="free")

mpg %>% 
  ggplot(aes(x = displ, y = hwy)) + 
  geom_point() +
  facet_grid(. ~ drv)

mpg %>% 
  ggplot(aes(x = displ, y = hwy)) + 
  geom_point() +
  facet_wrap(drv ~ ., nrow = 3)

?mpg

mpg %>% 
  ggplot(mapping = aes(x = cty, y = hwy)) +
  geom_point() +
  geom_smooth() +
  theme_classic()

mpg %>% 
  filter(manufacturer == "audi" | manufacturer == "ford" | 
           manufacturer == "chevrolet" | manufacturer == "dodge") %>% 
  ggplot(mapping = aes(x = fct_reorder(manufacturer, desc(hwy), median), y = hwy)) +
  geom_boxplot() +
  theme_classic()

mpg %>% 
  ggplot(mapping = aes(y = fct_reorder(manufacturer, desc(hwy), median), x = hwy)) +
  geom_boxplot() +
  theme_classic()

mpg %>% 
  group_by(manufacturer) %>% 
  count()

mpg %>% 
  ggplot(aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)",
    color = "Car type",
    title = "Fuel efficiency generally decreases with engine size",
    subtitle = "Two seaters (sports cars) are an exception because of their light weight",
    caption = "Data from fueleconomy.gov"  )

potential_outliers <- mpg %>% 
  filter(hwy > 40 | (hwy > 20 & displ > 5) | displ > 5.75)

mpg %>% 
  ggplot(aes(x = displ, y = hwy)) +
  geom_point() +
  geom_text_repel(data = potential_outliers, aes(label = manufacturer)) +
  geom_point(data = potential_outliers, color = "red", size = 3, shape = "circle") 

mpg %>% 
  ggplot(aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  scale_color_brewer(palette = "Dark2")

# Opening Diamonds Database #

diamonds

# Studying the database #

view(diamonds)

diamonds_clean <- diamonds %>% 
  mutate(x = if_else(x == 0, NA, x)) %>% 
  mutate(y = if_else(y == 0, NA, y)) %>% 
  mutate(z = if_else(z == 0, NA, z))

view(diamonds_clean)

diamonds_clean %>% 
  ggplot(mapping = aes(x = carat, y = price,colour = cut)) +
  geom_point() +
  geom_smooth() +
  theme_classic()

diamonds_clean %>% 
  count(color, cut)

diamonds_clean %>% 
  count(color, cut) %>% 
  ggplot(aes(x = color, y = cut)) +
  geom_tile(aes(fill = n))

diamonds_clean %>% 
  ggplot(aes(x = price, y = cut)) +
  geom_boxplot(alpha = 0.05) +
  scale_x_continuous(labels = label_dollar(scale = 1/1000, suffix = "K"),
                     breaks = seq(0, 20000, by = 5000)) +
  theme_classic()

diamonds_clean %>% 
  ggplot(aes(x = cut, fill = clarity)) +
  geom_bar(position = "fill") +
  scale_y_continuous(name = "Relative Frequency", labels = label_percent()) +
  labs(x = "Quality of cut",
       fill = "Clarity") +
  theme(legend.position = "right") +
  guides(fill = guide_legend(nrow = 4)) +
  theme_classic()

diamonds_clean %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_bin2d() + 
  scale_x_log10() + 
  scale_y_log10() +
  labs(x = "Log 10 of Carat",
       y = "Log 10 of Price")


