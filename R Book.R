# Loading libraries #

library(tidyverse)

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
