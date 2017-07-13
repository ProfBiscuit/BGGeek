library(tidyverse)
bg1 <- read_csv("C:/Users/Matthew Buras/Desktop/board-game-data.zip")
dim(bg1)
names(bg1)
str(bg1)
range(bg1$rank)
?t.test
var1 <- sd(bg1$geek_rating)^2
var2 <- sd(bg1$avg_rating)^2
var.test(bg1$geek_rating, bg1$avg_rating, ratio = 1, alternative = "two.sided")

t.test(bg1$geek_rating, bg1$avg_rating, alternative = "two.sided", 
        paired = TRUE, mu = 0, var.equal = FALSE)

bg2 <- bg1 %>% mutate(diff1 = avg_rating - geek_rating)
dim(bg2)
names(bg2)
bg2 %>% select(geek_rating, avg_rating, diff1)

is.data.frame(bg2)

bg2 %>% ggplot(aes(x = geek_rating)) + geom_histogram()
bg2 %>% ggplot(aes(x = avg_rating)) + geom_histogram()
bg2 %>% ggplot(aes(x = diff1)) + geom_histogram()
bg2 %>% ggplot(aes(x = log(diff1)))+ geom_histogram()

bg2 %>% ggplot(aes(x = geek_rating, y = max(rank) - rank)) + geom_point()
bg2 %>% ggplot(aes(x = avg_rating, y = max(rank) - rank)) + geom_point() + geom_smooth(se = FALSE)

bg2 %>% ggplot(aes(x = geek_rating, y = avg_time)) + geom_point()
bg2 %>% ggplot(aes(x = geek_rating, y = avg_time)) + geom_point() + scale_y_log10()

fit1 <- lm(geek_rating ~ avg_time, data = bg2)
summary(fit1)

#Q: is average time in seconds or minutes? makes more sense in seconds, almost.
bg2 %>% filter(0 < avg_time & avg_time < 2500) %>% summary(avg_time)
bg2 %>% filter(0 < avg_time & avg_time < 2500) %>% ggplot(aes(x = avg_rating, y = avg_time)) + geom_point()
fit2 <- lm(avg_rating ~ avg_time, data = bg2)
summary(fit2)
