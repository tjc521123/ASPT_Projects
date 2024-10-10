library(tidyverse)
library(readxl)
library(plotly)
library(zoo)

rolling_mean <- function(x) {
  rollmean(x, k = 3, fill = NA, align = 'right')
}

data <- read_excel(path = "C:/Users/Caitie Mayo/OneDrive - Architech Sports and Physical Therapy/Athlete Workout Cards/Athlete Workout Cards - Ballantyne/Data/questionnaire-report.xlsx",
                   skip = 1) %>%
  select(-contains("Average"))

data <- data %>%
  mutate(
    Wellness    = as.integer(Wellness),
    Soreness    = as.integer(Soreness),
    Training    = as.integer(Training),
    Total_Score = Wellness + Soreness + Training,
    Date        = mdy(Athlete)
  ) %>%
  select(-`Total Score`)

data$Athlete[mdy(data$Athlete) == data$Date] <- NA
data <- data %>%
  fill(Athlete)

data <- data[complete.cases(data$Date), ]

data$Athlete <- as.factor(data$Athlete)

data <- data %>%
  group_by(Athlete) %>%
  mutate(
    across(
      .cols  = is.numeric,
      .fns   = rolling_mean,
      .names = "{.col}_rolling"
    )
  )

plot_wellness <- data[data$Athlete == 'Tanner Clark', ] %>%
  ggplot(mapping = aes(x = Date, y = Wellness)) +
  geom_line() +
  geom_point(aes(colour = Wellness, size = 3)) +
  geom_line(mapping = aes(x = Date, y = Wellness_rolling)) +
  scale_color_gradient(low = 'red', high = 'green') +
  guides(size = 'none')
  # geom_line(mapping = aes(y = Wellness_rolling))

plot_soreness <- data[data$Athlete == 'Tanner Clark', ] %>%
  ggplot(mapping = aes(x = Date, y = Soreness)) +
  geom_line() +
  geom_point(aes(colour = Soreness, size = 3)) +
  geom_line(mapping = aes(x = Date, y = Soreness_rolling)) +
  scale_color_gradient(low = 'green', high = 'red') +
  guides(size = 'none')

plot_training <- data[data$Athlete == 'Tanner Clark', ] %>%
  ggplot(mapping = aes(x = Date, y = Training)) +
  geom_line() +
  geom_point(aes(colour = Training, size = 3)) +
  geom_line(mapping = aes(x = Date, y = Training_rolling)) +
  scale_color_gradient(low = 'red', high = 'green') +
  guides(size = 'none')

print(plot_wellness)
print(plot_soreness)
print(plot_training)

