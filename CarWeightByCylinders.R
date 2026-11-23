
# CarWeightByCylinders
# Wesley Sutton
# 11/22/2026
# Models car weight based on number of cylinders


library(tidyverse)
library(modelr)

# 1.Created a tibble with cylinder count and weight
car_data <- mtcars %>%
  as_tibble() %>%
  select(cyl, wt) %>%
  mutate(cyl = factor(cyl))

# 2.Fit a linear model to predict weight from cylinders
car_model <- lm(wt ~ cyl, data = car_data)

# 3.Create a grid with predicted weights
car_grid <- car_data %>%
  data_grid(cyl) %>%
  add_predictions(car_model)

# 4.Plotted car weights and predicted values
ggplot(car_data, aes(x = cyl, y = wt)) +
  geom_point() +
  geom_point(
    data = car_grid,
    aes(y = pred),
    color = "red",
    size = 4
  )

# Analysis:
# Cars with more cylinders usually weigh more.
# The red points show the predicted average weight
# for each cylinder group. Weight varies within
# each group, so cylinder count does not explain
# every difference in car weight.
