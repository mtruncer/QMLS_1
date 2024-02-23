library(tidyverse)
library(performance)
library(cowplot)

theme_set(cowplot::theme_cowplot())

set.seed(3422219)

width <- 6
height <- 4

n <- 20
DD <- tibble(x = seq(0, 50, length.out = n),
             y = 2 * x + rnorm(n) * x)

P <- ggplot(DD, aes(x, y)) + 
  geom_point(size = 3, color = "navy") +
  labs(x = "Concentration", y = "Absorbance") +
  theme(axis.title = element_text(face = "bold"))

save_plot("Quiz_06-4_scatter_1.png", P, base_width = width, base_height = height, bg = "white")

P + geom_smooth(formula = y ~ x - 1, method = "lm", se = FALSE,
                color = "firebrick4", linewidth = 2)

save_plot("Quiz_06-4_scatter_2.png", last_plot(), base_width = width, base_height = height,
          bg = "white")

fm <- lm(y ~ x - 1, data = DD)

summary(fm)

check_predictions(fm)

check_heteroskedasticity(fm)

check_normality(fm)

check_outliers(fm)

PP <- plot(check_model(fm, panel = FALSE))
PP[[1]]
PP[[2]]
PP[[3]]
PP[[4]]
PP[[5]]

for (ii in 1:5) {
  save_plot(filename = paste0("Quiz_06-4_Check_", ii, ".png"),
            plot = PP[[ii]],
            base_width = width, base_height = height, bg = "white")
}
