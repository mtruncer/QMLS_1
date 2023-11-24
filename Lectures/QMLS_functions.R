ssPlot <- function(X, Y, b, do.plot = TRUE, do.labels = TRUE) {
  n <- length(X)
  pred <- (X * b + (mean(Y) - b * mean(X)))
  SSy <- sum((Y - pred) ^ 2)
  
  M <- tibble(X, Y, pred)
  
  if (do.plot) {
    p <- ggplot() +
      geom_point(data = tibble(X = mean(X), Y = mean(Y)),
                 aes(X, Y), color = "navy", size = 7, pch = 1) +
      geom_abline(slope = b, intercept = mean(Y) - b * mean(X),
                  color = "navy", linewidth = 1) +
      geom_segment(data = M, aes(x = X, xend = X, y = Y, yend = pred),
                   color = "firebrick", linewidth = 1) +
      geom_point(data = M, aes(x = X, y = Y), size = 3)
    
    v1 <- round(b, 2)
    v2 <- round(SSy, 2)
    
    ll1 <- glue::glue("$\\theta_1 = {v1}$")
    ll2 <- glue::glue("$\\SS = {v2}$")
    
    xpos <- min(X) + 0.15 * diff(range(X))
    ypos <- min(Y) + 0.8 * diff(range(Y))
    
    if(do.labels) {
      p <- p +
        annotate(geom = "text",
                 label = TeX(ll1, output = "character"),
                 x = xpos, y = ypos,
                 parse = TRUE,
                 hjust = 0,
                 size = 9) +
        annotate(geom = "text",
                 label = TeX(ll2, output = "character"),
                 x = xpos, y = ypos * 0.96,
                 parse = TRUE,
                 hjust = 0,
                 size = 9)
    }
    print(p)
  }
  return(SSy)
}

shade_normal <- function(p, tail = "both", mean = 0, sd = 1) {
  require(tidyverse, quietly = TRUE)
  require(cowplot)
  crit <- qnorm(p, mean, sd)
  M <- tibble(x = seq(-4, 4, length = 200),
              y = dnorm(x))
  PP <- ggplot(M, aes(x, y)) +
    geom_line() +
    labs(x = "Value", y = "Relative Likelihood")
  lower <- geom_ribbon(data = subset(M, x < crit),
                       aes(ymax = y), ymin = 0,
                       fill = "red", alpha = 0.5)
  upper <- geom_ribbon(data = subset(M, x > abs(crit)),
                       aes(ymax = y), ymin = 0,
                       fill = "red", alpha = 0.5)
  if (tail == "both") PP <- PP + lower + upper
  if (tail == "lower") PP <- PP + lower
  if (tail == "upper") PP <- PP + upper
  return(p)
}

shade_t <- function(p, df, tail = "both") {
  require(tidyverse, quietly = TRUE)
  require(cowplot)
  theme_set(theme_cowplot())
  if (!require(latex2exp, quietly = TRUE)) {
    stop("Please install the package 'latex2exp':\n\tinstall.packages('latex2exp')")
  }
  
  crit <- qt(p, df, lower.tail = FALSE)
  M <- tibble(x = seq(crit * 3, -1 * crit * 3,
                      length = 200),
              y = dt(x, df))
  PP <- ggplot(M, aes(x, y)) +
    geom_line() +
    labs(x = TeX("$t$-statistic"), y = "Relative Likelihood")
  lower <- geom_ribbon(data = subset(M, x > crit),
                       aes(ymax = y), ymin = 0,
                       fill = "red", alpha = 0.5)
  upper <- geom_ribbon(data = subset(M, x < -1 * crit),
                       aes(ymax = y), ymin = 0,
                       fill = "red", alpha = 0.5)
  if (tail == "both") PP <- PP + lower + upper
  if (tail == "lower") PP <- PP + lower
  if (tail == "upper") PP <- PP + upper
  return(PP)
}

shade_F <- function(p, df1, df2, vline = NULL) {
  require(tidyverse, quietly = TRUE)
  require(cowplot)
  if (!require(latex2exp, quietly = TRUE)) {
    stop("Please install the package 'latex2exp':\n\tinstall.packages('latex2exp')")
  }
  
  crit <- qf(p, df1, df2, lower.tail = FALSE)
  if (!is.null(vline)) {
    x_max <- max(vline * 1.05, crit * 1.5)
  } else {
    x_max <- crit * 1.5
  }
  M <- tibble(x = seq(0.001, x_max, length = 200),
              y = df(x, df1, df2))
  PP <- ggplot(M, aes(x, y)) +
    geom_line() +
    geom_ribbon(data = subset(M, x > crit),
                aes(ymax = y), ymin = 0,
                fill = "red", alpha = 0.5) +
    ylim(c(0, 1.1 * max(M$y))) +
    labs(x = TeX("$F$"), y = "Relative Likelihood")
  return(PP)
}

shade_chisq <- function(q, df) {
  require(tidyverse, quietly = TRUE)
  require(cowplot)
  if (!require(latex2exp, quietly = TRUE)) {
    stop("Please install the package 'latex2exp':\n\tinstall.packages('latex2exp')")
  }
  
  crit <- qchisq(q, df, lower.tail = FALSE)
  M <- tibble(x = seq(0.1, crit * 1.5, length = 200),
              y = dchisq(x, df))
  p <- ggplot(M, aes(x, y)) +
    geom_line() +
    geom_ribbon(data = subset(M, x > crit),
                aes(ymax = y), ymin = 0,
                fill = "red", alpha = 0.5) +
    ylim(c(0, 1.1 * max(M$y))) +
    labs(x = TeX("$\\chi^2$"), y = "Relative Likelihood")
  return(p)
}
