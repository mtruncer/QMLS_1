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
