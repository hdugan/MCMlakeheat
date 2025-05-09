latexTable <- function(coefffit, usecols = 8) {
  # Create the LaTeX table
  latex_table <- xtable(coefffit,math.style.exponents = TRUE, digits = 3,
                        caption = paste0('Linear model fit 1'), 
                        align = c("l", "l", "l", rep("r", usecols)))
  # Convert to LaTeX
  print(latex_table, 
        hline.after = c(-1, 0, 1:nrow(coefffit)),
        include.rownames = FALSE, 
        sanitize.text.function = identity, 
        latex.environments = "center")
}