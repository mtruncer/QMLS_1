# Lecture slides

qmds <- list.files(pattern = "^[0-9]*-[1-6].*qmd$",
                   path = "Lectures/",
                   full.names = TRUE)

for (qq in qmds) {
  message("Processing ", qq)
  syscall <- paste0("quarto render ", qq, " --to revealjs")
  system(syscall)
}

## Problem sets

qmds <- list.files(pattern = "^PS_[0-9]*[_Key]*.qmd$",
                   path = "Problem_Sets",
                   full.names = TRUE)

for (qq in qmds) {
  message("Processing ", qq)
  syscall <- paste0("quarto render ", qq, " --to html")
  system(syscall)
}
