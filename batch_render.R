qmds <- list.files(pattern = "^01-[1-4].*qmd$",
                   path = "Lectures/",
                   full.names = TRUE)

for (qq in qmds) {
  message("Processing ", qq)
  syscall <- paste0("quarto render ", qq, " --to revealjs")
  system(syscall)
}
