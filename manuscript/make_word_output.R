library(here)
library(bookdown)
library(rmarkdown)

setwd(here("manuscript/")) # bookdown wants project root to be where the .Rmd files are

# input
file <- "manuscript.Rmd"

# output
render(input=file,
       output_format = "bookdown::word_document2",
       output_file = paste0("casey1_", format(Sys.Date(), ("%Y-%m-%d")), ".docx"),
       output_dir = "drafts_outbox",
       output_options = list(reference_docx = "templates/word-template.docx"))


setwd(here()) # return to the project root


# can use bookdown::preview_chapter(), https://eddjberry.netlify.com/post/writing-your-thesis-with-bookdown/