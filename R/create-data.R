# Download the pdf files of aerodrome licenses, and extract the licence number
# and aerodrome name into a tsv file

library(tidyverse)
library(tabulizer)
library(here)
library(stringi)
library(rvest)

sources_url <- "https://www.caa.co.uk/Commercial-industry/Airports/Aerodrome-licences/Licences/Aerodrome-licences-and-boundary-maps/"
data_dir <- file.path(here(), "data")
pdf_dir <- file.path(here(), "pdf")

if (!dir.exists(data_dir)) dir.create(data_dir)
if (!dir.exists(pdf_dir)) dir.create(pdf_dir)

read_html(sources_url) %>%
  html_nodes("table a") %>%
  html_attr("href") %>%
  paste0("https://www.caa.co.uk", .) %>%
  walk2(seq_along(.), ~ download.file(.x, destfile = file.path(pdf_dir, paste0(.y, ".pdf"))))

pdf_files <- list.files(pdf_dir, full.names = TRUE)

extract_licence <- function(x) {
  stri_split_fixed(x, pattern = " ") %>%
    unlist %>%
    rev() %>%
    .[1]
}

parse_pdf <- function(x) {
  n_pages <- get_n_pages(x)
  page_numbers <- seq(1, n_pages, by = 2) # tables are on odd-numbered pages
  # Coordinates found with locate_areas(source_filepath, 1)
  page_area <- list(structure(c(18, 44, 237, 562),
                              .Names = c("top", "left", "bottom", "right")))
  tables <- extract_tables(x, pages = page_numbers, area = page_area)
  clean_tables <-
    tables %>%
    # Remove empty rows
    map(~ .x[rowSums(.x == "") != dim(.x)[2],
             colSums(.x == "") != dim(.x)[1]]) %>%
    # Keep the first two columns (row headers and values)
    map(~ .x[, 1:2])
  # use cell 1,1 for the name, and remove it from the table
  data_frame(`licensed-aerodrome` = map_chr(clean_tables,
                                            ~ extract_licence(.x[1, 1])),
             name = map_chr(clean_tables, ~ .x[2, 2]))
}

aerodromes <- map_df(pdf_files, parse_pdf)

aerodromes %>%
  mutate(name = toupper(name)) %>%
  arrange(name) %>%
  write_tsv(file.path(data_dir, "licensed-aerodrome.tsv")) %>%
  print(n = Inf)
