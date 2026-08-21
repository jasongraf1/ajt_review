# ---------------------------------------------------------
# file: data.R
# author: Jason Grafmiller
# date: 2026-04-13
# description:
# Code for loading the data for the dashboard
# ---------------------------------------------------------

suppressMessages(library(here)) # for pathing to/from project directory
suppressMessages(library(tidyverse)) # for dplyr, ggplot, etc.
suppressMessages(library(janitor)) # for cleaning data

# load the most recent version of the spreadsheet
current_file <- here("data") |> 
  list.files(
    pattern = "^annotations_from_articles_.*\\.csv", 
    full.names = TRUE
  ) |> 
  sort() |> 
  rev() |> 
  first()

raw <- read.csv(current_file) |> 
  as_tibble() |> 
  filter(journal != "")

# load in the article list
full <- here("data", "articles_selected_final.csv") |> 
  read.csv() |> 
  as_tibble()

# add coded indication
full <- full |> 
  left_join(
    raw |> distinct(url) |> mutate(coded = "Coded"),
    by = "url"
  ) |> 
  mutate(coded = if_else(is.na(coded), "Not coded", "Coded") |> 
           as.factor(),
         coded = relevel(coded, ref = "Not coded"),
         journal = str_replace_all(journal, "^Linguistic.* ", "Ling. ") |>
           str_replace_all("Jo", "J. of ") |> 
           str_replace_all("Cognitive", "Cog. ") |>
           str_replace_all("NL & LT", "NLLT")
  )


# remove irrelevant articles
filtered <- raw |> 
  filter(!str_detect(coder_comments, "[Ee]xclude|No judgments collected")) |> 
  mutate(across(where(is.character), ~str_trim(.x))) |> 
  mutate(across(where(is.character), ~str_replace_all(.x, "[nN]ot [rR]eported", "Not Reported")))

# functions
extract_surname <- function(name) {
  name <- str_trim(name)
  parts <- str_split(name, "\\s+")[[1]]
  
  # Common suffixes
  suffixes <- c("Jr.", "Sr.", "II", "III", "IV")
  
  # If last part is a suffix → include previous word
  if (tail(parts, 1) %in% suffixes && length(parts) >= 2) {
    return(paste(parts[(length(parts)-1):length(parts)], collapse = " "))
  }
  
  # Handle particles (lowercase words before surname)
  particles <- c("de", "van", "von", "der", "den", "da", "di")
  
  if (length(parts) >= 2 && parts[length(parts)-1] %in% particles) {
    return(paste(parts[(length(parts)-1):length(parts)], collapse = " "))
  }
  
  # Default: last word
  tail(parts, 1)
}

# clean counts
clean <- filtered |> 
  mutate(
    N_items = str_extract(N_target_items, "(\\d{1,}$|^\\d{1,} )") |> 
      str_trim() |> 
      as.numeric(),
    N_conds = str_extract(N_conditions, "(\\d{1,}$|^\\d{1,} )") |> 
      str_trim() |> 
      as.numeric(),
    N_items_per_conds = str_extract(N_items_per_condition, "(\\d{1,}$|^\\d{1,} )") |> 
      str_trim() |> 
      as.numeric(),
    N_trials = str_extract(N_trials_per_participant, "(\\d{1,}$|^\\d{1,} )") |> 
      str_trim() |> 
      as.numeric(),
    N_parts = str_extract(N_participants_recruited, "(\\d{1,}$|^\\d{1,} )") |> 
      str_trim() |> 
      as.numeric(),
    N_parts_filtered = as.numeric(N_participants_after_filtering),
    N_parts_filtered_bin = ifelse(is.na(N_parts_filtered) | N_parts_filtered == N_parts, FALSE, TRUE),
    language = str_replace(language, "French as spoken.*", "French"),
    language = str_trim(language),
    journal = str_replace_all(journal, "^Linguistic.* ", "Ling. ") |>
      str_replace_all("Jo", "J. of ") |> 
      str_replace_all("Cognitive", "Cog. ") |>
      str_replace_all("NL & LT", "NLLT"),
    citation = map2_chr(author, year, ~{
      authors <- str_split(.x, ";\\s*")[[1]]
      surnames <- map_chr(authors, extract_surname)
      
      n <- length(surnames)
      
      if (n == 1) {
        sprintf("%s (%s)", surnames[1], .y)
      } else if (n == 2) {
        sprintf("%s & %s (%s)", surnames[1], surnames[2], .y)
      } else {
        sprintf("%s et al. (%s)", surnames[1], .y)
      }
    })
  )

# clean "Not Reported" extras
remove_nr <- function(text){
  return(gsub("Not Reported; ", "", text))
}

clean <- clean |> 
  mutate(across(c(demographics_reported, rating_terminology, linguistic_phenomenon, random_effects), remove_nr))

# Save to file -----------------------------------------

saveRDS(raw, here("data", "raw.rds"))
saveRDS(full, here("data", "full.rds"))
saveRDS(clean, here("data", "clean.rds"))



