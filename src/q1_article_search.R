# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Project/Title: 
# Author: 
# Date: 
# Version: 
# Description: 
# Notes: 
#    Need to install Rtools: https://cran.r-project.org/bin/windows/Rtools/
# TODO: when selecting areas/categories, it does not filter Q1 journals from SCIMAGOJR for areas/categories that have a semicolon in them
# TODO: Press enter to add 'Multidisciplinary' by default in the areas/categories selection section
# TODO: When exporting to CSV, do a colour gradient for relevance score and semantic similarity scores
# TODO: Investigate why only OA's source type 'journal' items are being included (e.g., 'book series', 'repo' is not included)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Set Up ------------------------------------------------------------- ----
# Clear Environment and Console -------------------------------------------
rm(list = ls())
cat("\014")

# Create Functions --------------------------------------------------------
smart_readline <- function(prompt = ""){
  if (interactive()) {
    return(readline(prompt))
  } else {
    cat(prompt)
    con <- file("stdin")
    on.exit(close(con))
    input <- readLines(con, n = 1, warn = FALSE)
    return(input)
  }
}

# Extract all unique atomic values that are delimited
fn_extract_unique_atomic_values =
  function(df_col, delimiter){
    df_col %>% 
      gsub(" \\(Q[1-4]\\)", "", .) %>% 
      strsplit(delimiter) %>%
      unlist %>% 
      gsub('"', "", .) %>% 
      unique %>% 
      trimws %>% 
      sort
  }

# Null coalescing operator for missing OA source metadata fields
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Fetch all OpenAlex sources via cursor pagination
fetch_all_oa_sources <- function(mailto = NULL) {
  results <- list()
  cursor <- "*"
  pb <- NULL
  
  message("Fetching source metadata from OpenAlex...")
  
  repeat {
    resp <- request("https://api.openalex.org/sources") |>
      req_url_query(
        select   = "id,display_name,type",
        `per-page` = 200,
        cursor   = cursor,
        mailto   = mailto
      ) |>
      req_perform() |>
      resp_body_json()
    
    if (is.null(pb)) {
      total <- resp$meta$count
      message("Downloading ", format(total, big.mark = ","), " sources...")
      pb <- progress_bar$new(
        format = "  [:bar] :current/:total (:percent) | ETA: :eta",
        total  = total,
        clear  = FALSE,
        width  = 80
      )
    }
    
    results <- c(results, resp$results)
    if (!pb$finished) pb$tick(length(resp$results))
    
    next_cursor <- resp$meta$next_cursor
    if (is.null(next_cursor)) break
    cursor <- next_cursor
  }
  
  message("Done! Fetched ", format(length(results), big.mark = ","), " sources.")
  
  bind_rows(lapply(results, function(x) {
    data.frame(
      id           = x$id           %||% NA_character_,
      display_name = x$display_name %||% NA_character_,
      type         = x$type         %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }))
}

# Fetch OpenAlex works in batches to respect URL length limits.
# The OpenAlex API silently returns NULL when too many source IDs are passed
# in a single request (URL becomes too long). This function splits the IDs
# into chunks of `chunk_size` and binds the results together.
fetch_oa_works_batched <- function(
    source_ids,
    search_terms,
    from_date,
    chunk_size = 50
) {
  id_chunks <- split(source_ids, ceiling(seq_along(source_ids) / chunk_size))
  
  message(
    "Fetching works across ", length(id_chunks), " batches ",
    "(", length(source_ids), " source IDs, chunk size = ", chunk_size, ")..."
  )
  
  pb <- progress_bar$new(
    format = "  Batch [:bar] :current/:total (:percent) | ETA: :eta",
    total  = length(id_chunks),
    clear  = FALSE,
    width  = 80
  )
  
  results <- map_dfr(id_chunks, function(chunk) {
    pb$tick()
    tryCatch(
      oa_fetch(
        entity                        = "works",
        primary_location.source.id    = chunk,
        default.search                = search_terms,
        from_publication_date         = from_date
      ),
      error = function(e) {
        message("\nBatch failed: ", conditionMessage(e))
        NULL
      }
    )
  })
  
  if (nrow(results) == 0) {
    message("No results returned across all batches.")
    return(NULL)
  }
  
  message("Total works fetched: ", format(nrow(results), big.mark = ","))
  results
}

# Select and Install Packages ---------------------------------------------
options(repos = c(CRAN = "https://cloud.r-project.org"))

for(pkg in c(
  "dplyr",
  "pak",
  "httr",
  "httr2",
  "jsonlite",
  "tidyverse",
  "openalexR",
  "this.path",
  "openxlsx",
  "stringr",
  "reticulate",
  "progress",
  "magrittr"
)){
  if(!pkg %in% installed.packages()) install.packages(pkg)
  if(!pkg %in% loadedNamespaces()) library(pkg, character.only = TRUE)
}

pak::pak("ikashnitsky/sjrdata")
library(sjrdata)

# Set Working Directory ---------------------------------------------------
setwd(paste0(this.dir(), "/.."))

# Configuration ------------------------------------------------------ ----
sjr_refresh_months = 6

# Set Default Values and Interactive Input --------------------------- ----
# Search scope: all Q1 sources, or within specific areas/categories
if(T){
  input_options <- c("all", "areas", "categories")
  
  cat("Select search scope:", sep = "\n")
  cat(paste0(1:length(input_options), ": ", input_options), sep = "\n")
  
  while (TRUE) {
    input_str   <- smart_readline(prompt = "Enter the number of your choice (1-3): ")
    choice_index <- suppressWarnings(as.integer(input_str))
    
    if (!is.na(choice_index) && choice_index > 0 && choice_index <= length(input_options)) {
      break
    } else {
      message("Invalid input. Please enter 1, 2, or 3.")
    }
  }
  
  input_search_in <- input_options[choice_index]
  print(paste("Selection set to:", input_search_in))
  
  
  
  
  if(!exists("input_search_in")) input_search_in = "all"
  
  # Areas/Categories selection (only when not searching across all)
  if(input_search_in != "all"){
    unique_values =
      eval(parse(text = paste0(
        "fn_extract_unique_atomic_values(sjr_journals$", input_search_in, ", '; ')"
      )))
    
    cat(paste0(1:length(unique_values), ": ", unique_values), sep = "\n")
    
    while (TRUE) {
      input_str   <- smart_readline(prompt = "Enter the numbers of your choices (separated by commas): ")
      input_split <- trimws(strsplit(input_str, ",")[[1]])
      search_in   <- suppressWarnings(as.integer(input_split))
      
      if (
        length(search_in) > 0 &&
        !any(is.na(search_in)) &&
        all(search_in > 0) &&
        all(search_in <= length(unique_values))
      ) {
        break
      } else {
        message("Invalid input. Please ensure you enter valid numbers from the list above, separated by commas.")
      }
    }
    
    search_in      <- unique(search_in)
    selected_values <- unique_values[search_in]
    message("You selected:\n", paste0("- ", selected_values, collapse = "\n"))
    
    # Offer to add 'Multidisciplinary' if not already selected
    # CHANGE 3: Updated prompt text; pressing Enter defaults to "y"
    multi_idx <- which(unique_values == "Multidisciplinary")
    if (length(multi_idx) > 0 && !(multi_idx %in% search_in)) {
      while(TRUE) {
        add_multi <- smart_readline(prompt = paste0(
          "You didn't select 'Multidisciplinary' in your list of ", input_search_in,
          ". Would you like to include it (y/n)? (default = y): "
        ))
        
        # Default empty input to "y"
        if (trimws(add_multi) == "") add_multi <- "y"
        add_multi <- tolower(trimws(add_multi))
        
        if (add_multi %in% c("y", "yes")) {
          selected_values <- c(selected_values, "Multidisciplinary")
          break
        } else if (add_multi %in% c("n", "no")) {
          break
        } else {
          message("Invalid input. Please enter 'y' or 'n'.")
        }
      }
    }
  }
}

# Enter search terms
search_terms <- as.character(smart_readline(prompt = "Enter your search term(s) (for OpenAlex): "))

# How many years to search back?
while (TRUE) {
  years_limit <- smart_readline(prompt = "How many years do you want to search back for (default = 7)? ")
  
  if (years_limit == "") {
    years_limit <- 7
    break
  }
  
  years_limit_int <- suppressWarnings(as.integer(years_limit))
  
  if (!is.na(years_limit_int) && years_limit_int > 0) {
    years_limit <- years_limit_int
    break
  } else {
    message("Invalid input. Please enter a valid number of years.")
  }
}

print(paste("Selected year limit:", years_limit))

# Load Data ---------------------------------------------------------- ----
# Scimagojr ---------------------------------------------------------------
sjr_source_rankings = sjr_journals

# OpenAlex ----------------------------------------------------------------
dir_oa_files = "data/reference_data/"

most_recent_date =
  list.files(dir_oa_files) %>%
  grep("^oa_source_id_\\d{8}\\.csv$", ., value = TRUE) %>%
  .[which.max(as.numeric(gsub("^oa_source_id_(\\d{8})\\.csv$", "\\1", .)))] %>%
  gsub("^oa_source_id_|\\.csv$", "", .)

oa_source_id = read.csv(paste0(dir_oa_files, "oa_source_id_", most_recent_date, ".csv"))

n_oa_sources =
  request("https://api.openalex.org/sources") |>
  req_perform() |>
  resp_body_json() |>
  _$meta$count

n_oa_sources_cache = nrow(oa_source_id)

if(
  Sys.Date() - as.Date(most_recent_date, format = "%Y%m%d") > 30.437 * sjr_refresh_months |
  abs(n_oa_sources_cache - n_oa_sources) > (0.10 * n_oa_sources)
){
  oa_source_id = fetch_all_oa_sources(mailto = "your@email.com")
  
  write.csv(
    oa_source_id,
    file = paste0(
      "data/reference_data/oa_source_id_",
      format(Sys.Date(), format = "%Y%m%d"),
      ".csv"
    ),
    row.names = FALSE
  )
}

# Pre-Processing ----------------------------------------------------- ----
# Initialise Error Handling Process ---------------------------------------
message_error = tibble()

fn_error = function(error_type, error_details, record_id){
  message_error <<-
    rbind(
      message_error,
      tibble(
        error_type    = error_type,
        error_details = error_details,
        record_id     = record_id
      )
    )
}

# Pre-Process Scimagojr Source Rankings -----------------------------------
sjr_source_rankings =
  sjr_source_rankings %>%
  
  dplyr::select(
    year,
    rank,
    title,
    type,
    sjr_best_quartile,
    h_index,
    areas,
    categories
  ) %>%
  
  group_by(title) %>%
  dplyr::filter(year == max(year)) %>%
  ungroup %>%
  
  dplyr::mutate(
    areas      = strsplit(areas,      "; "),
    categories = strsplit(categories, "; ")
  ) %>%
  
  dplyr::filter(
    if (input_search_in == "all") {
      sjr_best_quartile == "Q1"
      
    } else if (input_search_in == "areas") {
      map_lgl(areas, ~ any(.x %in% selected_values)) & sjr_best_quartile == "Q1"
      
    } else if (input_search_in == "categories") {
      target_strings <- paste0(selected_values, " (Q1)")
      map_lgl(categories, ~ any(.x %in% target_strings))
    }
  )

# Error Handling ----------------------------------------------------------
if(nrow(message_error) > 0){
  View(message_error)
  stop("There are critical errors that need to be addressed before continuing. Please view them in the object 'message_error'.")
}

# Processing --------------------------------------------------------- ----
# Join Scimagojr with OpenAlex --------------------------------------------
sjr_oa =
  sjr_source_rankings %>%
  mutate(title_lower = tolower(title)) %>%
  rename_with(~ paste0("sjr.", .), -title_lower) %>%
  
  left_join(
    oa_source_id %>%
      mutate(display_name_lower = tolower(display_name)) %>%
      rename_with(~ paste0("oa.", .), -display_name_lower),
    by = join_by(title_lower == display_name_lower)
  ) %>%
  select(-title_lower, -oa.display_name)

# Identify OA source IDs
search_in_source_ids =
  sjr_oa %>%
  dplyr::select(id = oa.id) %>%
  na.omit %>%
  unlist() %>%
  as.character()

# Fetch List of Articles --------------------------------------------------
# Derive the publication date cutoff (first day of the year 'years_limit' years ago)
from_publication_date <-
  as.Date(paste0(format(Sys.Date() - (years_limit * 365.25), "%Y"), "-01-01"))

if(length(search_in_source_ids) > 0){
  
  # --- FIX: Batch requests to avoid silent NULL returns caused by URL length limits ---
  # The OpenAlex API does not error when too many IDs are passed; it simply returns
  # nothing. Splitting into chunks of 50 keeps each request well within URL limits.
  search_results <- fetch_oa_works_batched(
    source_ids   = search_in_source_ids,
    search_terms = search_terms,
    from_date    = from_publication_date,
    chunk_size   = 50
  )
  
} else {
  stop("No matching OpenAlex source IDs were found. Check your SJR filter settings.")
}

if(is.null(search_results) || nrow(search_results) == 0){
  stop("No works were returned. Try broadening your search terms or increasing the year limit.")
}

# Clean Results -----------------------------------------------------------
results =
  search_results %>%
  dplyr::select(
    oa.source_id        = source_id,
    doi,
    title,
    publication_year,
    authorships,
    source_display_name,
    cited_by_count,
    abstract,
    oa_relevance_score  = relevance_score
  ) %>%
  dplyr::mutate(
    citations_pa =
      as.numeric(cited_by_count) / (year(Sys.Date()) - as.numeric(publication_year)),
    authorships = purrr::map_chr(authorships, ~ paste(.x$display_name, collapse = "; "))
  ) %>%
  dplyr::filter(
    citations_pa != 0,
    grepl("review", title, ignore.case = TRUE)
  )

# Add journal details
sjr_lookup <- sjr_oa %>%
  dplyr::select(
    oa.id,
    source_type = oa.type,
    sjr.rank,
    sjr.h_index
  )

results =
  results %>%
  rename(oa.id = oa.source_id) %>%
  left_join(sjr_lookup, by = "oa.id") %>%
  dplyr::select(-oa.id)

# Add Semantic Similarity Score -------------------------------------------
venv_dir <- "./sbert_env"

if (!dir.exists(venv_dir)) {
  message("Creating Python environment for Sentence Transformers...")
  virtualenv_create(venv_dir)
  virtualenv_install(venv_dir, packages = c("sentence-transformers", "numpy"))
}
use_virtualenv(venv_dir, required = TRUE)

st <- import("sentence_transformers")
np <- import("numpy")

# Prepare Data
titles_clean   <- ifelse(is.na(results$title),    "",                       as.character(results$title))
abstracts_clean <- ifelse(is.na(results$abstract), "",                       as.character(results$abstract))
combined_clean  <- ifelse(
  abstracts_clean == "",
  titles_clean,
  paste(titles_clean, abstracts_clean, sep = ". ")
)

# Load Model
message("Loading Model (all-MiniLM-L6-v2)...")
model <- st$SentenceTransformer('all-MiniLM-L6-v2')

# Pre-process Query
clean_query <- search_terms %>%
  gsub("[()]", "", .) %>%
  gsub(" OR ",  " ", .) %>%
  gsub(" AND ", " ", .) %>%
  tolower()

message("Contextualized Query: '", clean_query, "'")

# Generate Embeddings
message("Generating embeddings for titles, abstracts, and combined text...")
query_embedding    <- model$encode(clean_query)
title_embeddings   <- model$encode(titles_clean)
abstract_embeddings <- model$encode(abstracts_clean)
combined_embeddings <- model$encode(combined_clean)

# Calculate Cosine Similarity
calc_cosine_sim <- function(text_embs, query_emb) {
  dot_prods  <- text_embs %*% query_emb
  query_nrm  <- sqrt(sum(query_emb^2))
  text_nrms  <- sqrt(rowSums(text_embs^2))
  text_nrms  <- ifelse(text_nrms == 0, 1e-9, text_nrms)
  as.vector(dot_prods / (text_nrms * query_nrm))
}

title_scores    <- calc_cosine_sim(title_embeddings,    query_embedding)
abstract_scores <- calc_cosine_sim(abstract_embeddings, query_embedding)
combined_scores <- calc_cosine_sim(combined_embeddings, query_embedding)

# Compile and Sort Results
results <- results %>%
  mutate(
    sim_score_title    = round(as.vector(title_scores),    4),
    sim_score_abstract = round(as.vector(abstract_scores), 4),
    sim_score_combined = round(as.vector(combined_scores), 4),
    sim_score_abstract = ifelse(is.na(abstract), NA, sim_score_abstract),
    sim_score_title    = ifelse(is.na(title),    NA, sim_score_title)
  ) %>%
  rowwise() %>%
  mutate(
    sim_score_max = max(
      c(sim_score_title, sim_score_abstract, sim_score_combined),
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  mutate(sim_score_max = ifelse(is.infinite(sim_score_max), 0, sim_score_max)) %>%
  arrange(desc(sim_score_max)) %>%
  
  # Relocate fields
  relocate(oa_relevance_score, .before = sim_score_title) %>% 
  relocate(citations_pa, .after = cited_by_count) %>% 
  relocate(source_type, .after = sjr.h_index)

# Export ------------------------------------------------------------- ----
ts = format(Sys.time(), "%Y%m%d%H%M%S")

wb = createWorkbook()
addWorksheet(wb, "summary")
addWorksheet(wb, "results")

# Summary sheet
rows <- list(
  c("Timestamp of Search (YYYYMMDDHHMMSS):", ts),
  c("Search Term(s) (for OpenAlex):",        search_terms),
  c("Searched in {all, areas, categories}:", input_search_in),
  c("Searched in (specifics):",
    if(exists("selected_values")) paste0(selected_values, collapse = "\n") else "N/A (all Q1 sources)"),
  c("Number of Unique Sources IDs:",         nrow(sjr_source_rankings)),
  c("Search in the Last (years):",           years_limit),
  c("Number of Articles Found:",             nrow(results)),
  c("Number of Unique Articles Found:",      length(unique(tolower(results$title)))),
  c("Number of Sources Found:",              length(unique(results$source_display_name))),
  c("Number of Unique Sources Found:",       length(unique(tolower(results$source_display_name))))
)

summary_data <- data.frame(do.call(rbind, rows))
colnames(summary_data) <- c("Label", "Value")

writeData(
  wb,
  sheet    = "summary",
  x        = summary_data,
  startCol = 1,
  startRow = 1,
  colNames = FALSE
)

wrap_style <- createStyle(wrapText = TRUE, valign = "top")
addStyle(wb, "summary", style = wrap_style,
         rows      = 2:(nrow(summary_data) + 1),
         cols      = 2,
         gridExpand = TRUE)

setColWidths(wb, "summary", cols = 1:2, widths = "auto")

# Results sheet
writeDataTable(
  wb,
  sheet      = "results",
  x          = results,
  tableStyle = "TableStyleMedium2",
  withFilter = TRUE
)

# CHANGE 2: Conditional formatting — high = green, low = red
# Row 1 is the header; data rows start at 2
n_data_rows  <- nrow(results)
data_rows    <- 2:(n_data_rows + 1)

# Colour palettes
col_red_green    <- c("#F8696B", "#FFEB84", "#63BE7B")   # low = red,   high = green
col_green_red    <- c("#63BE7B", "#FFEB84", "#F8696B")   # low = green, high = red  (for rank: lower rank number = better)

# Helper: apply a 3-colour scale to a named column
apply_colour_scale <- function(wb, sheet, col_name, col_names_vec, data_rows, palette) {
  col_idx <- which(col_names_vec == col_name)
  if (length(col_idx) == 0) return(invisible(NULL))
  conditionalFormatting(
    wb, sheet,
    cols  = col_idx,
    rows  = data_rows,
    style = palette,
    type  = "colourScale"
  )
}

results_col_names <- names(results)

# cited_by_count   — higher is better → red-to-green
apply_colour_scale(wb, "results", "cited_by_count",    results_col_names, data_rows, col_red_green)

# citations_pa     — higher is better → red-to-green
apply_colour_scale(wb, "results", "citations_pa",      results_col_names, data_rows, col_red_green)

# sjr.rank         — lower rank number is better → green-to-red
apply_colour_scale(wb, "results", "sjr.rank",          results_col_names, data_rows, col_green_red)

# sjr.h_index      — higher is better → red-to-green
apply_colour_scale(wb, "results", "sjr.h_index",       results_col_names, data_rows, col_red_green)

# oa_relevance_score — higher is better → red-to-green
apply_colour_scale(wb, "results", "oa_relevance_score", results_col_names, data_rows, col_red_green)

# All sim_score_* columns — higher is better → red-to-green
sim_score_cols <- grep("^sim_score", results_col_names, value = TRUE)
for (col_name in sim_score_cols) {
  apply_colour_scale(wb, "results", col_name, results_col_names, data_rows, col_red_green)
}

saveWorkbook(
  wb,
  paste0("export/results/results_", ts, ".xlsx")
)

# Notices -----------------------------------------------------------------
if(input_search_in %in% c("all", "areas")){
  message(
    paste0(
      "INFO: You have selected to search in '", input_search_in, "'.\n",
      "Please note that sources are filtered based on the highest quartile they have reached ",
      "in their most recent years across all the categories they are related to. ",
      "To be more precise, please select a specific category by selecting the value ",
      "'category' for the 'input_search_in' parameter."
    )
  )
}