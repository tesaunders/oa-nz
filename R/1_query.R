library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)

#' Helper function to fetch a single institution's data safely
fetch_single_institution <- function(ror, year_filter, base_req) {
  message("\n--- Querying ROR: ", ror, " ---")
  
  inst_filter <- paste0("authorships.institutions.ror:", ror, ",", year_filter)
  page_req    <- base_req |> req_url_query(filter = inst_filter)
  
  all_results  <- list()
  cursor       <- "*"
  has_more     <- TRUE
  page_counter <- 1
  
  while (has_more) {
    message("  Fetching page ", page_counter, "...")
    current_req <- page_req |> req_url_query(cursor = cursor)
    
    # Run request safely and extract payload
    resp <- req_perform(current_req)
    
    # Handle potentially empty responses cleanly
    if (resp_status(resp) != 200) {
      warning("Failed to fetch data for page ", page_counter, ". Status code: ", resp_status(resp))
      break
    }
    
    payload <- resp_body_json(resp, simplifyVector = FALSE)
    
    if (length(payload$results) == 0) {
      break
    }
    
    all_results <- c(all_results, payload$results)
    
    next_cursor <- payload$meta$next_cursor
    if (!is.null(next_cursor) && next_cursor != "" && next_cursor != cursor && length(payload$results) > 0) {
      cursor       <- next_cursor
      page_counter <- page_counter + 1
      Sys.sleep(0.6) # Polite delay for API
    } else {
      has_more     <- FALSE
    }
  }
  
  if (length(all_results) == 0) return(NULL)
  
  purrr::map(all_results, \(work) {
    inst_list <- purrr::map(work$authorships, ~ .x$institutions) |> purrr::flatten()
    if (length(inst_list) == 0) inst_list <- list(list(ror = NA_character_, display_name = NA_character_))
    
    tibble::tibble(
      id               = work$id %||% NA_character_,
      publication_year = as.integer(work$publication_year %||% NA_integer_),
      type             = work$type %||% NA_character_,
      cited_by_count   = as.integer(work$cited_by_count %||% NA_integer_),
      oa_status        = work$open_access$oa_status %||% NA_character_,
      field            = work$primary_topic$field$display_name %||% NA_character_,
      version          = work$primary_location$version %||% NA_character_,
      institution_ror  = purrr::map_chr(inst_list, ~ .x$ror %||% NA_character_),
      institution_name = purrr::map_chr(inst_list, ~ .x$display_name %||% NA_character_)
    )
  }) |> 
    purrr::list_rbind() |> 
    dplyr::filter(institution_ror == ror) 
}

#' Main Function with Real-Time Checkpointing
fetch_publications <- function(
  full_retrieval = TRUE,
  output_dir = "data/raw",
  api_key = Sys.getenv("OPENALEX_API_KEY")
) {
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  cache_file   <- file.path(output_dir, "openalex_query_cache.rds")
  rds_out_file <- file.path(output_dir, "university_publications.rds")
  csv_out_file <- file.path(output_dir, "university_publications.csv")
  
  institution_rors <- c(
    "https://ror.org/03b94tp07", # University of Auckland
    "https://ror.org/013fsnh78", # University of Waikato
    "https://ror.org/052czxv31", # Massey University
    "https://ror.org/0040r6f76", # Victoria University of Wellington
    "https://ror.org/03y7q9t39", # University of Canterbury
    "https://ror.org/01jmxt844", # University of Otago
    "https://ror.org/04ps1r162", # Lincoln University
    "https://ror.org/01zvqw119", # Auckland University of Technology
    "https://ror.org/019wvm592", # Australian National University
    "https://ror.org/0384j8v12", # The University of Sydney
    "https://ror.org/047272k79", # The University of Western Australia
    "https://ror.org/01ej9dk98", # The University of Melbourne
    "https://ror.org/00rqy9422", # The University of Queensland
    "https://ror.org/02bfwt286", # Monash University
    "https://ror.org/03r8z3t63", # UNSW Sydney
    "https://ror.org/028g18b61", # Adelaide University (2026 onwards)
    "https://ror.org/00892tw58"  # The University of Adelaide (pre-2026, before merger)
  )
  
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  
  # Set up filter strings safely avoiding premium parameters
  if (!full_retrieval && file.exists(rds_out_file)) {
    message("Incremental Fetch active: Refreshing data specifically for the current year (", current_year, ")...")
    year_filter_str <- paste0("publication_year:", current_year, ",is_paratext:false,is_retracted:false,type:article|book|book-chapter")
  } else {
    message("Full Retrieval active: Querying historical range...")
    end_year   <- current_year - 1
    start_year <- end_year - 5 + 1
    year_filter_str <- paste0("publication_year:", start_year, "-", end_year, ",is_paratext:false,is_retracted:false,type:article|book|book-chapter")
  }
  
  # Configure base request 
  base_req <- request("https://api.openalex.org/works") |> 
    req_url_query(
      select   = "id,publication_year,type,cited_by_count,open_access,primary_topic,primary_location,authorships",
      per_page = 200
    ) |> 
    req_retry(
      max_tries = 5,
      backoff = \(attempt) min(2 ^ attempt, 30), 
      is_transient = \(resp) resp_status(resp) %in% c(429, 500, 502, 503, 504)
    )
  
  if (!is.null(api_key) && api_key != "") base_req <- base_req |> req_url_query(api_key = api_key)
  
  for (ror in institution_rors) {
    
    # Check what is currently saved on disk at the start of every loop iteration
    completed_rors <- character()
    existing_data  <- NULL
    if (file.exists(rds_out_file)) {
      existing_data <- readRDS(rds_out_file)
      if ("institution_ror" %in% names(existing_data)) {
        completed_rors <- unique(existing_data$institution_ror)
      }
    }
    
    # Skip if running a full retrieval and this specific ROR is already saved
    if (ror %in% completed_rors && full_retrieval) {
      message(">>> Skipping ROR: ", ror, " (Already saved on disk) <<<")
      next
    }
    
    # Fetch the data for this single university
    df_inst <- fetch_single_institution(ror, year_filter_str, base_req)
    
    # If data was returned, safely append/merge it locally
    if (!is.null(df_inst) && nrow(df_inst) > 0) {
      message("  Processing and saving data for ", ror, "...")
      
      if (!is.null(existing_data)) {
        # Combine historical records with new/updated current year records
        updated_data <- dplyr::bind_rows(existing_data, df_inst) |> 
          dplyr::distinct(id, institution_ror, .keep_all = TRUE)
      } else {
        updated_data <- df_inst
      }
      
      # Overwrite the files immediately so progress is locked in dynamically
      saveRDS(updated_data, file = rds_out_file)
      write.csv(updated_data, file = csv_out_file, row.names = FALSE)
    }
    
    Sys.sleep(2) # Breather between universities
  }
  
  # Update final metadata run tracking log
  saveRDS(list(last_run_date = as.character(Sys.Date())), file = cache_file)
  message("\nPipeline completed successfully! All fetched data is safe on disk.")
  
  if (file.exists(rds_out_file)) {
    return(readRDS(rds_out_file))
  } else {
    return(invisible(NULL))
  }
}

fetch_publications(full_retrieval = FALSE)