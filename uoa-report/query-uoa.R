# Load packages -----------------------------------------------------------

library(httr2)
library(dplyr)
library(tidyr)
library(purrr)

# Define publication years (previous full year back 10 years) --------------

pub_years <- (as.numeric(format(Sys.Date(), "%Y"))-10):(as.numeric(format(Sys.Date(), "%Y"))-1)

# Build API request (using email address stored as environment variable) --

req_uoa <-
  request("https://api.openalex.org/") |> 
  req_url_path_append("works") |> 
  req_url_query(filter = paste0("is_paratext:false,is_retracted:false,type:article|book|book-chapter,",
                                "publication_year:", 
                                paste0(pub_years, collapse = "|"), 
                                ",", 
                                "authorships.institutions.lineage:i154130895")) |> 
  req_url_query(select = c("id", "publication_year", "type", "cited_by_count", "open_access",
                           "primary_topic"), .multi = "comma") |>
  req_url_query(mailto = Sys.getenv("EMAIL_ADDRESS")) |> 
  req_url_query("per-page" = "200") |>
  req_url_query(cursor = "*")

# Make request, retrieving publication data with cursor-based pagination --

resp_uoa <- 
  req_perform_iterative(
    req = req_uoa,
    next_req = iterate_with_cursor(
      param_name = "cursor",
      resp_param_value = \(resp) resp_body_json(resp)$meta$next_cursor
    ),
    max_reqs = Inf
  )

# Process the json into a dataframe ---------------------------------------

data_uoa <-
  resp_uoa |> 
  map(resp_body_json) |>
  map("results") |>
  list_flatten() |> 
  map(~ tibble(
    id = .x$id,
    publication_year = .x$publication_year,
    type = .x$type,
    cited_by_count = .x$cited_by_count,
    is_oa = .x$open_access$is_oa,
    oa_status = .x$open_access$oa_status,
    field = .x$primary_topic$field$display_name
  )) |>
  bind_rows()
  
# Export data as csv ------------------------------------------------------

write.csv(data_uoa, "uoa-report/uoa-data.csv", row.names = FALSE)