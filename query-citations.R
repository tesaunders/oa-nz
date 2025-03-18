# Load packages -----------------------------------------------------------

library(httr2)
library(dplyr)
library(tidyr)
library(purrr)

# Define publication years (previous full year back 5 years) --------------

pub_years <- (as.numeric(format(Sys.Date(), "%Y"))-5):(as.numeric(format(Sys.Date(), "%Y"))-1)

# Define institutions -----------------------------------------------------

universities <- 
  data.frame(institution = c("University of Auckland",
                             "Auckland University of Technology",
                             "University of Waikato",
                             "Massey University",
                             "Victoria University of Wellington",
                             "University of Canterbury",
                             "Lincoln University",
                             "University of Otago",
                             "University of Melbourne",
                             "Australian National University",
                             "University of Sydney",
                             "University of Queensland",
                             "University of Western Australia",
                             "University of Adelaide",
                             "Monash University",
                             "UNSW Sydney")
  )

# Retrieve OpenAlex IDs and join to institutions --------------------------
# Email address is stored as an environment variable ----------------------

openalex_ids <-
  request("https://api.openalex.org/") |> 
  req_url_path_append("institutions") |> 
  req_url_query(filter = "country_code:nz|au") |>
  req_url_query(search = c(paste0('"', paste0(universities$institution), collapse = " OR ",'"'))) |> 
  req_url_query(select = c("id", "display_name", "country_code"), .multi = "comma") |> 
  req_url_query(mailto = Sys.getenv("EMAIL_ADDRESS")) |> 
  req_perform() |>
  resp_body_json()

universities <-
  openalex_ids$results |> 
  map(as.data.frame) |> 
  bind_rows() |> 
  inner_join(universities, by = join_by(display_name == institution))

# Build API request -------------------------------------------------------

req_nz <-
  request("https://api.openalex.org/") |> 
  req_url_path_append("works") |> 
  req_url_query(filter = paste0("is_paratext:false,is_retracted:false,type:article|book|book-chapter,",
                                "publication_year:", 
                                paste0(pub_years, collapse = "|"), 
                                ",", 
                                "authorships.institutions.lineage:",
                                paste0(filter(universities, country_code == "NZ") |> pull(id), collapse = "|"))) |> 
  req_url_query(select = c("id", "publication_year", "type", "cited_by_count", 
                           "open_access", "primary_topic"), .multi = "comma") |>
  req_url_query(mailto = Sys.getenv("EMAIL_ADDRESS")) |> 
  req_url_query("per-page" = "200") |>
  req_url_query(cursor = "*")

req_au <-
  request("https://api.openalex.org/") |> 
  req_url_path_append("works") |> 
  req_url_query(filter = paste0("is_paratext:false,is_retracted:false,type:article|book|book-chapter,",
                                "publication_year:", 
                                paste0(pub_years, collapse = "|"), 
                                ",", 
                                "authorships.institutions.lineage:",
                                paste0(filter(universities, country_code == "AU") |> pull(id), collapse = "|"))) |> 
  req_url_query(select = c("id", "publication_year", "type", "cited_by_count", 
                           "open_access", "primary_topic"), .multi = "comma") |>
  req_url_query(mailto = Sys.getenv("EMAIL_ADDRESS")) |> 
  req_url_query("per-page" = "200") |>
  req_url_query(cursor = "*")

# Make request, retrieving publication data with cursor-based pagination --

resp_nz <- 
  req_perform_iterative(
    req = req_nz,
    next_req = iterate_with_cursor(
      param_name = "cursor",
      resp_param_value = \(resp) resp_body_json(resp)$meta$next_cursor
    ),
    max_reqs = Inf
  )

resp_au <- 
  req_perform_iterative(
    req = req_au,
    next_req = iterate_with_cursor(
      param_name = "cursor",
      resp_param_value = \(resp) resp_body_json(resp)$meta$next_cursor
    ),
    max_reqs = Inf
  )

# Process the json into a dataframe ---------------------------------------

data_nz <-
  resp_nz |> 
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

data_au <-
  resp_au |> 
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

write.csv(data_nz, "data/citations-nz.csv", row.names = FALSE)
write.csv(data_au, "data/citations-au.csv", row.names = FALSE)