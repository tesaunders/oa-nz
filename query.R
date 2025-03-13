# Load packages -----------------------------------------------------------

library(httr2)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# Define publication years (previous full year back 10 years) -------------

pub_years <- (as.numeric(format(Sys.Date(), "%Y"))-10):(as.numeric(format(Sys.Date(), "%Y"))-1)

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
                             "The University of Melbourne",
                             "Australian National University",
                             "The University of Sydney",
                             "The University of Queensland",
                             "The University of Western Australia",
                             "The University of Adelaide",
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

# Build API requests ------------------------------------------------------

## Grouped publication data -----------------------------------------------

id_year <- do.call(paste0, (expand.grid(paste0(universities$id, ",", "publication_year:"), pub_years)))

req_pubs <-
  id_year |> 
  map(\(id_year_x) request("https://api.openalex.org/") |>
        req_url_path_append("works") |>
        req_url_query(filter = paste0("is_paratext:false,is_retracted:false,type:article|book|book-chapter,authorships.institutions.lineage:", id_year_x)) |>
        req_url_query(group_by = "open_access.oa_status") |>
        req_url_query(mailto = Sys.getenv("EMAIL_ADDRESS")))

## Pooled citation data ---------------------------------------------------

req_citation <-
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

# Perform requests --------------------------------------------------------

## Grouped publication data: sequential requests --------------------------

resp_pubs <-
  req_pubs |>
  req_perform_sequential() |> 
  resps_successes() |>
  resps_data(\(resp) resp_body_json(resp)) 

data_pubs <-
  resp_pubs |>
  keep(names(resp_pubs) == "group_by") |> 
  bind_rows()

## Pooled citation data: iterative requests with cursor-based pagination --

resp_citation <- 
  req_perform_iterative(
    req = req_citation,
    next_req = iterate_with_cursor(
      param_name = "cursor",
      resp_param_value = \(resp) resp_body_json(resp)$meta$next_cursor
    ),
    max_reqs = Inf
  )

# Process data ------------------------------------------------------------

## Grouped publication data: add labels and calculate %'s -----------------

data_pubs_full <- 
  data_pubs |> 
  mutate(
    institution = rep(universities$display_name, each = 6, times = length(pub_years)),
    year = rep(pub_years, each = length(universities$display_name)*6),
    country = case_when(str_detect(institution, pattern = "Auc|Wai|Mas|Well|Can|Lin|Ota") ~ "nz",
                        .default = "au"),
  ) |> 
  group_by(institution, year) |> 
  mutate(
    pc = (count / sum(count) * 100),
  ) |> 
  select(year, institution, country, key, count, pc)

## Pooled citation data: process json into dataframe ----------------------

data_citation <-
  resp_citation |> 
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

# Write data to .csv ------------------------------------------------------

## Grouped publication data -----------------------------------------------

write.csv(data_pubs_full, "data/publications.csv", row.names = FALSE)

## Pooled citation data ---------------------------------------------------

write.csv(data_citation, "data/citations.csv", row.names = FALSE)