# Load packages -----------------------------------------------------------

library(httr2)
library(dplyr)
library(purrr)

# Define publication years (back 10 years from previous full year) --------

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
                             "UNSW Sydney"),
             abbrev = c("UOA",
                        "AUT",
                        "UOW",
                        "MU",
                        "VUW",
                        "UC",
                        "LU",
                        "UO",
                        "UM",
                        "ANU",
                        "US",
                        "UQ",
                        "UWA",
                        "UA",
                        "MAU",
                        "UNSWS")
  )

# Retrieve OpenAlex IDs and join to institutions --------------------------

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

# Create function to request & process data -------------------------------

get_pubs <- function(inst) {
  req_inst <-
    request("https://api.openalex.org/") |> 
    req_url_path_append("works") |> 
    req_url_query(filter = paste0("is_paratext:false,is_retracted:false,type:article|book|book-chapter,",
                                  "publication_year:", 
                                  paste0(pub_years, collapse = "|"), 
                                  ",", 
                                  paste0("authorships.institutions.lineage:", inst))) |> 
    req_url_query(select = c("id", "publication_year", "type", "cited_by_count", 
                             "open_access", "primary_topic", "primary_location"), .multi = "comma") |>
    req_url_query(mailto = Sys.getenv("EMAIL_ADDRESS")) |> 
    req_url_query("per-page" = "200") |>
    req_url_query(cursor = "*")
  
  resp_inst <- 
    req_perform_iterative(
      req = req_inst,
      next_req = iterate_with_cursor(
        param_name = "cursor",
        resp_param_value = \(resp) resp_body_json(resp)$meta$next_cursor
      ),
      max_reqs = Inf
    )
  
  data_inst <-
    resp_inst |> 
    map(resp_body_json) |>
    map("results") |>
    list_flatten() |> 
    map(~ tibble(
      id = .x$id,
      institution = inst,
      publication_year = .x$publication_year,
      type = .x$type,
      cited_by_count = .x$cited_by_count,
      is_oa = .x$open_access$is_oa,
      oa_status = .x$open_access$oa_status,
      field = .x$primary_topic$field$display_name,
      journal = .x$primary_location$source$display_name,
      publisher = .x$primary_location$source$host_organization_name
    )) |>
    bind_rows()
}

# Get all pubs for all institutions for all pub years ---------------------

pubs_all <-
  map(universities$id, get_pubs) |> 
  bind_rows()

# Join institution names by matching institution ID -----------------------

data_all <-
  pubs_all |> 
  left_join(universities,
            by = join_by(institution == id)) |>
  rename(item_id = id,
         institution_id = institution,
         institution = display_name) |> 
  relocate(c(institution, abbrev, country_code), .after = institution_id) |> 
  select(-institution_id)

# Summarise data grouped by year, institution, and OA status --------------

data_summary <-
  data_all |>
  group_by(publication_year, institution, oa_status) |>
  tally() |>
  mutate(
    pc = n / sum(n)
  ) |> 
  left_join(universities[2:4], 
            by = join_by(institution == display_name), 
            keep = FALSE) |> 
  relocate(c(abbrev, country_code), .after = institution)

# Export data -------------------------------------------------------------

write.csv(data_all, "data/pubs-all.csv", row.names = FALSE)
saveRDS(data_all, "data/pubs-all.rds")
write.csv(data_summary, "data/pubs-summary.csv", row.names = FALSE)