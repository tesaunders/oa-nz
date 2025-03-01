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

# Build API request (using email address stored as environment variable) --

id_year <- do.call(paste0, (expand.grid(paste0(universities$id, ",", "publication_year:"), pub_years)))
  
req_pubs <-
  id_year |> 
  map(\(id_year_x) request("https://api.openalex.org/") |>
        req_url_path_append("works") |>
        req_url_query(filter = paste0("is_paratext:false,is_retracted:false,type:article|book|book-chapter,authorships.institutions.lineage:", id_year_x)) |>
        req_url_query(group_by = "open_access.oa_status") |>
        req_url_query(mailto = Sys.getenv("EMAIL_ADDRESS")))

# Make requests sequentially and process the json into a dataframe --------

resp_pubs <-
  req_pubs |>
  req_perform_sequential() |> 
  resps_successes() |>
  resps_data(\(resp) resp_body_json(resp)) 

req_data <-
  resp_pubs |>
  keep(names(resp_pubs) == "group_by") |> 
  bind_rows()

# Add relevant labels and calculate percentages ---------------------------

pub_data <- 
  req_data |> 
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

# Export data as csv ------------------------------------------------------

write.csv(pub_data, "data/publications.csv",
          row.names = FALSE)