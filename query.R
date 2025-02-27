library(httr2)
library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# Define time periods

prev_year <- as.numeric(format(Sys.Date(), "%Y"))-1
pub_years <- 2015:prev_year

# Define institutions

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

# Retrieve OpenAlex IDs and append to universities

openalex_ids <-
  request("https://api.openalex.org/") |> 
  req_url_path_append("institutions") |> 
  req_url_query(filter = "country_code:nz|au") |>
  req_url_query(search = c(paste0('"', paste0(universities$institution), collapse = " OR ",'"'))) |> 
  req_url_query(select = c("id", "display_name", "country_code"), .multi = "comma") |> 
  req_perform() |>
  resp_body_json()

universities <-
  openalex_ids$results |> 
  map(as.data.frame) |> 
  bind_rows() |> 
  inner_join(universities, by = join_by(display_name == institution))

# Retrieve publication data

id_year <- do.call(paste0, (expand.grid(paste0(universities$id, ",", "publication_year:"), pub_years)))
  
queries <-
  id_year |> 
  map(\(id_year_x) request("https://api.openalex.org/") |>
        req_url_path_append("works") |>
        req_url_query(filter = paste0("authorships.institutions.lineage:", id_year_x)) |>
        req_url_query(group_by = "open_access.oa_status") |>
        req_url_query(mailto = "tom.saunders@auckland.ac.nz"))

responses <-
  queries |>
  req_perform_sequential() |> 
  resps_successes() |>
  resps_data(\(resp) resp_body_json(resp)) 

write_json(responses, "data/raw_response.json")

raw_data <-
  responses |>
  keep(names(responses) == "group_by") |> 
  bind_rows()

# Add relevant labels and calculate percentages

oa_data <- raw_data |> 
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

# Export data

write.csv(oa_data, "data/oa-data.csv",
          row.names = FALSE)