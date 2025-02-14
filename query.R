library(jsonlite)
library(purrr)
library(tibble)
library(dplyr)
library(stringr)
library(tidyr)

# Define most current full year and publication years of interest

prev_year <- as.numeric(format(Sys.Date(), "%Y"))-1
pub_years <- data.frame("years" = as.factor(2010:prev_year))

# Define institutions and rors

universities <- data.frame(institution = c("University of Auckland",
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

# Retrieve institutional ROR IDs from OpenAlex by searching for each institution

ror_query <- paste0("https://api.openalex.org/institutions?search=",
                    '"',
                    paste0(universities$institution, collapse = '" OR "'),
                    '"',
                    "&filter=country_code:nz|au",
                    "&select=display_name,ror,country_code")

ror_query <- str_replace_all(ror_query, " ", "%20")

ror_query_resp <- fromJSON(ror_query)

universities <- left_join(universities,
                          unnest(ror_query_resp[["results"]]),
                          by = join_by(institution == display_name))

# Retrieve OpenAlex IDs from OpenAlex by matching ROR IDs

id_query <- paste0("https://api.openalex.org/institutions?filter=ror:", 
                     paste0(universities$ror, collapse = "|"),
                     "&select=display_name,id")

id_query_resp <- fromJSON(id_query)

universities <- left_join(universities,
                          unnest(id_query_resp[["results"]]),
                          by = join_by(institution == display_name))

# Prepare main query

parameters <- paste(c(
  "is_paratext:false",
  "type:article|book|book-chapter",
  "is_retracted:false"),
  collapse = ",")

group <- "&group_by=oa_status"

mail <- "&mailto=tom.saunders@auckland.ac.nz"

# Get list of requests from all institution x year combinations

id_year <- do.call(paste0, (expand.grid(paste0(universities$id, ",", "publication_year:"), pub_years$years)))

req <- paste0("https://api.openalex.org/works?filter=institutions.id:",
                 id_year, 
                 ",", 
                 parameters,
                 group,
                 mail)

# Retrieve data from OpenAlex

raw_response <- req |> 
  map(fromJSON)

write_json(raw_response, "data/raw_response.json")

raw_data <- raw_response |> 
  map(pluck, "group_by") |> 
  bind_rows()

# Add relevant labels

oa_data <- raw_data |> 
  mutate(
    institution = rep(universities$institution, 
                      each = 6, times = nrow(pub_years)),
    year = rep(c(2010:prev_year), each = length(universities$institution)*6),
    country = case_when(str_detect(institution, pattern = "Auc|Wai|Mas|Well|Can|Lin|Ota") ~ "nz",
                        .default = "au"),
  ) |> 
  group_by(institution, year) |> 
  mutate(
    pc = (count / sum(count) * 100),
  ) |> 
  select(year, institution, country, key, count, pc)

# Combine and export data

write.csv(oa_data, "data/oa-data.csv",
          row.names = FALSE)
