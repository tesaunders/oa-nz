library(jsonlite)
library(tidyverse)

# Get most current full year

prev_year <- as.numeric(format(Sys.Date(), "%Y"))-1
pub_years <- data.frame("years" = as.factor(2010:prev_year))

# Get institutions and rors

institutions <- data.frame(institution  = c("University of Auckland",
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
                                            "UNSW Sydney"),
                           ror = c("https://ror.org/03b94tp07",
                                   "https://ror.org/01zvqw119",
                                   "https://ror.org/013fsnh78",
                                   "https://ror.org/052czxv31",
                                   "https://ror.org/0040r6f76",
                                   "https://ror.org/03y7q9t39",
                                   "https://ror.org/04ps1r162",
                                   "https://ror.org/01jmxt844",
                                   "https://ror.org/01ej9dk98",
                                   "https://ror.org/019wvm592",
                                   "https://ror.org/0384j8v12",
                                   "https://ror.org/00rqy9422",
                                   "https://ror.org/047272k79",
                                   "https://ror.org/00892tw58",
                                   "https://ror.org/02bfwt286",
                                   "https://ror.org/03r8z3t63"),
                           country = c("nz",
                                       "nz",
                                       "nz",
                                       "nz",
                                       "nz",
                                       "nz",
                                       "nz",
                                       "nz",
                                       "au",
                                       "au",
                                       "au",
                                       "au",
                                       "au",
                                       "au",
                                       "au",
                                       "au")
                           )

inst_nz <- institutions |> 
  filter(country == "nz")

inst_au <- institutions |> 
  filter(country == "au")

# Define query parameters

parameters <- paste(c(
  "is_paratext:false",
  "type:article|book|book-chapter",
  "is_retracted:false"), 
  collapse = ",")

group <- "&group_by=oa_status"

# Get list of requests from all institution x year combinations

ror_year_nz <- do.call(paste0, (expand.grid(paste0(inst_nz$ror, ",", "publication_year:"), pub_years$years)))

req_nz <- paste0("https://api.openalex.org/works?filter=institutions.ror:",
                 ror_year_nz, 
                 ",", 
                 parameters, 
                 group,
                 "&mailto=tom.saunders@auckland.ac.nz")

ror_year_au <- do.call(paste0, (expand.grid(paste0(inst_au$ror, ",", "publication_year:"), pub_years$years)))

req_au <- paste0("https://api.openalex.org/works?filter=institutions.ror:",
                 ror_year_au, 
                 ",", 
                 parameters, 
                 group,
                 "&mailto=tom.saunders@auckland.ac.nz")

# Loop through queries and save raw JSON output

raw_response_nz <- NULL

for (i in 1:length(req_nz)) {
  raw_response_nz[[i]] <- (read_json(req_nz[[i]], simplifyVector = TRUE))
}

exportJSON_nz <- toJSON(raw_response_nz)
write(exportJSON_nz, "data/raw_response_institution_nz.json")

raw_response_au <- NULL

for (i in 1:length(req_au)) {
  raw_response_au[[i]] <- (read_json(req_au[[i]], simplifyVector = TRUE))
}

exportJSON_au <- toJSON(raw_response_au)
write(exportJSON_au, "data/raw_response_institution_au.json")

# Flatten raw JSON response

flat_nz <- enframe(unlist(raw_response_nz))
flat_nz$value <- as.numeric(flat_nz$value)

flat_au <- enframe(unlist(raw_response_au))
flat_au$value <- as.numeric(flat_au$value)

# Filter to relevant rows

flat_clean_nz <- flat_nz |> 
  filter(str_detect(flat_nz$name, "group_by.count")) |> 
  mutate(
    institution = rep(inst_nz$institution, 
                      each = 5, times = 14),
    year = rep(c(2010:prev_year), each = length(inst_nz$institution)*5),
    oa_type = rep(c("closed", "gold", "hybrid", "green", "bronze"), 
                  times = length(2010:prev_year)*length(inst_nz$institution)),
  ) |> 
  group_by(institution, year) |> 
  mutate(
    pc = (value / sum(value) * 100),
  ) |> 
  select(year, institution, oa_type, value, pc)

flat_clean_au <- flat_au |> 
  filter(str_detect(flat_au$name, "group_by.count")) |> 
  mutate(
    institution = rep(inst_au$institution, 
                      each = 5, times = 14),
    year = rep(c(2010:prev_year), each = length(inst_au$institution)*5),
    oa_type = rep(c("closed", "gold", "hybrid", "green", "bronze"), 
                  times = length(2010:prev_year)*length(inst_au$institution)),
  ) |> 
  group_by(institution, year) |> 
  mutate(
    pc = (value / sum(value) * 100),
  ) |> 
  select(year, institution, oa_type, value, pc)

# Export data to .csv with latest year appended

write.csv(flat_clean_nz, paste0("data/institutions_nz.csv"), row.names = FALSE)
write.csv(flat_clean_au, paste0("data/institutions_au.csv"), row.names = FALSE)
