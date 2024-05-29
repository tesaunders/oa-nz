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

# Assemble queries

req_nz <- paste0("https://api.openalex.org/works?filter=",
              parameters,
              ",",
              "institutions.ror:",
              paste0(inst_nz$ror, collapse = "|"),
              ",",
              "publication_year:",
              pub_years$years,
              group,
              "&mailto=tom.saunders@auckland.ac.nz")

req_au <- paste0("https://api.openalex.org/works?filter=",
                 parameters,
                 ",",
                 "institutions.ror:",
                 paste0(inst_au$ror, collapse = "|"),
                 ",",
                 "publication_year:",
                 pub_years$years,
                 group,
                 "&mailto=tom.saunders@auckland.ac.nz")

# Loop through queries

raw_nz <- NULL

for (i in 1:length(req_nz)) {
  raw_nz[[i]] <- (read_json(req_nz[[i]], simplifyVector = TRUE))
}

raw_au <- NULL

for (i in 1:length(req_au)) {
  raw_au[[i]] <- (read_json(req_au[[i]], simplifyVector = TRUE))
}

# Flatten raw JSON response

flat_nz <- enframe(unlist(raw_nz))
flat_nz$value <- as.numeric(flat_nz$value)

flat_au <- enframe(unlist(raw_au))
flat_au$value <- as.numeric(flat_au$value)

# Filter to relevant rows

flat_clean_nz <- flat_nz |> 
  filter(str_detect(flat_nz$name, "group_by.count")) |> 
  mutate(
    year = rep(c(2010:prev_year), each = 5),
    oa_type = rep(c("closed", "gold", "hybrid", "green", "bronze"), times = (prev_year-2010+1)),
  ) |> 
  group_by(year) |> 
  mutate(
    pc = (value / sum(value) * 100),
  ) |> 
  select(year, oa_type, value, pc)

flat_clean_au <- flat_au |> 
  filter(str_detect(flat_au$name, "group_by.count")) |> 
  mutate(
    year = rep(c(2010:prev_year), each = 5),
    oa_type = rep(c("closed", "gold", "hybrid", "green", "bronze"), times = (prev_year-2010+1)),
  ) |> 
  group_by(year) |> 
  mutate(
    pc = (value / sum(value) * 100),
  ) |> 
  select(year, oa_type, value, pc)

# Export data to .csv with latest year appended

write.csv(flat_clean_nz, paste0("data/overall_nz.csv"), row.names = FALSE)
write.csv(flat_clean_au, paste0("data/overall_au.csv"), row.names = FALSE)