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
                                            "AgResearch",
                                            "ESR",
                                            "GNS",
                                            "Landcare Research",
                                            "NIWA",
                                            "Plant & Food Research",
                                            "Scion",
                                            "Te Pūkenga",
                                            "Cawthron Institute",
                                            "Malaghan Institute"),
                           ror = c("https://ror.org/03b94tp07",
                                   "https://ror.org/01zvqw119",
                                   "https://ror.org/013fsnh78",
                                   "https://ror.org/052czxv31",
                                   "https://ror.org/0040r6f76",
                                   "https://ror.org/03y7q9t39",
                                   "https://ror.org/04ps1r162",
                                   "https://ror.org/01jmxt844",
                                   "https://ror.org/0124gwh94",
                                   "https://ror.org/0405trq15",
                                   "https://ror.org/03vaqfv64",
                                   "https://ror.org/02p9cyn66",
                                   "https://ror.org/04hxcaz34",
                                   "https://ror.org/02bchch95",
                                   "https://ror.org/048r72142",
                                   "https://ror.org/00tsqex91",
                                   "https://ror.org/03sffqe64",
                                   "https://ror.org/02487ts63")
                          )

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
                 paste0(institutions$ror, collapse = "|"),
                 ",",
                 "publication_year:",
                 pub_years$years,
                 group)

# institutions.country_code:"NZ" does not work

# Loop through URLs to make requests

raw_nz <- NULL

for (i in 1:length(req_nz)) {
  raw_nz[[i]] <- (read_json(req_nz[[i]], simplifyVector = TRUE))
}

# Flatten raw JSON

oa_nz <- enframe(unlist(raw_nz))
oa_nz$value <- as.numeric(oa_nz$value)

# Filter to relevant rows

oa_nz <- oa_nz |> 
  filter(str_detect(oa_nz$name, "group_by.count")) |> 
  mutate(
    year = rep(c(2010:prev_year), each = 5),
    oa_type = rep(c("closed", "gold", "hybrid", "green", "bronze"), times = 14),
  ) |> 
  group_by(year) |> 
  mutate(
    pc = (value / sum(value) * 100),
  ) |> 
  select(year, oa_type, value, pc)

# Export data to .csv with latest year appended

write.csv(oa_nz, paste0("data/oa_nz_", prev_year, ".csv"), row.names = FALSE)
