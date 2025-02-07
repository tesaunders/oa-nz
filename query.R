library(jsonlite)
library(purrr)
library(tibble)
library(dplyr)
library(stringr)

# Define most current full year and publication years of interest

prev_year <- as.numeric(format(Sys.Date(), "%Y"))-1
pub_years <- data.frame("years" = as.factor(2010:prev_year))

# Define institutions and rors

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
                                   "https://ror.org/03r8z3t63")
                           )

# Define query parameters

parameters <- paste(c(
  "is_paratext:false",
  "type:article|book|book-chapter",
  "is_retracted:false"),
  collapse = ",")

group <- "&group_by=oa_status"

mail <- "&mailto=tom.saunders@auckland.ac.nz"

# Get list of requests from all institution x year combinations

ror_year <- do.call(paste0, (expand.grid(paste0(institutions$ror, ",", "publication_year:"), pub_years$years)))

req <- paste0("https://api.openalex.org/works?filter=institutions.ror:",
                 ror_year, 
                 ",", 
                 parameters,
                 group,
                 mail)

# Retrieve data from OpenAlex

raw_response <- req |> 
  map(fromJSON)

# Wrangle into data frame

flat <- enframe(unlist(raw_response))
flat$value <- as.numeric(flat$value)

# Filter to relevant rows

flat_clean <- flat |> 
  filter(str_detect(flat$name, "group_by.count")) |> 
  mutate(
    institution = rep(institutions$institution, 
                      each = 6, times = nrow(pub_years)),
    year = rep(c(2010:prev_year), each = length(institutions$institution)*6),
    oa_type = rep(c("closed", "gold", "hybrid", "green", "bronze", "diamond"), 
                  times = length(2010:prev_year)*length(institutions$institution)),
    country = case_when(str_detect(institution, pattern = "Auc|Wai|Mas|Well|Can|Lin|Ota") ~ "nz",
                        .default = "au"),
  ) |> 
  group_by(institution, year) |> 
  mutate(
    pc = (value / sum(value) * 100),
  ) |> 
  select(year, institution, country, oa_type, value, pc)

# Combine and export data

write.csv(flat_clean, "data/oa-data.csv",
          row.names = FALSE)
