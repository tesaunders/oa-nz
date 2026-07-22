library(dplyr)

pubs <- readRDS(file = "data_raw/university_publications.rds")

pubs <- pubs |> 
  mutate(
    country = case_when(
      str_detect(institution_name, "Auc|Wai|Mas|Vic|Can|Ota|Lin|Tec") ~ "nz",
      TRUE ~ "au"
    ),
    institution_name = case_when(
      str_detect(institution_name, "^University of Sydney$") ~ "The University of Sydney",
      str_detect(institution_name, "^University of Western Australia$") ~ "The University of Western Australia",
      str_detect(institution_name, "^University of Melbourne$") ~ "The University of Melbourne",
      str_detect(institution_name, "^University of Queensland$") ~ "The University of Queensland",
      str_detect(institution_name, "^University of Adelaide$|^The University of Adelaide$") ~ "Adelaide University",
      .default = institution_name
    )
  )

# Export cleaned data

if (!dir.exists("data")) dir.create("data", recursive = TRUE)

write.csv(pubs, "data/all-data.csv", row.names = FALSE)

# Summarise open access status by institution and publication year

pubs |> 
  count(institution_name, publication_year, oa_status, name = "count") |> 
  group_by(institution_name, publication_year) |> 
  mutate(percent = round(count / sum(count), 4)) |> 
  write.csv("data/oa-institution-year.csv", row.names = FALSE)

# Summarise open access rate by field

pubs |> 
  distinct(id, .keep_all = TRUE) |>
  group_by(country, field) |> 
  summarise(
    percent_open = sum(oa_status != "closed") / n()
  ) |> 
  arrange(desc(percent_open)) |> 
  write.csv("data/oa-field.csv", row.names = FALSE)

# Summarise citations by open access status

pubs |> 
  distinct(id, .keep_all = TRUE) |>
  group_by(country, publication_year, oa_status) |> 
  summarise(
    works = n(),
    n_citations = sum(cited_by_count),
    max_citations = max(cited_by_count),
    min_citations = min(cited_by_count),
    med_citations = median(cited_by_count),
    avg_citations = mean(cited_by_count)
  ) |> 
  write.csv("data/citations-oa-year.csv", row.names = FALSE)

# Summarise citations by open or closed

pubs |> 
  distinct(id, .keep_all = TRUE) |> 
  mutate(open = oa_status != "closed") |>
  summarise(
    med_citations = median(cited_by_count, na.rm = TRUE),
    .by = c(country, publication_year, open)
  ) |> 
  write.csv("data/citations-open-year.csv", row.names = FALSE)