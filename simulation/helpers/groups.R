mutate_cancer_grade <- function(df0) {
  df1 <- df0 |> 
    mutate(DCIS = case_when(
      dominant_histocode == 38 ~ "High Grade DCIS", 
      dominant_histocode == 39 ~ "Intermediate Grade DCIS",
      dominant_histocode == 40 ~ "Low Grade DCIS",
      dominant_histocode %in% c(38, 39, 40, 35, 41, 42) ~ "Other DCIS/LCIS",
      dominant_histocode %in% c(1:14, 88) ~ "Other",
      (dominant_histocode %in% c(21:31)) & is.na(cancer_grade) ~ "Unknown",
      (dominant_histocode %in% c(21:31)) & !is.na(cancer_grade) ~ "FIX_THIS",
      dominant_histocode %in% c(36, 37) ~ "Other",
      is.na(dominant_histocode) ~ "Other",
      TRUE ~ "ValueError"
    ))
  
  f <- \(x) glue::glue("Grade {x} Invasive")
  ind <- which(df1$DCIS == "FIX_THIS")
  df1$DCIS[ind] <- f(df1$cancer_grade[ind])
  df1
}


mutate_age_group <- function(df0) {
  df0 |>
    mutate(
      age_group = case_when(
        (age < 45) ~ "40-44",
        (age >= 45 & age < 50) ~ "45-49",
        (age >= 75) ~ "75+",
        TRUE ~ as.character(age)
      )
    )
}


mutate_region <- function(df0) {
  df0 |>
    mutate(
      region = countrycode::countrycode(
        country_of_birth, 
        origin = "country.name", 
        destination = "region"
      )
    )
}


mutate_algorithm <- function(df0) {
  df0 |>
    mutate(
      algorithm = case_when(
        (image_manufacturer == "SIEMENS" &
           (grepl(image_manufacturer_algorithm, pattern = "F1") |  
              grepl(image_manufacturer_algorithm, pattern = "Flavor1") |
              grepl(image_manufacturer_algorithm, pattern = "Flavour1"))
        ) ~ "SIEMENS - F1",
        (image_manufacturer == "SIEMENS" &
           grepl(image_manufacturer_algorithm, pattern = "HC_")) ~ "SIEMENS - HC",
        (image_manufacturer == "SIEMENS") ~ "SIEMENS - Other",
        TRUE ~ image_manufacturer
      )
    )
  
}
