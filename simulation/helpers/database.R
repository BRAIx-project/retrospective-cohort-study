database <- function(x) {
  lookup_table <- list(
    "models_ensemble_9" = "/models/outputs",
    "readers_v1.1.1" = "/readers/1.1.1",
    "models_v1.1.2" = "/retro/models/outputs",
    "readers_v1.1.2" = "/readers/1.1.2"
  )
  
  if (missing(x)) {
    return(lookup_table)
  }
  
  lookup_table[[x]]
}

