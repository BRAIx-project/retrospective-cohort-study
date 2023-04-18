box <- function(device, x, y, w, h, text, ...) {
  cx <- x + w / 2
  cy <- y + h / 2
  device$bars(x, y, w, h, ...)  
  device$text(
    text, x = cx, y = cy, 
    attr = list(
      "text-anchor" = "middle",
      "alignment-baseline" = "middle"
    )
  )
}

reader_box <- function(device, x, y, w, h, text, ...) {
  cx <- x + w / 2
  cy <- y + h / 2
  bg <- switch(
    text, 
    "AI reader" = "orange",
    "Reader 3" = "purple",
    "blue"
  )
  if (grepl("AI", text)) bg <- "orange"
  
  device$bars(x, y, w, h, bg = bg, ...)
  device$text(
    text, x = cx, y = cy, 
    attr = list(
      "text-anchor" = "middle",
      "alignment-baseline" = "middle"
    )
  )
}

baseline_flowchart <- function(device, data) {
  device$clear()
  device$par(xlim = c(0, 1), ylim = c(0, 1))
  box_style <- list("stroke-width" = 0.5, "fill-opacity" = 0.15) 
  
  box(device, 0.05, 0.5, 0.15, 0.12, "Mammograms", bg = "grey", style = box_style)
  
  device$lines(x = c(0.2, 0.225, 0.225, 0.25), y = c(0.56, 0.56, 0.71, 0.71))
  device$lines(x = c(0.245, 0.245, 0.25), y = c(0.715, 0.705, 0.71), bg = "black")
  box(device, 0.25, 0.65, 0.15, 0.12, "Reader 1", bg = "blue", style = box_style)
  
  device$lines(x = c(0.2, 0.225, 0.225, 0.25), y = c(0.56, 0.56, 0.41, 0.41))
  device$lines(x = c(0.245, 0.245, 0.25), y = c(0.415, 0.405, 0.41), bg = "black")
  box(device, 0.25, 0.35, 0.15, 0.12, "Reader 2", bg = "blue", style = box_style)
  
  device$lines(x = 0.2 + c(0.2, 0.225, 0.225, 0.25), y = c(0.71, 0.71, 0.56, 0.56))
  device$lines(x = 0.2 + c(0.2, 0.225, 0.225, 0.25), y = c(0.41, 0.41, 0.56, 0.56))
  device$lines(x = 0.45 + c(-0.005, -0.005, 0), 
               y = 0.56 + c(0.005, -0.005, 0), 
               bg = "black")
  box(device, 0.45, 0.5, 0.15, 0.12, "Reader 3", bg = "purple", style = box_style)
  
  # device$lines(x = c(0.6, 0.65), y = c(0.56, 0.56))
  # device$lines(x = 0.65 + c(-0.005, -0.005, 0), 
  # y = 0.56 + c(0.005, -0.005, 0), 
  # bg = "black")
  # box(device, 0.65, 0.5, 0.15, 0.12, "Outcome", bg = "grey", style = box_style)
  
  device$lines(x = 0.6 + c(0, 0.025, 0.025, 0.1), 
               y = c(0.56, 0.56, 0.71, 0.71))
  device$lines(x = c(0.4, 0.7), y = c(0.73, 0.73))
  device$lines(x = 0.7 + c(-0.005, -0.005, 0), 
               y = 0.73 + c(0.005, -0.005, 0), bg = "black")
  device$lines(x = 0.7 + c(-0.005, -0.005, 0), 
               y = 0.71 + c(0.005, -0.005, 0), bg = "black")
  box(device, 0.7, 0.65, 0.15, 0.12, "Recall", bg = "red", style = box_style)
  
  device$lines(x = 0.6 + c(0, 0.025, 0.025, 0.1), 
               y = c(0.56, 0.56, 0.41, 0.41))
  device$lines(x = c(0.4, 0.7), y = c(0.39, 0.39))
  device$lines(x = 0.7 + c(-0.005, -0.005, 0), 
               y = 0.41 + c(0.005, -0.005, 0), bg = "black")
  device$lines(x = 0.7 + c(-0.005, -0.005, 0), 
               y = 0.39 + c(0.005, -0.005, 0), bg = "black")
  box(device, 0.7, 0.35, 0.15, 0.12, "No Recall", bg = "green", style = box_style)
  
  # Count labels
  box(device, 0.21, 0.535, 0.08, 0.05, data$n_preread, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.355, 0.535, 0.08, 0.05, data$n_reader_3, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.44, 0.705, 0.08, 0.05, data$n_reader_1_and_2_recall, bg = "pink", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.44, 0.365, 0.08, 0.05, data$n_reader_1_and_2_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.59, 0.635, 0.08, 0.05, data$n_reader_3_recall, bg = "pink", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.59, 0.435, 0.08, 0.05, data$n_reader_3_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  device$par(xlim = NULL, ylim = NULL)
}

# library(animate)
# device <- animate$new(1024, 768)
# data <- list(
#   n_preread = 477958,
#   n_reader_1_and_2 = 477958,
#   n_reader_1_and_2_no_recall = 436420,
#   n_reader_1_and_2_recall = 4793,
#   n_reader_3 = 36745,
#   n_reader_3_recall = 16550,
#   n_reader_3_no_recall = 20195
# )
# baseline_flowchart(device, data)

arrow <- function(device, x, y, awidth = 0.005, aheight = 0.005) {
  device$lines(x, y)
  device$lines(x = tail(x, 1) + c(-aheight, -aheight, 0),
               y = tail(y, 1) + c(awidth, -awidth, 0),
               bg = "black")
}

band_pass_screening_flowchart <- function(device, data,
                                          custom_labels = c("Reader 1", "Reader 2", "Reader 3")) {
  device$clear()
  device$par(xlim = c(-0.2, 1), ylim = c(0, 1))
  box_style <- list("stroke-width" = 0.5, "fill-opacity" = 0.15) 
  
  box(device, -0.15, 0.5, 0.15, 0.12, "Mammograms", bg = "grey", style = box_style)
  device$lines(x = c(0, 0.05), y = c(0.56, 0.56))
  device$lines(x = 0.05 + c(-0.005, -0.005, 0), 
               y = 0.56 + c(0.005, -0.005, 0), 
               bg = "black")
  
  box(device, 0.05, 0.5, 0.15, 0.12, "", bg = "orange", style = box_style)
  device$text("AI Reader", x = 0.125, y = 0.56, attr = list("text-anchor" = "middle","alignment-baseline" = "middle"))
  
  device$lines(x = c(0.125, 0.125, 0.675, 0.675, 0.7),
               y = c(0.62, 0.79, 0.79, 0.75, 0.75))
  device$lines(x = 0.7 + c(-0.005, -0.005, 0),
               y = 0.75 + c(0.005, -0.005, 0),
               bg = "black")
  
  device$lines(x = c(0.125, 0.125, 0.675, 0.675, 0.7),
               y = c(0.5, 0.33, 0.33, 0.37, 0.37))
  device$lines(x = 0.7 + c(-0.005, -0.005, 0),
               y = 0.37 + c(0.005, -0.005, 0),
               bg = "black")
  
  device$lines(x = c(0.2, 0.225, 0.225, 0.25), y = c(0.56, 0.56, 0.71, 0.71))
  device$lines(x = c(0.245, 0.245, 0.25), y = c(0.715, 0.705, 0.71), bg = "black")
  reader_box(device, 0.25, 0.65, 0.15, 0.12, custom_labels[1], style = box_style)
  
  device$lines(x = c(0.2, 0.225, 0.225, 0.25), y = c(0.56, 0.56, 0.41, 0.41))
  device$lines(x = c(0.245, 0.245, 0.25), y = c(0.415, 0.405, 0.41), bg = "black")
  reader_box(device, 0.25, 0.35, 0.15, 0.12, custom_labels[2], style = box_style)
  
  device$lines(x = 0.2 + c(0.2, 0.225, 0.225, 0.25), y = c(0.71, 0.71, 0.56, 0.56))
  device$lines(x = 0.2 + c(0.2, 0.225, 0.225, 0.25), y = c(0.41, 0.41, 0.56, 0.56))
  device$lines(x = 0.45 + c(-0.005, -0.005, 0), 
               y = 0.56 + c(0.005, -0.005, 0), 
               bg = "black")
  reader_box(device, 0.45, 0.5, 0.15, 0.12, custom_labels[3], style = box_style)
  
  device$lines(x = 0.6 + c(0, 0.025, 0.025, 0.1), 
               y = c(0.56, 0.56, 0.71, 0.71))
  device$lines(x = c(0.4, 0.7), y = c(0.73, 0.73))
  device$lines(x = 0.7 + c(-0.005, -0.005, 0), 
               y = 0.73 + c(0.005, -0.005, 0), bg = "black")
  device$lines(x = 0.7 + c(-0.005, -0.005, 0), 
               y = 0.71 + c(0.005, -0.005, 0), bg = "black")
  box(device, 0.7, 0.65, 0.15, 0.12, "Recall", bg = "red", style = box_style)
  
  device$lines(x = 0.6 + c(0, 0.025, 0.025, 0.1), y = c(0.56, 0.56, 0.41, 0.41))
  device$lines(x = c(0.4, 0.7), y = c(0.39, 0.39))
  device$lines(x = 0.7 + c(-0.005, -0.005, 0), 
               y = 0.41 + c(0.005, -0.005, 0), bg = "black")
  device$lines(x = 0.7 + c(-0.005, -0.005, 0), 
               y = 0.39 + c(0.005, -0.005, 0), bg = "black")
  box(device, 0.7, 0.35, 0.15, 0.12, "No Recall", bg = "green", style = box_style)
  
  # Count labels
  box(device, -0.02, 0.485, 0.08, 0.05, data$n_preread, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.21, 0.535, 0.08, 0.05, data$screening_pass, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.355, 0.535, 0.08, 0.05, data$n_reader_3, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.44, 0.705, 0.08, 0.05, data$n_reader_1_and_2_recall, bg = "pink", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.44, 0.365, 0.08, 0.05, data$n_reader_1_and_2_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.59, 0.635, 0.08, 0.05, data$n_reader_3_recall, bg = "pink", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.59, 0.435, 0.08, 0.05, data$n_reader_3_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.085, 0.42, 0.08, 0.05, data$screening_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.085, 0.65, 0.08, 0.05, data$screening_recall, bg = "pink", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  device$par(xlim = NULL, ylim = NULL)
}


autonomous_flowchart <- function(device, data,
                                 custom_labels = c("Reader 1", "Reader 2", "Reader 3")) {
  device$clear()
  device$par(xlim = c(-0.2, 1), ylim = c(0, 1))
  box_style <- list("stroke-width" = 0.5, "fill-opacity" = 0.15) 
  
  box(device, -0.15, 0.5, 0.15, 0.12, "Mammograms", bg = "grey", style = box_style)
  device$lines(x = c(0, 0.05), y = c(0.56, 0.56))
  device$lines(x = 0.05 + c(-0.005, -0.005, 0), 
               y = 0.56 + c(0.005, -0.005, 0), 
               bg = "black")
  
  box(device, 0.05, 0.5, 0.15, 0.12, "", bg = "orange", style = box_style)
  device$text("AI autonomous", x = 0.125, y = 0.575, attr = list("text-anchor" = "middle","alignment-baseline" = "middle"))
  device$text("screening", x = 0.125, y = 0.545, attr = list("text-anchor" = "middle","alignment-baseline" = "middle"))
  
  device$lines(x = c(0.125, 0.125, 0.675, 0.675, 0.7), 
               y = c(0.5, 0.33, 0.33, 0.37, 0.37))
  device$lines(x = 0.7 + c(-0.005, -0.005, 0),
               y = 0.37 + c(0.005, -0.005, 0),
               bg = "black")
  
  device$lines(x = c(0.2, 0.225, 0.225, 0.25), y = c(0.56, 0.56, 0.71, 0.71))
  device$lines(x = c(0.245, 0.245, 0.25), y = c(0.715, 0.705, 0.71), bg = "black")
  reader_box(device, 0.25, 0.65, 0.15, 0.12, custom_labels[1], style = box_style)
  
  device$lines(x = c(0.2, 0.225, 0.225, 0.25), y = c(0.56, 0.56, 0.41, 0.41))
  device$lines(x = c(0.245, 0.245, 0.25), y = c(0.415, 0.405, 0.41), bg = "black")
  reader_box(device, 0.25, 0.35, 0.15, 0.12, custom_labels[2], style = box_style)
  
  device$lines(x = 0.2 + c(0.2, 0.225, 0.225, 0.25), y = c(0.71, 0.71, 0.56, 0.56))
  device$lines(x = 0.2 + c(0.2, 0.225, 0.225, 0.25), y = c(0.41, 0.41, 0.56, 0.56))
  device$lines(x = 0.45 + c(-0.005, -0.005, 0), 
               y = 0.56 + c(0.005, -0.005, 0), 
               bg = "black")
  reader_box(device, 0.45, 0.5, 0.15, 0.12, custom_labels[3], style = box_style)
  
  # device$lines(x = c(0.6, 0.65), y = c(0.56, 0.56))
  # device$lines(x = 0.65 + c(-0.005, -0.005, 0), 
  #              y = 0.56 + c(0.005, -0.005, 0), 
  #              bg = "black")
  # box(device, 0.65, 0.5, 0.15, 0.12, "Outcome", bg = "grey", style = box_style)
  
  device$lines(x = 0.6 + c(0, 0.025, 0.025, 0.1), 
               y = c(0.56, 0.56, 0.71, 0.71))
  device$lines(x = c(0.4, 0.7), y = c(0.73, 0.73))
  device$lines(x = 0.7 + c(-0.005, -0.005, 0), 
               y = 0.73 + c(0.005, -0.005, 0), bg = "black")
  device$lines(x = 0.7 + c(-0.005, -0.005, 0), 
               y = 0.71 + c(0.005, -0.005, 0), bg = "black")
  box(device, 0.7, 0.65, 0.15, 0.12, "Recall", bg = "red", style = box_style)
  
  device$lines(x = 0.6 + c(0, 0.025, 0.025, 0.1), y = c(0.56, 0.56, 0.41, 0.41))
  device$lines(x = c(0.4, 0.7), y = c(0.39, 0.39))
  device$lines(x = 0.7 + c(-0.005, -0.005, 0), 
               y = 0.41 + c(0.005, -0.005, 0), bg = "black")
  device$lines(x = 0.7 + c(-0.005, -0.005, 0), 
               y = 0.39 + c(0.005, -0.005, 0), bg = "black")
  box(device, 0.7, 0.35, 0.15, 0.12, "No Recall", bg = "green", style = box_style)
  
  # Count labels
  box(device, -0.02, 0.485, 0.08, 0.05, data$n_preread, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.21, 0.535, 0.08, 0.05, data$screening_recall, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.355, 0.535, 0.08, 0.05, data$n_reader_3, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.44, 0.705, 0.08, 0.05, data$n_reader_1_and_2_recall, bg = "pink", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.44, 0.365, 0.08, 0.05, data$n_reader_1_and_2_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.59, 0.635, 0.08, 0.05, data$n_reader_3_recall, bg = "pink", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.59, 0.435, 0.08, 0.05, data$n_reader_3_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.085, 0.435, 0.08, 0.05, data$screening_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  device$par(xlim = NULL, ylim = NULL)
}

# library(animate)
# device <- animate$new(1200, 768)
# data <- list(
#   screening_recall = 20000,
#   screening_no_recall = 2000,
#   n_preread = 477958,
#   n_reader_1_and_2 = 477958,
#   n_reader_1_and_2_no_recall = 436420,
#   n_reader_1_and_2_recall = 4793,
#   n_reader_3 = 36745,
#   n_reader_3_recall = 16550,
#   n_reader_3_no_recall = 20195
# )
# autonomous_flowchart(device, data)


autonomous_practical_flowchart <- function(device, data,
                                           custom_labels = c("Reader 1", "Reader 2", "Reader 3")) {
  device$clear()
  device$par(xlim = c(-0.2, 1.0), ylim = c(0, 1))
  box_style <- list("stroke-width" = 0.5, "fill-opacity" = 0.15) 
  
  box(device, -0.15, 0.5, 0.15, 0.12, "Mammograms", bg = "grey", style = box_style)
  device$lines(x = c(0, 0.05), y = c(0.56, 0.56))
  device$lines(x = 0.05 + c(-0.005, -0.005, 0), 
               y = 0.56 + c(0.005, -0.005, 0), 
               bg = "black")
  
  box(device, 0.05, 0.5, 0.15, 0.12, "", bg = "orange", style = box_style)
  device$text("AI autonomous", x = 0.125, y = 0.575, attr = list("text-anchor" = "middle","alignment-baseline" = "middle"))
  device$text("screening", x = 0.125, y = 0.545, attr = list("text-anchor" = "middle","alignment-baseline" = "middle"))
  
  box(device, 0.55, 0.25, 0.15, 0.12, "Reader 4", bg = "purple", style = box_style)
  device$lines(x = c(0.125, 0.125, 0.55), 
               y = c(0.5, 0.31, 0.31))
  device$lines(x = 0.55 + c(-0.005, -0.005, 0),
               y = 0.31 + c(0.005, -0.005, 0),
               bg = "black")
  device$lines(x = c(0.7, 0.825, 0.825, 0.85),
               y = c(0.33, 0.33, 0.37, 0.37))
  device$lines(x = 0.85 + c(-0.005, -0.005, 0),
               y = 0.37 + c(0.005, -0.005, 0),
               bg = "black")
  device$lines(x = c(0.7, 1.03, 1.03, 1),
               y = c(0.29, 0.29, 0.7125, 0.7125))
  device$lines(x = 1.01 + c(-0.005, -0.005, -0.01),
               y = 0.7125 + c(0.005, -0.005, 0),
               bg = "black")
  
  device$lines(x = c(0.2, 0.225, 0.225, 0.25), y = c(0.56, 0.56, 0.71, 0.71))
  device$lines(x = c(0.245, 0.245, 0.25), y = c(0.715, 0.705, 0.71), bg = "black")
  reader_box(device, 0.25, 0.65, 0.15, 0.12, custom_labels[1], style = box_style)
  
  device$lines(x = c(0.2, 0.225, 0.225, 0.25), y = c(0.56, 0.56, 0.41, 0.41))
  device$lines(x = c(0.245, 0.245, 0.25), y = c(0.415, 0.405, 0.41), bg = "black")
  reader_box(device, 0.25, 0.35, 0.15, 0.12, custom_labels[2], style = box_style)
  
  device$lines(x = 0.2 + c(0.2, 0.225, 0.225, 0.35), 
               y = c(0.71, 0.71, 0.56, 0.56))
  device$lines(x = 0.2 + c(0.2, 0.225, 0.225, 0.35), 
               y = c(0.41, 0.41, 0.56, 0.56))
  device$lines(x = 0.55 + c(-0.005, -0.005, 0), 
               y = 0.56 + c(0.005, -0.005, 0), 
               bg = "black")
  reader_box(device, 0.55, 0.5, 0.15, 0.12, custom_labels[3], style = box_style)
  
  # device$lines(x = c(0.6, 0.65), y = c(0.56, 0.56))
  # device$lines(x = 0.65 + c(-0.005, -0.005, 0), 
  #              y = 0.56 + c(0.005, -0.005, 0), 
  #              bg = "black")
  # box(device, 0.65, 0.5, 0.15, 0.12, "Outcome", bg = "grey", style = box_style)
  
  device$lines(x = 0.6 + c(0.1, 0.225, 0.225, 0.25), 
               y = c(0.56, 0.56, 0.71, 0.71))
  device$lines(x = c(0.4, 0.85), y = c(0.73, 0.73))
  device$lines(x = 0.85 + c(-0.005, -0.005, 0), 
               y = 0.73 + c(0.005, -0.005, 0), bg = "black")
  device$lines(x = 0.85 + c(-0.005, -0.005, 0), 
               y = 0.71 + c(0.005, -0.005, 0), bg = "black")
  box(device, 0.85, 0.65, 0.15, 0.12, "Recall", bg = "red", style = box_style)
  
  device$lines(x = 0.6 + c(0.1, 0.225, 0.225, 0.25), 
               y = c(0.56, 0.56, 0.41, 0.41))
  device$lines(x = c(0.4, 0.85), y = c(0.39, 0.39))
  device$lines(x = 0.85 + c(-0.005, -0.005, 0), 
               y = 0.41 + c(0.005, -0.005, 0), bg = "black")
  device$lines(x = 0.85 + c(-0.005, -0.005, 0), 
               y = 0.39 + c(0.005, -0.005, 0), bg = "black")
  box(device, 0.85, 0.35, 0.15, 0.12, "No Recall", bg = "green", style = box_style)
  
  # Count labels
  box(device, -0.02, 0.485, 0.08, 0.05, data$n_preread, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.21, 0.535, 0.08, 0.05, data$screening_recall, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  
  box(device, 0.355, 0.535, 0.08, 0.05, data$n_reader_3, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.44, 0.705, 0.08, 0.05, data$n_reader_1_and_2_recall, bg = "pink", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.44, 0.365, 0.08, 0.05, data$n_reader_1_and_2_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.76, 0.635, 0.08, 0.05, data$n_reader_3_recall, bg = "pink", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.76, 0.435, 0.08, 0.05, data$n_reader_3_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  
  box(device, 0.085, 0.435, 0.08, 0.05, data$screening_no_recall, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.72, 0.305, 0.08, 0.05, data$review_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.85, 0.265, 0.08, 0.05, data$review_recall, bg = "pink", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  device$par(xlim = NULL, ylim = NULL)
}

# library(animate)
# device <- animate$new(1200, 768)
# data <- list(
#   screening_no_recall = 2000,
#   screening_recall = 20000,
#   review_recall = 1000,
#   review_no_recall = 1000,
#   n_preread = 477958,
#   n_reader_1_and_2 = 477958,
#   n_reader_1_and_2_no_recall = 436420,
#   n_reader_1_and_2_recall = 4793,
#   n_reader_3 = 36745,
#   n_reader_3_recall = 16550,
#   n_reader_3_no_recall = 20195
# )
# autonomous_practical_flowchart(device, data)


independent_flowchart <- function(device, data, 
                                  custom_labels = c("Reader 1", "AI Reader", "Reader 3")) {
  device$clear()
  device$par(xlim = c(0, 1), ylim = c(0, 1))
  box_style <- list("stroke-width" = 0.5, "fill-opacity" = 0.15) 
  
  box(device, 0.05, 0.5, 0.15, 0.12, "Mammograms", bg = "grey", style = box_style)
  
  device$lines(x = c(0.2, 0.225, 0.225, 0.25), y = c(0.56, 0.56, 0.71, 0.71))
  device$lines(x = c(0.245, 0.245, 0.25), y = c(0.715, 0.705, 0.71), bg = "black")
  reader_box(device, 0.25, 0.65, 0.15, 0.12, custom_labels[1], style = box_style)
  
  device$lines(x = c(0.2, 0.225, 0.225, 0.25), y = c(0.56, 0.56, 0.41, 0.41))
  device$lines(x = c(0.245, 0.245, 0.25), y = c(0.415, 0.405, 0.41), bg = "black")
  reader_box(device, 0.25, 0.35, 0.15, 0.12, custom_labels[2], style = box_style)
  
  device$lines(x = 0.2 + c(0.2, 0.225, 0.225, 0.25), y = c(0.71, 0.71, 0.56, 0.56))
  device$lines(x = 0.2 + c(0.2, 0.225, 0.225, 0.25), y = c(0.41, 0.41, 0.56, 0.56))
  device$lines(x = 0.45 + c(-0.005, -0.005, 0), 
               y = 0.56 + c(0.005, -0.005, 0), 
               bg = "black")
  reader_box(device, 0.45, 0.5, 0.15, 0.12, custom_labels[3], style = box_style)
  
  # device$lines(x = c(0.6, 0.65), y = c(0.56, 0.56))
  # device$lines(x = 0.65 + c(-0.005, -0.005, 0), 
  #              y = 0.56 + c(0.005, -0.005, 0), 
  #              bg = "black")
  # box(device, 0.65, 0.5, 0.15, 0.12, "Outcome", bg = "grey", style = box_style)
  
  device$lines(x = 0.4 + c(0.2, 0.225, 0.225, 0.3), y = c(0.56, 0.56, 0.71, 0.71))
  device$lines(x = c(0.4, 0.7), y = c(0.73, 0.73))
  device$lines(x = 0.7 + c(-0.005, -0.005, 0), 
               y = 0.73 + c(0.005, -0.005, 0), bg = "black")
  device$lines(x = 0.7 + c(-0.005, -0.005, 0), 
               y = 0.71 + c(0.005, -0.005, 0), bg = "black")
  box(device, 0.7, 0.65, 0.15, 0.12, "Recall", bg = "red", style = box_style)
  
  device$lines(x = 0.4 + c(0.2, 0.225, 0.225, 0.3), y = c(0.56, 0.56, 0.41, 0.41))
  device$lines(x = c(0.4, 0.7), y = c(0.39, 0.39))
  device$lines(x = 0.7 + c(-0.005, -0.005, 0), 
               y = 0.41 + c(0.005, -0.005, 0), bg = "black")
  device$lines(x = 0.7 + c(-0.005, -0.005, 0), 
               y = 0.39 + c(0.005, -0.005, 0), bg = "black")
  box(device, 0.7, 0.35, 0.15, 0.12, "No Recall", bg = "green", style = box_style)
  
  # Count labels
  box(device, 0.21, 0.535, 0.08, 0.05, data$n_preread, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.355, 0.535, 0.08, 0.05, data$n_reader_3, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.44, 0.705, 0.08, 0.05, data$n_reader_1_and_2_recall, bg = "pink", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.44, 0.365, 0.08, 0.05, data$n_reader_1_and_2_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.59, 0.635, 0.08, 0.05, data$n_reader_3_recall, bg = "pink", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.59, 0.435, 0.08, 0.05, data$n_reader_3_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  device$par(xlim = NULL, ylim = NULL)
}

# library(animate)
# device <- animate$new(1024, 768)
# data <- list(
#   n_preread = 477958,
#   n_reader_1_and_2 = 477958,
#   n_reader_1_and_2_no_recall = 436420,
#   n_reader_1_and_2_recall = 4793,
#   n_reader_3 = 36745,
#   n_reader_3_recall = 16550,
#   n_reader_3_no_recall = 20195
# )
# independent_flowchart(device, data)


final_pass_flowchart <- function(device, data,
                                 custom_labels = c("Reader 1", "Reader 2", "Reader 3")) {
  device$clear()
  device$par(xlim = c(0, 1.25), ylim = c(0, 1))
  box_style <- list("stroke-width" = 0.5, "fill-opacity" = 0.15) 
  
  box(device, 0.05, 0.5, 0.15, 0.12, "Mammograms", bg = "grey", style = box_style)
  
  device$lines(x = c(0.2, 0.225, 0.225, 0.25), y = c(0.56, 0.56, 0.71, 0.71))
  device$lines(x = c(0.245, 0.245, 0.25), y = c(0.715, 0.705, 0.71), bg = "black")
  reader_box(device, 0.25, 0.65, 0.15, 0.12, custom_labels[1], style = box_style)
  
  device$lines(x = c(0.2, 0.225, 0.225, 0.25), y = c(0.56, 0.56, 0.41, 0.41))
  device$lines(x = c(0.245, 0.245, 0.25), y = c(0.415, 0.405, 0.41), bg = "black")
  reader_box(device, 0.25, 0.35, 0.15, 0.12, custom_labels[2], style = box_style)
  
  device$lines(x = 0.2 + c(0.2, 0.225, 0.225, 0.25), y = c(0.71, 0.71, 0.56, 0.56))
  device$lines(x = 0.2 + c(0.2, 0.225, 0.225, 0.25), y = c(0.41, 0.41, 0.56, 0.56))
  device$lines(x = 0.45 + c(-0.005, -0.005, 0), 
               y = 0.56 + c(0.005, -0.005, 0), 
               bg = "black")
  reader_box(device, 0.45, 0.5, 0.15, 0.12, custom_labels[3], style = box_style)
  
  # device$lines(x = c(0.6, 0.65), y = c(0.56, 0.56))
  # device$lines(x = 0.65 + c(-0.005, -0.005, 0), 
  #              y = 0.56 + c(0.005, -0.005, 0), 
  #              bg = "black")
  # box(device, 0.65, 0.5, 0.15, 0.12, "Outcome", bg = "grey", style = box_style)
  
  device$lines(x = 0.4 + c(0.2, 0.225, 0.225, 0.3), y = c(0.56, 0.56, 0.71, 0.71))
  device$lines(x = c(0.4, 0.7), y = c(0.73, 0.73))
  device$lines(x = 0.7 + c(-0.005, -0.005, 0), 
               y = 0.73 + c(0.005, -0.005, 0), bg = "black")
  device$lines(x = 0.7 + c(-0.005, -0.005, 0), 
               y = 0.71 + c(0.005, -0.005, 0), bg = "black")
  box(device, 0.7, 0.65, 0.15, 0.12, "Recall", bg = "red", style = box_style)
  
  device$lines(x = 0.4 + c(0.2, 0.225, 0.225, 0.3), y = c(0.56, 0.56, 0.41, 0.41))
  device$lines(x = c(0.4, 0.7), y = c(0.39, 0.39))
  device$lines(x = 0.7 + c(-0.005, -0.005, 0), 
               y = 0.41 + c(0.005, -0.005, 0), bg = "black")
  device$lines(x = 0.7 + c(-0.005, -0.005, 0), 
               y = 0.39 + c(0.005, -0.005, 0), bg = "black")
  box(device, 0.7, 0.35, 0.15, 0.12, "", bg = "green", style = box_style)
  device$text("No Recall", x = 0.775, y = 0.425, attr = list("text-anchor" = "middle","alignment-baseline" = "middle"))
  device$text("(Prelim)", x = 0.775, y = 0.395, attr = list("text-anchor" = "middle","alignment-baseline" = "middle"))
  
  
  # Final pass
  box(device, 0.88, 0.35, 0.15, 0.12, "AI final pass", bg = "orange", style = box_style)
  
  device$lines(x = c(0.85, 0.88), y = c(0.41, 0.41))
  device$lines(x = 0.88 + c(-0.005, -0.005, 0), 
               y = 0.41 + c(0.005, -0.005, 0), bg = "black")
  
  device$lines(x = 0.95 + c(0, 0, -0.1), y = c(0.47, 0.71, 0.71))
  device$lines(x = 0.86 + c(-0.005, -0.005, -0.01), 
               y = 0.71 + c(0.005, -0.005, 0), bg = "black")
  
  box(device, 1.06, 0.35, 0.15, 0.12, "No Recall", bg = "green", style = box_style)
  device$lines(x = c(1.03, 1.06), y = c(0.41, 0.41))
  device$lines(x = 1.06 + c(-0.005, -0.005, 0), 
               y = 0.41 + c(0.005, -0.005, 0), bg = "black")
  
  # Count labels
  box(device, 0.21, 0.535, 0.08, 0.05, data$n_preread, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.355, 0.535, 0.08, 0.05, data$n_reader_3, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.44, 0.705, 0.08, 0.05, data$n_reader_1_and_2_recall, bg = "pink", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.44, 0.365, 0.08, 0.05, data$n_reader_1_and_2_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.59, 0.635, 0.08, 0.05, data$n_reader_3_recall, bg = "pink", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.59, 0.435, 0.08, 0.05, data$n_reader_3_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  
  box(device, 0.825, 0.32, 0.08, 0.05, data$n_reader_1_and_2_no_recall + data$n_reader_3_no_recall, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.91, 0.5, 0.08, 0.05, data$final_pass_recall, bg = "pink", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 1.005, 0.32, 0.08, 0.05, data$final_pass_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  device$par(xlim = NULL, ylim = NULL)
}

# library(animate)
# device <- animate$new(1300, 768)
# data <- list(
#   n_preread = 477958,
#   n_reader_1_and_2 = 477958,
#   n_reader_1_and_2_no_recall = 436420,
#   n_reader_1_and_2_recall = 4793,
#   n_reader_3 = 36745,
#   n_reader_3_recall = 16550,
#   n_reader_3_no_recall = 20195,
#   final_pass_recall = 1000,
#   final_pass_no_recall = 1000
# )
# final_pass_flowchart(device, data)


final_pass_2_flowchart <- function(device, data,
                                   custom_labels = c("Reader 1", "Reader 2", "Reader 3")) {
  device$clear()
  device$par(xlim = c(0, 1.25), ylim = c(0, 1))
  box_style <- list("stroke-width" = 0.5, "fill-opacity" = 0.15) 
  
  box(device, 0.05, 0.5, 0.15, 0.12, "Mammograms", bg = "grey", style = box_style)
  
  device$lines(x = c(0.2, 0.225, 0.225, 0.25), y = c(0.56, 0.56, 0.71, 0.71))
  device$lines(x = c(0.245, 0.245, 0.25), y = c(0.715, 0.705, 0.71), bg = "black")
  reader_box(device, 0.25, 0.65, 0.15, 0.12, custom_labels[1], style = box_style)
  
  device$lines(x = c(0.2, 0.225, 0.225, 0.25), y = c(0.56, 0.56, 0.41, 0.41))
  device$lines(x = c(0.245, 0.245, 0.25), y = c(0.415, 0.405, 0.41), bg = "black")
  reader_box(device, 0.25, 0.35, 0.15, 0.12, custom_labels[2], style = box_style)
  
  device$lines(x = 0.2 + c(0.2, 0.225, 0.225, 0.25), y = c(0.71, 0.71, 0.56, 0.56))
  device$lines(x = 0.2 + c(0.2, 0.225, 0.225, 0.25), y = c(0.41, 0.41, 0.56, 0.56))
  device$lines(x = 0.45 + c(-0.005, -0.005, 0), 
               y = 0.56 + c(0.005, -0.005, 0), 
               bg = "black")
  reader_box(device, 0.45, 0.5, 0.15, 0.12, custom_labels[3], style = box_style)
  
  device$lines(x = 0.4 + c(0.2, 0.225, 0.225, 0.295), 
               y = c(0.56, 0.56, 0.71, 0.71))
  device$lines(x = c(0.4, 0.695), y = c(0.73, 0.73))
  device$lines(x = 0.695 + c(-0.005, -0.005, 0), 
               y = 0.73 + c(0.005, -0.005, 0), bg = "black")
  device$lines(x = 0.695 + c(-0.005, -0.005, 0), 
               y = 0.71 + c(0.005, -0.005, 0), bg = "black")
  box(device, 0.695, 0.65, 0.15, 0.12, "", bg = "red", style = box_style)
  device$text("Recall", x = 0.77, y = 0.725, attr = list("text-anchor" = "middle","alignment-baseline" = "middle"))
  device$text("(Prelim)", x = 0.77, y = 0.695, attr = list("text-anchor" = "middle","alignment-baseline" = "middle"))
  
  device$lines(x = 0.4 + c(0.2, 0.225, 0.225, 0.295), 
               y = c(0.56, 0.56, 0.41, 0.41))
  device$lines(x = c(0.4, 0.695), y = c(0.39, 0.39))
  device$lines(x = 0.695 + c(-0.005, -0.005, 0), 
               y = 0.41 + c(0.005, -0.005, 0), bg = "black")
  device$lines(x = 0.695 + c(-0.005, -0.005, 0), 
               y = 0.39 + c(0.005, -0.005, 0), bg = "black")
  box(device, 0.695, 0.35, 0.15, 0.12, "", bg = "green", style = box_style)
  device$text("No Recall", x = 0.77, y = 0.425, attr = list("text-anchor" = "middle","alignment-baseline" = "middle"))
  device$text("(Prelim)", x = 0.77, y = 0.395, attr = list("text-anchor" = "middle","alignment-baseline" = "middle"))
  
  # Final pass - No Recall
  box(device, 0.89, 0.5, 0.15, 0.12, "AI final pass", bg = "orange", style = box_style)
  
  device$lines(x = 0.845 + c(0, 0.02, 0.02, 0.04), 
               y = c(0.41, 0.41, 0.56, 0.56))
  device$lines(x = 0.845 + c(0, 0.02, 0.02, 0.04), 
               y = c(0.71, 0.71, 0.56, 0.56))
  device$lines(x = 0.88 + c(0.005, 0.005, 0.01), 
               y = 0.56 + c(0.005, -0.005, 0), bg = "black")
  
  box(device, 1.08, 0.35, 0.15, 0.12, "No Recall", bg = "green", style = box_style)
  device$lines(x = c(1.04, 1.06, 1.06, 1.08), 
               y = c(0.56, 0.56, 0.41, 0.41))
  device$lines(x = 1.08 + c(-0.005, -0.005, 0), 
               y = 0.41 + c(0.005, -0.005, 0), bg = "black")
  
  # Final pass - Recall
  box(device, 1.08, 0.65, 0.15, 0.12, "Recall", bg = "red", style = box_style)
  device$lines(x = c(1.04, 1.06, 1.06, 1.08), 
               y = c(0.56, 0.56, 0.71, 0.71))
  device$lines(x = 1.08 + c(-0.005, -0.005, 0), 
               y = 0.71 + c(0.005, -0.005, 0), bg = "black")
  
  # Count labels
  box(device, 0.21, 0.535, 0.08, 0.05, data$n_preread, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.355, 0.535, 0.08, 0.05, data$n_reader_3, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.44, 0.705, 0.08, 0.05, data$n_reader_1_and_2_recall, bg = "pink", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.44, 0.365, 0.08, 0.05, data$n_reader_1_and_2_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.59, 0.635, 0.08, 0.05, data$n_reader_3_recall, bg = "pink", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.59, 0.435, 0.08, 0.05, data$n_reader_3_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  
  box(device, 0.855, 0.635, 0.08, 0.05, 
      data$n_reader_1_and_2_recall + data$n_reader_3_recall, 
      bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.855, 0.435, 0.08, 0.05, 
      data$n_reader_1_and_2_no_recall + data$n_reader_3_no_recall, 
      bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.99, 0.635, 0.08, 0.05, data$final_pass_recall, bg = "pink", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.99, 0.435, 0.08, 0.05, data$final_pass_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  device$par(xlim = NULL, ylim = NULL)
}

# library(animate)
# device <- animate$new(1300, 768)
# data <- list(
#   n_preread = 477958,
#   n_reader_1_and_2 = 477958,
#   n_reader_1_and_2_no_recall = 436420,
#   n_reader_1_and_2_recall = 4793,
#   n_reader_3 = 36745,
#   n_reader_3_recall = 16550,
#   n_reader_3_no_recall = 20195,
#   final_pass_recall = 1000,
#   final_pass_no_recall = 1000
# )
# final_pass_2_flowchart(device, data)


#-------------------------------------------------------------------------------
autonomous_final_pass_flowchart <- function(device, data,
                                            custom_labels = c("Reader 1", "Reader 2", "Reader 3")) {
  device$clear()
  device$par(xlim = c(-0.2, 1.25), ylim = c(0, 1))
  box_style <- list("stroke-width" = 0.5, "fill-opacity" = 0.15) 
  
  box(device, -0.15, 0.5, 0.15, 0.12, "Mammograms", bg = "grey", style = box_style)
  device$lines(x = c(0, 0.05), y = c(0.56, 0.56))
  device$lines(x = 0.05 + c(-0.005, -0.005, 0), 
               y = 0.56 + c(0.005, -0.005, 0), 
               bg = "black")
  
  box(device, 0.05, 0.5, 0.15, 0.12, "", bg = "orange", style = box_style)
  device$text("AI autonomous", x = 0.125, y = 0.575, attr = list("text-anchor" = "middle","alignment-baseline" = "middle"))
  device$text("screening", x = 0.125, y = 0.545, attr = list("text-anchor" = "middle","alignment-baseline" = "middle"))
  
  device$lines(x = c(0.125, 0.125, 0.625, 0.625, 0.65), 
               y = c(0.5, 0.33, 0.33, 0.37, 0.37))
  device$lines(x = 0.65 + c(-0.005, -0.005, 0),
               y = 0.37 + c(0.005, -0.005, 0),
               bg = "black")
  
  device$lines(x = c(0.2, 0.225, 0.225, 0.25), y = c(0.56, 0.56, 0.71, 0.71))
  device$lines(x = c(0.245, 0.245, 0.25), y = c(0.715, 0.705, 0.71), bg = "black")
  reader_box(device, 0.25, 0.65, 0.15, 0.12, custom_labels[1], style = box_style)
  
  device$lines(x = c(0.2, 0.225, 0.225, 0.25), y = c(0.56, 0.56, 0.41, 0.41))
  device$lines(x = c(0.245, 0.245, 0.25), y = c(0.415, 0.405, 0.41), bg = "black")
  reader_box(device, 0.25, 0.35, 0.15, 0.12, custom_labels[2], style = box_style)
  
  device$lines(x = 0.2 + c(0.2, 0.225, 0.225, 0.25), y = c(0.71, 0.71, 0.56, 0.56))
  device$lines(x = 0.2 + c(0.2, 0.225, 0.225, 0.25), y = c(0.41, 0.41, 0.56, 0.56))
  device$lines(x = 0.45 + c(-0.005, -0.005, 0), 
               y = 0.56 + c(0.005, -0.005, 0), 
               bg = "black")
  reader_box(device, 0.45, 0.5, 0.15, 0.12, custom_labels[3], style = box_style)
  
  # device$lines(x = c(0.6, 0.65), y = c(0.56, 0.56))
  # device$lines(x = 0.65 + c(-0.005, -0.005, 0), 
  #              y = 0.56 + c(0.005, -0.005, 0), 
  #              bg = "black")
  # box(device, 0.65, 0.5, 0.15, 0.12, "Outcome", bg = "grey", style = box_style)
  
  device$lines(x = 0.4 + c(0.2, 0.225, 0.225, 0.25), y = c(0.56, 0.56, 0.71, 0.71))
  device$lines(x = c(0.4, 0.65), y = c(0.73, 0.73))
  device$lines(x = 0.65 + c(-0.005, -0.005, 0), 
               y = 0.73 + c(0.005, -0.005, 0), bg = "black")
  device$lines(x = 0.65 + c(-0.005, -0.005, 0), 
               y = 0.71 + c(0.005, -0.005, 0), bg = "black")
  box(device, 0.65, 0.65, 0.15, 0.12, "Recall", bg = "red", style = box_style)
  
  device$lines(x = 0.4 + c(0.2, 0.225, 0.225, 0.25), y = c(0.56, 0.56, 0.41, 0.41))
  device$lines(x = c(0.4, 0.65), y = c(0.39, 0.39))
  device$lines(x = 0.65 + c(-0.005, -0.005, 0), 
               y = 0.41 + c(0.005, -0.005, 0), bg = "black")
  device$lines(x = 0.65 + c(-0.005, -0.005, 0), 
               y = 0.39 + c(0.005, -0.005, 0), bg = "black")
  
  box(device, 0.65, 0.35, 0.15, 0.12, "", bg = "green", style = box_style)
  device$text("No Recall", x = 0.725, y = 0.425, attr = list("text-anchor" = "middle","alignment-baseline" = "middle"))
  device$text("(Prelim)", x = 0.725, y = 0.395, attr = list("text-anchor" = "middle","alignment-baseline" = "middle"))
  
  # Final pass
  box(device, 0.83, 0.35, 0.15, 0.12, "AI final pass", bg = "orange", style = box_style)
  
  device$lines(x = c(0.8, 0.83), y = c(0.41, 0.41))
  device$lines(x = 0.83 + c(-0.005, -0.005, 0), 
               y = 0.41 + c(0.005, -0.005, 0), bg = "black")
  
  device$lines(x = 0.9 + c(0, 0, -0.1), y = c(0.47, 0.71, 0.71))
  device$lines(x = 0.81 + c(-0.005, -0.005, -0.01), 
               y = 0.71 + c(0.005, -0.005, 0), bg = "black")
  
  box(device, 1.01, 0.35, 0.15, 0.12, "No Recall", bg = "green", style = box_style)
  device$lines(x = c(0.98, 1.01), y = c(0.41, 0.41))
  device$lines(x = 1.01 + c(-0.005, -0.005, 0), 
               y = 0.41 + c(0.005, -0.005, 0), bg = "black")
  
  # Count labels
  box(device, -0.02, 0.485, 0.08, 0.05, data$n_preread, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.21, 0.535, 0.08, 0.05, data$screening_recall, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  
  box(device, 0.355, 0.535, 0.08, 0.05, data$n_reader_3, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.44, 0.705, 0.08, 0.05, data$n_reader_1_and_2_recall, bg = "pink", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.44, 0.365, 0.08, 0.05, data$n_reader_1_and_2_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.56, 0.635, 0.08, 0.05, data$n_reader_3_recall, bg = "pink", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.56, 0.435, 0.08, 0.05, data$n_reader_3_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.085, 0.435, 0.08, 0.05, data$screening_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  
  box(device, 0.775, 0.32, 0.08, 0.05, data$n_reader_1_and_2_no_recall + data$n_reader_3_no_recall + data$screening_no_recall, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.86, 0.5, 0.08, 0.05, data$final_pass_recall, bg = "pink", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.955, 0.32, 0.08, 0.05, data$final_pass_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  device$par(xlim = NULL, ylim = NULL)
}

# library(animate)
# device <- animate$new(1300, 768)
# data <- list(
#   screening_no_recall = 1000,
#   screening_recall = 2000,
#   n_preread = 477958,
#   n_reader_1_and_2 = 477958,
#   n_reader_1_and_2_no_recall = 436420,
#   n_reader_1_and_2_recall = 4793,
#   n_reader_3 = 36745,
#   n_reader_3_recall = 16550,
#   n_reader_3_no_recall = 20195,
#   final_pass_recall = 1000,
#   final_pass_no_recall = 1000
# )
# autonomous_final_pass_flowchart(device, data)


autonomous_practical_final_pass_flowchart <- function(device, data,
                                                      custom_labels = c("Reader 1", "Reader 2", "Reader 3")) {
  device$clear()
  device$par(xlim = c(-0.15, 1.25), ylim = c(0, 1))
  box_style <- list("stroke-width" = 0.5, "fill-opacity" = 0.15) 
  
  box(device, -0.15, 0.5, 0.15, 0.12, "Mammograms", bg = "grey", style = box_style)
  device$lines(x = c(0, 0.05), y = c(0.56, 0.56))
  device$lines(x = 0.05 + c(-0.005, -0.005, 0), 
               y = 0.56 + c(0.005, -0.005, 0), 
               bg = "black")
  
  box(device, 0.05, 0.5, 0.15, 0.12, "", bg = "orange", style = box_style)
  device$text("AI autonomous", x = 0.125, y = 0.575, attr = list("text-anchor" = "middle","alignment-baseline" = "middle"))
  device$text("screening", x = 0.125, y = 0.545, attr = list("text-anchor" = "middle","alignment-baseline" = "middle"))
  
  # Autnomous practical
  box(device, 0.55, 0.25, 0.15, 0.12, "Reader 4", bg = "purple", style = box_style)
  # in-1
  device$lines(x = c(0.125, 0.125, 0.55), 
               y = c(0.5, 0.31, 0.31))
  device$lines(x = 0.55 + c(-0.005, -0.005, 0),
               y = 0.31 + c(0.005, -0.005, 0),
               bg = "black")
  # out-1: No recall
  device$lines(x = c(0.7, 0.825, 0.825, 0.85),
               y = c(0.33, 0.33, 0.37, 0.37))
  device$lines(x = 0.85 + c(-0.005, -0.005, 0),
               y = 0.37 + c(0.005, -0.005, 0),
               bg = "black")
  # out-2: Recall
  device$lines(x = c(0.7, 1.38, 1.38, 1),
               y = c(0.29, 0.29, 0.73, 0.73))
  device$lines(x = 1.01 + c(-0.005, -0.005, -0.01),
               y = 0.73 + c(0.005, -0.005, 0),
               bg = "black")
  
  device$lines(x = c(0.2, 0.225, 0.225, 0.25), y = c(0.56, 0.56, 0.71, 0.71))
  device$lines(x = c(0.245, 0.245, 0.25), y = c(0.715, 0.705, 0.71), bg = "black")
  reader_box(device, 0.25, 0.65, 0.15, 0.12, custom_labels[1], style = box_style)
  
  device$lines(x = c(0.2, 0.225, 0.225, 0.25), y = c(0.56, 0.56, 0.41, 0.41))
  device$lines(x = c(0.245, 0.245, 0.25), y = c(0.415, 0.405, 0.41), bg = "black")
  reader_box(device, 0.25, 0.35, 0.15, 0.12, custom_labels[2], style = box_style)
  
  device$lines(x = 0.2 + c(0.2, 0.225, 0.225, 0.35), 
               y = c(0.71, 0.71, 0.56, 0.56))
  device$lines(x = 0.2 + c(0.2, 0.225, 0.225, 0.35), 
               y = c(0.41, 0.41, 0.56, 0.56))
  device$lines(x = 0.55 + c(-0.005, -0.005, 0), 
               y = 0.56 + c(0.005, -0.005, 0), 
               bg = "black")
  reader_box(device, 0.55, 0.5, 0.15, 0.12, custom_labels[3], style = box_style)
  
  # device$lines(x = c(0.6, 0.65), y = c(0.56, 0.56))
  # device$lines(x = 0.65 + c(-0.005, -0.005, 0), 
  #              y = 0.56 + c(0.005, -0.005, 0), 
  #              bg = "black")
  # box(device, 0.65, 0.5, 0.15, 0.12, "Outcome", bg = "grey", style = box_style)
  
  device$lines(x = 0.6 + c(0.1, 0.225, 0.225, 0.25), 
               y = c(0.56, 0.56, 0.71, 0.71))
  device$lines(x = c(0.4, 0.85), y = c(0.73, 0.73))
  device$lines(x = 0.85 + c(-0.005, -0.005, 0), 
               y = 0.73 + c(0.005, -0.005, 0), bg = "black")
  device$lines(x = 0.85 + c(-0.005, -0.005, 0), 
               y = 0.71 + c(0.005, -0.005, 0), bg = "black")
  box(device, 0.85, 0.65, 0.15, 0.12, "Recall", bg = "red", style = box_style)
  
  device$lines(x = 0.6 + c(0.1, 0.225, 0.225, 0.25), 
               y = c(0.56, 0.56, 0.41, 0.41))
  device$lines(x = c(0.4, 0.85), y = c(0.39, 0.39))
  device$lines(x = 0.85 + c(-0.005, -0.005, 0), 
               y = 0.41 + c(0.005, -0.005, 0), bg = "black")
  device$lines(x = 0.85 + c(-0.005, -0.005, 0), 
               y = 0.39 + c(0.005, -0.005, 0), bg = "black")
  box(device, 0.85, 0.35, 0.15, 0.12, "", bg = "green", style = box_style)
  device$text("No Recall", x = 0.925, y = 0.425, attr = list("text-anchor" = "middle","alignment-baseline" = "middle"))
  device$text("(Prelim)", x = 0.925, y = 0.395, attr = list("text-anchor" = "middle","alignment-baseline" = "middle"))
  
  # Final pass
  box(device, 1.03, 0.35, 0.15, 0.12, "AI final pass", bg = "orange", style = box_style)
  
  device$lines(x = c(1, 1.03), y = c(0.41, 0.41))
  device$lines(x = 1.03 + c(-0.005, -0.005, 0), 
               y = 0.41 + c(0.005, -0.005, 0), bg = "black")
  
  device$lines(x = 1.1 + c(0, 0, -0.1), y = c(0.47, 0.71, 0.71))
  device$lines(x = 1.01 + c(-0.005, -0.005, -0.01), 
               y = 0.71 + c(0.005, -0.005, 0), bg = "black")
  
  box(device, 1.21, 0.35, 0.15, 0.12, "No Recall", bg = "green", style = box_style)
  device$lines(x = c(1.18, 1.21), y = c(0.41, 0.41))
  device$lines(x = 1.21 + c(-0.005, -0.005, 0), 
               y = 0.41 + c(0.005, -0.005, 0), bg = "black")
  
  # Count labels
  box(device, 0.21, 0.535, 0.08, 0.05, data$n_preread, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.355, 0.535, 0.08, 0.05, data$n_reader_3, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.44, 0.705, 0.08, 0.05, data$n_reader_1_and_2_recall, bg = "pink", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.44, 0.365, 0.08, 0.05, data$n_reader_1_and_2_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.76, 0.635, 0.08, 0.05, data$n_reader_3_recall, bg = "pink", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.76, 0.435, 0.08, 0.05, data$n_reader_3_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  
  box(device, 0.085, 0.435, 0.08, 0.05, data$screening_no_recall, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.72, 0.305, 0.08, 0.05, data$review_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 0.85, 0.265, 0.08, 0.05, data$review_recall, bg = "pink", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  
  box(device, 0.975, 0.32, 0.08, 0.05, data$n_reader_1_and_2_no_recall + data$n_reader_3_no_recall, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 1.06, 0.5, 0.08, 0.05, data$final_pass_recall, bg = "pink", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  box(device, 1.155, 0.32, 0.08, 0.05, data$final_pass_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  
  device$par(xlim = NULL, ylim = NULL)
}


# library(animate)
# device <- animate$new(1300, 768)
# data <- list(
#   screening_no_recall = 1000,
#   review_recall = 1000,
#   review_no_recall = 1000,
#   n_preread = 477958,
#   n_reader_1_and_2 = 477958,
#   n_reader_1_and_2_no_recall = 436420,
#   n_reader_1_and_2_recall = 4793,
#   n_reader_3 = 36745,
#   n_reader_3_recall = 16550,
#   n_reader_3_no_recall = 20195,
#   final_pass_recall = 1000,
#   final_pass_no_recall = 1000
# )
# autonomous_practical_final_pass_flowchart(device, data)


autonomous_single_final_pass_flowchart <- function(device, data, show_counts = TRUE) {
  device$clear()
  device$par(xlim = c(-0.2, 1.25), ylim = c(0, 1))
  box_style <- list("stroke-width" = 0.5, "fill-opacity" = 0.15) 
  
  box(device, 0.05, 0.5, 0.15, 0.12, "Mammograms", bg = "grey", style = box_style)
  device$lines(x = c(0.2, 0.25), y = c(0.56, 0.56))
  device$lines(x = 0.25 + c(-0.005, -0.005, 0), 
               y = 0.56 + c(0.005, -0.005, 0), 
               bg = "black")
  
  box(device, 0.25, 0.5, 0.15, 0.12, "", bg = "orange", style = box_style)
  device$text("AI autonomous", x = 0.325, y = 0.575, attr = list("text-anchor" = "middle","alignment-baseline" = "middle"))
  device$text("screening", x = 0.325, y = 0.545, attr = list("text-anchor" = "middle","alignment-baseline" = "middle"))
  
  device$lines(x = c(0.325, 0.325, 0.65), 
               y = c(0.5, 0.39, 0.39))
  
  device$lines(x = 0.2 + c(0.2, 0.25), y = c(0.56, 0.56))
  device$lines(x = 0.45 + c(-0.005, -0.005, 0), 
               y = 0.56 + c(0.005, -0.005, 0), 
               bg = "black")
  reader_box(device, 0.45, 0.5, 0.15, 0.12, "AI reader", style = box_style)
  
  device$lines(x = 0.4 + c(0.2, 0.225, 0.225, 0.25), y = c(0.56, 0.56, 0.71, 0.71))
  device$lines(x = 0.65 + c(-0.005, -0.005, 0), 
               y = 0.71 + c(0.005, -0.005, 0), bg = "black")
  box(device, 0.65, 0.65, 0.15, 0.12, "Recall", bg = "red", style = box_style)
  
  device$lines(x = 0.4 + c(0.2, 0.225, 0.225, 0.25), y = c(0.56, 0.56, 0.41, 0.41))
  # device$lines(x = c(0.4, 0.65), y = c(0.39, 0.39))
  device$lines(x = 0.65 + c(-0.005, -0.005, 0),
               y = 0.41 + c(0.005, -0.005, 0), bg = "black")
  device$lines(x = 0.65 + c(-0.005, -0.005, 0), 
               y = 0.39 + c(0.005, -0.005, 0), bg = "black")
  
  box(device, 0.65, 0.35, 0.15, 0.12, "", bg = "green", style = box_style)
  device$text("No Recall", x = 0.725, y = 0.425, attr = list("text-anchor" = "middle","alignment-baseline" = "middle"))
  device$text("(Prelim)", x = 0.725, y = 0.395, attr = list("text-anchor" = "middle","alignment-baseline" = "middle"))
  
  # Final pass
  box(device, 0.83, 0.35, 0.15, 0.12, "AI final pass", bg = "orange", style = box_style)
  
  device$lines(x = c(0.8, 0.83), y = c(0.41, 0.41))
  device$lines(x = 0.83 + c(-0.005, -0.005, 0), 
               y = 0.41 + c(0.005, -0.005, 0), bg = "black")
  
  device$lines(x = 0.9 + c(0, 0, -0.1), y = c(0.47, 0.71, 0.71))
  device$lines(x = 0.81 + c(-0.005, -0.005, -0.01), 
               y = 0.71 + c(0.005, -0.005, 0), bg = "black")
  
  box(device, 1.01, 0.35, 0.15, 0.12, "No Recall", bg = "green", style = box_style)
  device$lines(x = c(0.98, 1.01), y = c(0.41, 0.41))
  device$lines(x = 1.01 + c(-0.005, -0.005, 0), 
               y = 0.41 + c(0.005, -0.005, 0), bg = "black")
  
  # Count labels
  if (show_counts) {
    box(device, 0.2-0.02, 0.485, 0.08, 0.05, data$n_preread, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
    box(device, 0.385, 0.485, 0.08, 0.05, data$screening_recall, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
    
    box(device, 0.56, 0.635, 0.08, 0.05, data$n_reader_recall, bg = "pink", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
    box(device, 0.56, 0.435, 0.08, 0.05, data$n_reader_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
    box(device, 0.285, 0.435, 0.08, 0.05, data$screening_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
    
    box(device, 0.775, 0.32, 0.08, 0.05, data$n_reader_no_recall + data$screening_no_recall, bg = "white", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
    box(device, 0.86, 0.5, 0.08, 0.05, data$final_pass_recall, bg = "pink", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
    box(device, 0.955, 0.32, 0.08, 0.05, data$final_pass_no_recall, bg = "lightgreen", style = list("stroke-width" = 0.5, 'fill-opacity' = 1))
  }
  device$par(xlim = NULL, ylim = NULL)
}

# library(animate)
# device <- animate$new(1300, 768)
# data <- list(
#   screening_no_recall = 1000,
#   screening_recall = 2000,
#   n_preread = 477958,
#   n_reader = 36745,
#   n_reader_recall = 16550,
#   n_reader_no_recall = 20195,
#   final_pass_recall = 1000,
#   final_pass_no_recall = 1000
# )
# autonomous_single_final_pass_flowchart(device, data)


# Testing ======================================================================
if (FALSE) {
  library(animate)
  device <- animate$new(1300, 768)
  data <- list(
    screening_recall = 1000,
    screening_no_recall = 100000,
    review_recall = 1000,
    review_no_recall = 1000,
    n_preread = 477958,
    n_reader_1_and_2 = 477958,
    n_reader_1_and_2_no_recall = 436420,
    n_reader_1_and_2_recall = 4793,
    n_reader_3 = 36745,
    n_reader_3_recall = 16550,
    n_reader_3_no_recall = 20195,
    final_pass_recall = 1000,
    final_pass_no_recall = 1000
  )
  human_readers <- c("Reader 1", "Reader 2", "Reader 3")
  AI_two <- c("Reader 1", "AI reader", "Reader 3")
  AI_three <- c("Reader 1", "Reader 2", "AI reader")
  
  baseline_flowchart(device, data)
  
  independent_flowchart(device, data, AI_two)
  independent_flowchart(device, data, AI_three)
  
  final_pass_flowchart(device, data)
  final_pass_flowchart(device, data, AI_two)
  final_pass_flowchart(device, data, AI_three)
  
  final_pass_2_flowchart(device, data)
  final_pass_2_flowchart(device, data, AI_two)
  final_pass_2_flowchart(device, data, AI_three)
  
  autonomous_flowchart(device, data, human_readers)
  autonomous_flowchart(device, data, AI_two)
  autonomous_flowchart(device, data, AI_three)
  
  autonomous_practical_flowchart(device, data, human_readers)
  autonomous_practical_flowchart(device, data, AI_two)
  autonomous_practical_flowchart(device, data, AI_three)
  
  final_pass_flowchart(device, data, human_readers)
  final_pass_flowchart(device, data, AI_two)
  final_pass_flowchart(device, data, AI_three)
  
  autonomous_final_pass_flowchart(device, data, human_readers)
  autonomous_final_pass_flowchart(device, data, AI_two)
  autonomous_final_pass_flowchart(device, data, AI_three)
  
  autonomous_practical_final_pass_flowchart(device, data, human_readers)
  autonomous_practical_final_pass_flowchart(device, data, AI_two)
  autonomous_practical_final_pass_flowchart(device, data, AI_three)
}
