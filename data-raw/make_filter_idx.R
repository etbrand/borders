# Create lookups to remove trees and filter by color

pkgload::load_all()

flower_str <- paste(readLines("data-raw/flower_str.txt"), collapse = "")

is_tree <- function(plant_type) {
  if (is.null(plant_type)) {
    return(FALSE)
  }
  grepl("tree", plant_type, ignore.case = TRUE)
}

like_flower <- function(name) {
  if (is.null(name)) {
    return(FALSE)
  }
  grepl(flower_str, name, ignore.case = TRUE)
}

has_flowers <- function(details_record) {
  any(
    like_flower(details_record$type),
    like_flower(details_record$common_name),
    like_flower(details_record$scientific_name)
  )
}

simplify_color <- function(color) {
  switch(color,
         "blu" = "blue",
         "bronz" = "yellow",
         "brown" = "brown",
         "cream" = "white",
         "green" = "green",
         "lavender" = "purple",
         "orang" = "orange",
         "pink" = "pink",
         "plum" = "purple",
         "purpl" = "purple",
         "red" = "red",
         "salmon" = "pink",
         "whit" = "white",
         "yellow" = "yellow")
}

append_color <- function(color_idx, id, color_record) {

  dominant_color <- get_flower_color(color_record, FALSE)

  if (is.null(dominant_color)) {
    message(paste0("Color not mapped for entry '", color_record, "'"))
    return(color_idx)
  } else {
    cat(blue(
      paste0("Color '", color_record, "' mapped to ", dominant_color, "\n")
    ))
  }

  if (dominant_color == "burgundy") {
    color_idx$purple <- c(color_idx$purple, id)
    color_idx$red <- c(color_idx$red, id)
    return(color_idx)
  }

  if (dominant_color == "violet") {
    color_idx$purple <- c(color_idx$purple, id)
    color_idx$blue <- c(color_idx$blue, id)
    return(color_idx)
  }

  if (dominant_color == "chartreuse") {
    color_idx$green <- c(color_idx$green, id)
    color_idx$yellow <- c(color_idx$yellow, id)
    return(color_idx)
  }

  if (dominant_color %in% c("coral", "peach")) {
    color_idx$pink <- c(color_idx$pink, id)
    color_idx$orange <- c(color_idx$orange, id)
    return(color_idx)
  }

  if (dominant_color == "rose") {
    color_idx$pink <- c(color_idx$pink, id)
    color_idx$red <- c(color_idx$red, id)
    return(color_idx)
  }

  if (dominant_color %in% c("fuchsia", "magenta")) {
    color_idx$pink <- c(color_idx$pink, id)
    color_idx$purple <- c(color_idx$purple, id)
    return(color_idx)
  }
  simple_color <- simplify_color(dominant_color)
  color_idx[[simple_color]] <- c(color_idx[[simple_color]], id)
  color_idx
}

ids <- 1:10000
tree_ids <- NULL
color_idx <- NULL

for (id in ids) {
  resp <- req_details(id)
  if (resp$status == 200) {
    details_record <- resp_to_list(resp)
    if (is_tree(details_record$type)) {
      tree_ids <- c(tree_ids, id)
    }
    if (has_flowers(details_record)) {
      color_idx <- append_color(color_idx, id, details_record$flower_color)
    } else {
      color_idx$no_color <- c(color_idx$no_color, id)
    }
  }
  Sys.sleep(1)
}

usethis::use_data(color_idx, tree_ids, overwrite = TRUE)
