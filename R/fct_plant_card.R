# Plant types are messy; this cleans them up
simplify_plant_type <- function(plant_type) {

  if (!is.character(plant_type)) {
    return("Default")
  }

  type_like <- function(pattern) grepl(pattern, plant_type, ignore.case = TRUE)

  if (type_like("flower|violet|begonia|aster|iris|veronica|verbena")) {
    return("Flower")
  }
  if (type_like("orchid")) {
    return("Orchid")
  }
  if (type_like("waterlily")) {
    return("Waterlily")
  }
  if (type_like("fruit")) {
    return("Fruit")
  }
  if (type_like("shrub")) {
    return("Shrub")
  }
  if (type_like("tree|evergreen")) {
    return("Tree")
  }
  if (type_like("conifer")) {
    return("Conifer")
  }
  if (type_like("palm")) {
    return("Palm")
  }
  if (type_like("grass|rush|sedge|reed|graminoid")) {
    return("Grass")
  }
  if (type_like("carnivorous")) {
    return("Carnivorous")
  }
  if (type_like("vine|climb|creep")) {
    return("Vine")
  }
  if (type_like("weed|invasive")) {
    return("Weed")
  }
  if (type_like("produce|vegetable")) {
    return("Produce")
  }
  if (type_like("bulb")) {
    return("Bulb")
  }
  if (type_like("herb")) {
    return("Herb")
  }
  if (type_like("cactus")) {
    return("Cactus")
  }
  if (type_like("grain")) {
    return("Grain")
  }
  if(type_like("fern")) {
    return("Fern")
  }

  message(paste0("No icon for plant type '", plant_type, "'"))

  plant_type
}

plant_card_header <- function(plant_type, common_name) {

  header_icon <- switch(
    simplify_plant_type(plant_type),
    "Bulb" = "noun-onion-4623459",
    "Cactus" = "noun-cactus-2590999",
    "Carnivorous" = "noun-venus-flytrap-27589",
    "Conifer" = "noun-pine-5973913",
    "Fern" = "noun-fern-4273283",
    "Flower" = "noun-flower-7088847",
    "Fruit" = "noun-fruit-6461708",
    "Grain" = "noun-wheat-124955",
    "Grass" = "noun-grass-6990187",
    "Herb" = "noun-herb-6363034",
    "Orchid" = "noun-orchid-7084860",
    "Palm" = "noun-palm-5949874",
    "Produce" = "noun-vegetable-7110902",
    "Shrub" = "noun-shrub-4416049",
    "Tree" = "noun-tree-7108880",
    "Vine" = "noun-vine-2247901",
    "Waterlily" = "noun-water-lily-6701192",
    "noun-plant-5018755"
  )

  bslib::card_header(
    class = "plant-card-header",
    tags$span(
      tags$img(src = glue::glue("www/noun_icons/{header_icon}.svg"),
               height = 20, width = 20),
      stringr::str_to_title(common_name)
    )
  )

}
