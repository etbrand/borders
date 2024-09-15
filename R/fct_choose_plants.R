insert_plant_cards <- function(ns, plants, your_border) {
  plant_cards <- plants |>
    purrr::imap(~ plant_card_UI(ns(.y), .x, .y %in% names(your_border())))
  shiny::insertUI(
    paste0("#", ns("show_more_div")),
    where = "beforeBegin",
    ui = tagList(!!!unname(plant_cards))
  )
}

make_filters <- function(id, ns) {
  tags$div(
    class = "mb-0",
    shinyWidgets::pickerInput(
      inputId = ns(id),
      label = tags$span(
        paste0(stringr::str_to_sentence(gsub("_", " ", id)), ":")
      ),
      choices = filter_selects[[id]],
      multiple = TRUE,
      options = if (id == "flower_color") list(
        `actions-box` = TRUE,
        `select-all-text` = "Select all",
        `deselect-all-text` = "Deselect all"
      )
    )
  )
}

#TODO see if can be used for border
make_plant_stub <- function(id, details_record, your_border, card_ids,
                            search_ids, ns = NULL) {

  in_border <- id %in% names(your_border())

  if (is.null(ns)) {
    update_btn_id <- paste0(id, "-border_update")
  } else {
    update_btn_id <- ns(paste0(id, "-search_update"))
  }

  shinyjs::onclick(
    id = update_btn_id,
    expr = update_border(
      id = id,
      your_border = your_border,
      details_record = details_record,
      card_ids = card_ids,
      search_ids = search_ids
    ),
    asis = TRUE
  )

  if (not_null(details_record$default_image$thumbnail)) {
    img_path <- details_record$default_image$thumbnail
  } else {
    img_path <- "www/noun_icons/noun-no-image-3581362.svg"
  }
  tags$div(
    id = if (not_null(ns)) paste0(ns(id), "_stub") else paste0(id, "_stub"),
    class =
      "border-stub d-flex justify-content-between align-items-center",
    tags$div(
      class = "d-inline-flex justify-self-start",
      tags$div(
        class = "align-self-center me-2",
        tags$img(
          src = img_path,
          height = 35,
          width = 35,
          onerror = img_on_error
        )
      ),
      tags$div(
        class = "me-1",
        tags$div(stringr::str_to_title(details_record$common_name)),
        tags$div(tags$em(details_record$scientific_name))
      )
    ),
    tags$button(
      class = if (in_border) "btn btn-stub btn-stub-remove-plant me-2"
      else "btn btn-stub btn-stub-add-plant me-2",
      id = update_btn_id
    )
  )
}

update_border <- function(id, your_border, details_record, card_ids,
                          search_ids) {

  border_btn_id <- paste0(id, "-border_update")
  search_btn_id <- paste0("choose_plants-", id, "-search_update")

  card_id <- paste0("choose_plants-", id, "-card")
  card_btn_id <- paste0("choose_plants-", id, "-card_update")
#browser()
  if (id %in% names(your_border())) {

    # Remove plant from border
    your_border(pop_id(your_border(), id))

    # Remove border stub from offcanvas
    shiny::removeUI(selector =  paste0("#", id, "_stub"))

    if (length(your_border()) == 0) {
      shinyjs::show("empty_border", asis = TRUE)
    }

    # Update card if it's in the browse plants list
    if (id %in% card_ids()) {
      shinyjs::runjs(paste0("toggleColor('choose_plants-", id, "-card');"))
      removeClassAsis(card_btn_id, "btn-remove-plant")
      removeClassAsis(paste0(card_btn_id, "_text"), "remove-plant-text")
      addClassAsis(card_btn_id, "btn-add-plant")
      addClassAsis(paste0(card_btn_id, "_text"), "add-plant-text")
    }

    # Update stub if it's in the search results
    if (id %in% search_ids()) {
      removeClassAsis(search_btn_id, "btn-stub-remove-plant")
      addClassAsis(search_btn_id, "btn-stub-add-plant")
    }

  } else {

    # Add plant to border
    your_border(c(your_border(), rlang::list2(!!id := details_record)))

    if (length(your_border()) == 1) {
      shinyjs::hide("empty_border", asis = TRUE)
    }

    shiny::insertUI(
      selector = "#border_plants",
      ui = make_plant_stub(
        id = id,
        your_border = your_border,
        details_record = details_record,
        card_ids = card_ids,
        search_ids = search_ids
      )
    )

    # Update card if it's in the browse plants list
    if (id %in% card_ids()) {
      shinyjs::runjs(paste0("toggleColor('choose_plants-", id, "-card');"))
      removeClassAsis(card_btn_id, "btn-add-plant")
      removeClassAsis(paste0(card_btn_id, "_text"), "add-plant-text")
      addClassAsis(card_btn_id, "btn-remove-plant")
      addClassAsis(paste0(card_btn_id, "_text"), "remove-plant-text")
    }

    # Update stub if it's in the search results
    if (id %in% search_ids()) {
      removeClassAsis(search_btn_id, "btn-stub-add-plant")
      addClassAsis(search_btn_id, "btn-stub-remove-plant")
    }

  }
}



# shiny::removeUI(selector =  paste0("#", plant, "_stub"))
# if (length(border_ids) == 0) {
#   shinyjs::show("empty_border", asis = TRUE)
# }

# update_from_card <- function(id, species_details, your_border) {
#   if (id %in% names(your_border())) {
#     your_border(pop_id(your_border(), id))
#     shinyjs::removeClass("update", "btn-remove-plant")
#     shinyjs::removeClass("update_text", "remove-plant-text")
#     shinyjs::addClass("update", "btn-add-plant")
#     shinyjs::addClass("update_text", "add-plant-text")
#   } else {
#     your_border(c(your_border(), rlang::list2(!!id := species_details)))
#     shinyjs::removeClass("update", "btn-add-plant")
#     shinyjs::removeClass("update_plant_text", "add-plant-text")
#     shinyjs::addClass("update", "btn-remove-plant")
#     shinyjs::addClass("update_text", "remove-plant-text")
#   }
# }
#
# update_from_stub <- function(id, species_details, your_border,
#                              from_border = TRUE) {
#   if (id %in% names(your_border())) {
#     your_border(pop_id(your_border(), id))
#     shiny::removeUI(selector =  paste0("#", id, "_stub"))
#     if (length(your_border()) == 0) {
#       shinyjs::show("empty_border", asis = TRUE)
#     }
#     #TODO: Update search buttons too
#     if (!from_border) {
#       shinyjs::removeClass("update", "btn-stub-remove-plant", asis = from_border)
#       shinyjs::addClass("update", "btn-stub-add-plant", asis = from_border)
#     }
#   } else {
#     your_border(c(your_border(), rlang::list2(!!id := species_details)))
#     shinyjs::removeClass("update", "btn-stub-add-plant", asis = from_border)
#     shinyjs::addClass("update", "btn-remove-plant", asis = from_border)
#   }
# }

update_show_more <- function(rem_pages, rem_ids, any_results = FALSE) {
  shinyjs::show("show_more")
  shinyjs::removeClass(id = "show_more", "btn-spinner")
  shinyjs::removeClass(id = "show_more_text", "show-more-busy")
  if (length(rem_pages) == 0 && length(rem_ids) == 0) {
    if (!any_results) {
      show_no_results_modal()
      shinyjs::hide("show_more")
    } else {
      shinyjs::removeClass("show_more_text", "show-more-ready")
      shinyjs::addClass("show_more_text", "no-more")
      shinyjs::disable("show_more")
    }
  } else {
    shinyjs::enable("show_more")
    shinyjs::removeClass("show_more_text", "no-more")
    shinyjs::addClass("show_more_text", "show-more-ready")
    if (!any_results) {
      print("NOTHING FOUND YET")
      #TODO: Add 'nothing found yet' click show more to try again
    }
  }
}

