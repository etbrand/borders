#' Insert plant cards
#'
#' Add plant cards to the UI
#' @param ns Namespacing function
#' @param details Named list of species details
#' @param border_ids IDs of the plants in your border
insert_plant_cards <- function(ns, details, border_ids) {
  plant_cards <- details |>
    purrr::imap(~ plant_card_UI(ns(.y), .x, .y %in% border_ids))
  shiny::insertUI(
    paste0("#", ns("show_more_div")),
    where = "beforeBegin",
    ui = tagList(!!!unname(plant_cards))
  )
}

#' Update plant list
#'
#' Update both the UI and the server logic for plant cards added to the plant
#' list
#' @param ns Namespacing function
#' @param details Named list of species details
#' @param your_border A reactiveVal with plant details for your border
#' @param card_ids A reactiveVal with the IDs of plants currently shown in the
#' plant list
#'@param search_ids A reactiveVal with the IDs of plants currently shown in the
#' search results
update_plant_list <- function(ns, details, your_border, card_ids, search_ids) {
  if (length(details) > 0) {
    insert_plant_cards(ns, details, names(your_border()))
    details |>
      purrr::iwalk(
        ~ plant_card_server(
          id = .y,
          details_record = .x,
          your_border = your_border,
          card_ids = card_ids,
          search_ids = search_ids
        )
      )
  }
}

#' Make filters
#'
#' Make UI for a pickerInput filter
#' @param id Filter ID
#' @param ns Namespacing function
#' @return The UI for a pickerInput filter
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

#' Make plant stub
#'
#' Make a plant stub for the search results or border Offcanvas
#' @param session Session object
#' @param ns Namespacing function, only used for stubs in the search results,
#' which live in the choose plants module
#' @param details_record Species details for the plant stub
#' @param your_border A reactiveVal with plant details for your border
#' @param card_ids A reactiveVal with the IDs of plants currently shown in the
#' plant list
#'@param search_ids A reactiveVal with the IDs of plants currently shown in the
#' search results
make_plant_stub <- function(session, id, details_record, your_border, card_ids,
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
      session = session,
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
          height = 40,
          width = 40,
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

#' Update border
#'
#' Update border when a plant is added or removed. Takes care of updating any
#' relevant UI in the border offCanvas, search results, and plant card list.
#' @inheritParams make_plant_stub
update_border <- function(session, id, your_border, details_record, card_ids,
                          search_ids) {

  border_btn_id <- paste0(id, "-border_update")
  search_btn_id <- paste0("choose_plants-", id, "-search_update")

  card_id <- paste0("choose_plants-", id, "-card")
  card_btn_id <- paste0("choose_plants-", id, "-card_update")

  if (id %in% names(your_border())) {

    # Remove plant from border
    your_border(pop_id(your_border(), id))

    # Remove border stub from offcanvas
    shiny::removeUI(selector =  paste0("#", id, "_stub"))

    if (length(your_border()) == 0) {
      shinyjs::hide("save_border", asis = TRUE)
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
      shinyjs::show("save_border", asis = TRUE)
    }

    shiny::insertUI(
      selector = "#border_plants",
      ui = make_plant_stub(
        session = session,
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

  shinyjs::runjs(paste0(
    "window.location.hash = '", update_url(session, your_border()), "';"))

}

# Update url for bookmarking
update_url <- function(session, your_border) {

  current_url <- isolate(session$clientData$url_hash)
  split_url <- strsplit(current_url, "_")[[1]]

  if (length(your_border) == 0) {
    if (length(split_url) == 4) {
      new_url <- paste(split_url[c(1:2, 4)], collapse = "_")
    } else {
      new_url <- paste(split_url[1:2], collapse = "_")
    }
  } else {
    id_str <- paste(to_api_ids(names(your_border)), collapse = "-")
    if (length(split_url) == 4) {
      new_url <- paste0(c(split_url[1:2], id_str, split_url[4]), collapse = "_")
    } else {
      new_url <- paste0(c(split_url[1:2], id_str), collapse = "_")
    }
  }
  new_url
}

#' Update show more
#'
#' Update the show more button
#' @param rem_pages Remaining pages from the species list request
#' @param rem_ids Remaining IDs from the last batch of species list pages
#' @param status API status ("OK" or "api_error")
#' @param any_result Boolean - are there any results already shown in the plant
#' list?
update_show_more <- function(rem_pages, rem_ids, status, any_results = FALSE) {
  shinyjs::show("show_more")
  shinyjs::removeClass(id = "show_more", "btn-spinner")
  shinyjs::removeClass(id = "show_more_text", "show-more-busy")
  if (length(rem_pages) == 0 && length(rem_ids) == 0) {
    if (status == "OK" && !any_results) {
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
  }
}

#' Compose filters
#'
#' Combine pickerInput filters and checkbox filters
#' @param select_filters pickerInput filters
#' @param checkbox_filters checkboxInput filters
#' @param zone Hardiness zone
#' @return A list with
#' \item{url_args}{URL arguments that can be passed to a species list request}
#' \item{addl_filters}{Additional filters to be applied after the species list
#' request has been performed}
compose_filters <- function(select_filters, checkbox_filters, zone) {
  edible_filter <-
    if (checkbox_filters$edible_only) list(edible = 1) else NULL
  poisonous_filter <-
    if (checkbox_filters$nonpoisonous_only) list(poisonous = 0) else NULL
  hardiness_filter <- list(hardiness = hardiness_range(zone))

  url_args <- select_filters |>
    c(hardiness_filter, edible_filter, poisonous_filter)
  url_args$flower_color <- NULL

  addl_filters <- list(
    include_no_img = checkbox_filters$include_no_img,
    include_trees = checkbox_filters$include_trees
  )

  if (not_null(select_filters$flower_color)) {
    addl_filters <- addl_filters |>
      c(list(flower_color = select_filters$flower_color))
  }

  list(url_args = url_args, addl_filters = addl_filters)

}
