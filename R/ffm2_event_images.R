#
#
# input <- list(project_id = "test", deployment = "wvb_ff_5034_220809", event_definition = 5, md_categories = "md_animal", md_threshold = 0.6)
#
# md_out <-
# download_import_megadetector_bboxes(
#   project_id = input$project,
#   deployment = input$deployment) %>%
#   dplyr::mutate(category = ifelse(conf <= input$md_threshold, 0, category)) %>%
#   dplyr::mutate(bbox_category = ifelse(conf <= input$md_threshold, "md_empty", bbox_category)) %>%
#   dplyr::group_by(file) %>%
#   dplyr::filter(dplyr::n() == 1 | conf > input$md_threshold) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(x_off = ifelse(conf < input$md_threshold, NA, x_off)) %>%
#   dplyr::mutate(y_off = ifelse(conf < input$md_threshold, NA, y_off)) %>%
#   dplyr::mutate(width = ifelse(conf < input$md_threshold, NA, width)) %>%
#   dplyr::mutate(height = ifelse(conf < input$md_threshold, NA, height)) %>%
#   dplyr::mutate(event_num = 1+cumsum(c(0,diff(as.numeric(datetimeoriginal)))>60*input$event_definition)) %>%
#   dplyr::mutate(event_id = paste0(project_id, "_", deployment, "_", event_num)) %>%
#   dplyr::group_by(event_id) %>%
#   dplyr::mutate(event_cat_num = sum(unique(bbox_category) %in% c("md_animal", "md_vehicle", "md_person"))) %>%
#   dplyr::mutate(event_category = ifelse(event_cat_num == 0, "md_empty",
#                                         ifelse(
#                                           event_cat_num > 1, "md_mixed",
#                                           bbox_category[bbox_category %in% c("md_animal", "md_vehicle", "md_person")][1])                                            )) %>%
#   dplyr::ungroup() %>%
#   dplyr::filter(event_category %in% input$md_categories)

#' @export
ffm2_event_images <- function(md_out, event_num_selected, scale_factor = .2){
# event_images <-
#   md_out %>%
#     dplyr::filter(event_num == 5) %>%
#     dplyr::filter(!duplicated(file_id)) %>%
#     dplyr::pull(file) %>%
#     sapply(magick::image_read) %>%
#     lapply(magick::image_resize, geometry = magick::geometry_size_percent(width = 100 * scale_factor)) %>%
#     lapply(magick::image_frame, color = "white", geometry = "10x10+3+3") %>%
#     do.call(what = c)
#


  md_out$df %>%
    dplyr::filter(event_num == event_num_selected) %>%
    dplyr::filter(!duplicated(file_id)) %>%
    dplyr::pull(file) %>%
    magick::image_read() |>
    magick::image_join() |>
    magick::image_resize(geometry = magick::geometry_size_percent(width = 100 * .15))
}


