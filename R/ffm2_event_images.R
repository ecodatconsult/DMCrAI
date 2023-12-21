
#' @export
ffm2_event_images <- function(md_out, event_num_selected, scale_factor = .2){

  md_out$df %>%
    dplyr::filter(event_num == event_num_selected) %>%
    dplyr::filter(!duplicated(file_id)) %>%
    dplyr::pull(file) %>%
    magick::image_read() |>
    magick::image_join() |>
    magick::image_resize(geometry = magick::geometry_size_percent(width = 100 * scale_factor))
}


