prepare_md_out_upload <-function(md_out_events_df,
                                     vehicle_threshold,
                                     animal_threshold,
                                     person_threshold,
                                     md_obs_cats = c("md_animal", "md_person", "md_vehicle")){
  md_out_events_df %>%
    dplyr::mutate(category_word = ifelse(
      (category_word == "md_vehicle" & conf > vehicle_threshold) |
        (category_word == "md_animal" & conf > animal_threshold) |
        (category_word == "md_person" & conf > person_threshold),
      category_word,
      "md_empty")) %>%
    dplyr::group_by(file) %>%
    dplyr::mutate(empty_box = dplyr::n() == 1 & category_word == "md_empty") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(conf = ifelse(empty_box, 0 , conf),
                  category = ifelse(empty_box, 0 , category),
                  x_off = ifelse(empty_box, NA , x_off),
                  y_off = ifelse(empty_box, NA , y_off),
                  width = ifelse(empty_box, NA , width),
                  height = ifelse(empty_box, NA , height)
    ) %>%
    dplyr::filter(!(!is.na(x_off) & category_word == "md_empty")) %>%
    dplyr::group_by(event_id) %>%
    dplyr::mutate(event_category = ifelse(
      length(unique(category_word)) == 1,
      category_word,
      ifelse(
        sum(unique(category_word) %in% md_obs_cats) > 1,
        "md_mixed",
        md_obs_cats[md_obs_cats %in% unique(category_word)]))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(event_category != "md_empty")
}
