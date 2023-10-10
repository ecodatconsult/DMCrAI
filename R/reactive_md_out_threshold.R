reactive_md_out_threshold <-function(md_out,
                                     md_out_table_df,
                                     vehicle_threshold,
                                     animal_threshold,
                                     person_threshold,
                                     md_obs_cats = c("md_animal", "md_person", "md_vehicle")){
    md_out %>%
    dplyr::mutate(category_word = ifelse(
      (category_word == "md_vehicle" & conf > vehicle_threshold) |
        (category_word == "md_animal" & conf > animal_threshold) |
        (category_word == "md_person" & conf > person_threshold),
      category_word,
      "md_empty")) %>%
    dplyr::left_join(md_out_table_df %>%
                       dplyr::select(file, event_id)) %>%
    dplyr::group_by(event_id) %>%
    dplyr::mutate(dir_category = ifelse(
      length(unique(category_word)) == 1,
      category_word,
      ifelse(
        sum(unique(category_word) %in% md_obs_cats) > 1,
        "md_mixed",
        md_obs_cats[md_obs_cats %in% unique(category_word)]))) %>%
    dplyr::ungroup()
}
