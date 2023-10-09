shinyDMCrAI_figThreshold <- function(md_out_threshold, classified_images){
  (md_out_threshold %>%
     dplyr::filter(!duplicated(file)) %>%
     dplyr::group_by(dir_category) %>%
     dplyr::summarise(count = 100 * dplyr::n()/sum(classified_images)) %>%
     ggplot2::ggplot(ggplot2::aes(x = dir_category, y = count, fill = dir_category))+
     ggiraph::geom_col_interactive(ggplot2::aes(tooltip = paste0(round(count), "% aller Bilder"), data_id = dir_category))+
     ggplot2::scale_fill_manual(values = c("coral", "gray", "navyblue", "wheat", "deepskyblue4"))+
     ggplot2::theme_classic()+
     ggplot2::theme(legend.position = "none")+
     ggplot2::labs(x = "Kategorie", y = "Anteil nach Grenzwertfestlegung")) %>%
    ggiraph::girafe(code = NULL)
}
