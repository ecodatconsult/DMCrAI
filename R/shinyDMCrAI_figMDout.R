shinyDMCrAI_figMDout <- function(md_out){
  (md_out %>%
     dplyr::filter(category > 0) %>%
     dplyr::group_by(category_word) %>%
     dplyr::mutate(count = dplyr::n()) %>%
     dplyr::ungroup() %>%
     ggplot2::ggplot(ggplot2::aes(x = conf, color = category_word, fill = category_word))+
     ggplot2::scale_color_manual("Category", values = c("coral", "navyblue", "wheat"))+
     ggplot2::scale_fill_manual("Category", values = c("coral", "navyblue", "wheat"))+
     ggplot2::theme_minimal()+
     ggplot2::labs(x = "MD-Confidence", y = "Anzahl")+
     ggplot2::scale_y_log10()+
     ggiraph::geom_histogram_interactive(ggplot2::aes(tooltip = paste0(category_word, " (n = ", count, ")"), data_id = category_word))) %>%
    ggiraph::girafe(code = NULL) %>%
    ggiraph::girafe_options(ggiraph::opts_hover(css = "opacity:1;"),
                            ggiraph::opts_hover_inv(css = "opacity:0.2;"))
}
