itcer_pais <- function(fecha) {
  dln_itcer %>%
    filter(Meses == fecha) %>%
    group_by(Meses,paisx) %>%
    summarise(dln_itcer = sum(dln_itcer)) %>%
    ggplot() +
    aes(x=reorder(paisx, dln_itcer),y=dln_itcer) +
    geom_bar(stat = "identity") +
    facet_wrap(. ~ Meses, scales="free_x")  +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 90)) +
    coord_flip() +
    labs(
      x = "",
      y = ""
    )
}