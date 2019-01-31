back.col <- "grey99"

geom_aes_defaults <- function() {
  geom_names <- apropos("^Geom", ignore.case = FALSE)
  geoms <- mget(geom_names, env = asNamespace("ggplot2"))
  map(geoms, ~ .$default_aes)
}

replace_geom_aes_defaults <- function(name, old_aes, new_aes) {
  matching_geoms <-
    map(geom_aes_defaults(), name) %>%
    compact() %>%
    keep( ~ . == old_aes)
  geoms <- gsub("^Geom(.*)", "\\1", names(matching_geoms))
  walk(geoms, update_geom_defaults, setNames(list(new_aes), name))
}
replace_geom_aes_defaults("colour", "black", "grey30")
replace_geom_aes_defaults("fill", "black", "grey30")


#' A beautiful ggplot theme
#'
#' This function makes your ggplots stick out.
#' @import tidyverse
#' @export
theme_dynge <- function() {
  # Ops?tter mit eget tema til ggplot.
  list(
    theme_classic(),
    theme(
      #text = element_text(family = "Helvetica", size = 10),
      legend.position = "top",
      panel.background = element_rect(
        fill = back.col,
        size = .5,
        colour = "grey25"
      ),
      plot.background = element_rect(fill = "white"),
      axis.line = element_blank(),
      legend.background = element_rect(fill = back.col, colour = "grey25"),
      legend.text = element_text(face = "italic"),
      legend.title = element_text(face = "bold"),
      legend.key = element_rect(fill = back.col, colour = "grey70"),
      strip.text.y = element_text(angle = 0),
      strip.background = element_blank(),
      plot.margin = unit(c(1, 1, .7, .5), "cm"),
      axis.ticks.length = unit(.2, "cm"),
      axis.ticks = element_blank()
    ),
    scale_fill_grey(),
    scale_color_grey()
  )
}
