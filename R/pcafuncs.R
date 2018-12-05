#'
#'
#'
#' @import ggrepel
#' @export
pca_biplot <- function(model) {
  if (!(TRUE %in% (class(model) == "PCA"))) {
    stop("You must enter a model of a PCA from the FactoMineR package.")
  }

}

#'
#'
#'
#'
#' @export
pca_scree <- function(model, type = c("eig", "var", "cvar")) {

}

#'
#'
#'
#' @export
pca_QoR <- function(model) {
  if (!(TRUE %in% (class(model) == "PCA"))) {
    stop("You must enter a model of a PCA from the FactoMineR package.")
  }

  data <- as.tibble(model$var$cos2)
  data$Variable <- rownames(model$var$coord)
  data <- gather(data, key = Dimension, value = QoR, -Variable)
  plot <- ggplot(data, aes(Dimension, Variable)) +
    geom_tile(aes(fill = QoR), color = "black") +
    theme_dynge() +
    scale_fill_continuous(
      low = brewer.pal(3, "Blues")[1],
      high = brewer.pal(3, "Blues")[3],
      limits = c(0, 1),
      breaks = seq(0, 1, .25)
    )
  plot
  return(list(data = data, plot = plot))
}

#'
#'
#'
#' @import ggrepel
#' @export
pca_loading_circle <- function(model) {
  if (!(TRUE %in% (class(model) == "PCA"))) {
    stop("You must enter a model of a PCA from the FactoMineR package.")
  }


}

#'
#'
#'
#' @import ggrepel
#' @export
pca_scatter <- function(model,
                        point_id = NULL,
                        dim = c(1, 2)) {
  if (!(TRUE %in% (class(model) == "PCA"))) {
    stop("You must enter a model of a PCA from the FactoMineR package.")
  }
  data <- as.tibble(model$ind$coord)
  if (!is.null(point_id)) {
    if (length(point_id) != nrow(model$ind$coord)) {
      if (nrow(model$ind$coord) %% length(point_id) == 0) {
        warning(
          cat(
            "The number of observations and the ID vector does not match. This results in repetition of labels ",
            nrow(model$ind$coord) / length(point_id),
            " times.",
            sep = ""
          )
        )
      } else {
        stop(
          "The number of observations (",
          nrow(model$ind$coord),
          ") and the ID vector ("
          ,
          length(point_id) ,
          ") does not add up.",
          sep = ""
        )
      }
    }
    data$id <- point_id
    text_id <- function() {
      ggrepel::geom_text_repel(aes(label = id), col = "grey75")
    }

  } else {
    text_id <- function() {

    }
  }
  ggplot(data, aes(data[[dim[1]]] , data[[dim[2]]])) +
    geom_hline(yintercept = 0,
               col = "grey80",
               lty = "dotted") +
    geom_vline(xintercept = 0,
               col = "grey80",
               lty = "dotted") +
    geom_point() +
    text_id() +
    theme_dynge() +
    labs(x = colnames(data)[dim[1]], y = colnames(data)[dim[2]])

}
