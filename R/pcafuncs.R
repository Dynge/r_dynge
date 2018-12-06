#' Classic Screeplot
#'
#' This function constructs a screeplot based on either the eigenvalues, percentage of variance or the cummulative percentage of variance.
#' this can be done by adjusting the type variable.
#' @param model The PCA model from FactoMineR.
#' @param type The type of model. This can be one of three: "eig" (eigenvalues),
#' "pov" (proportion of variance) or
#' "cpov" (cummulative proportion of variance).
#' Defaults to "cpov".
#' @return
#' \item{data}{Data used in the plot.}
#' \item{plot}{The ggplot object.}
#' @export
pca_scree <- function(model, type = "cpov") {
  if (!(TRUE %in% (class(model) == "PCA"))) {
    stop("You must enter a model of a PCA from the FactoMineR package.")
  }
  data <- tibble(
    eig = model$eig[, 1],
    pov = model$eig[, 2],
    cpov = model$eig[, 3],
    PC = seq_len(nrow(model$eig))
  )
  plot <- ggplot(data, aes(PC, data[[type]])) +
    geom_point() +
    geom_line() +
    theme_dynge() +
    ylab(type)
  return(list(data = data,
              plot = plot))
}

#' Quality of Representation
#'
#' This function plot the different attributes of your PCA model and
#' their respective quality of representation to the different principal components.
#' @param model The PCA model from FactoMineR.
#' @param labels Boolean indicating if values should be plotted or not. Defaults to TRUE.
#' @return
#' \item{data}{The data used to make the plot.}
#' \item{plot}{The ggplot object.}
#' @export
pca_QoR <- function(model, labels = TRUE) {
  if (!(TRUE %in% (class(model) == "PCA"))) {
    stop("You must enter a model of a PCA from the FactoMineR package.")
  }

  data <- as.tibble(model$var$cos2)
  data$Variable <- rownames(model$var$coord)
  data <- gather(data, key = Dimension, value = QoR, -Variable)
  if (labels) {
    labeling <- function() {
      geom_text(aes(label = round(QoR, 1)))
    }
  } else {
    labeling <- function() {

    }
  }
  plot <- ggplot(data, aes(Dimension, Variable)) +
    geom_tile(aes(fill = QoR), color = "grey25") +
    labeling() +
    theme_dynge() +
    scale_fill_continuous(
      low = brewer.pal(3, "Blues")[1],
      high = brewer.pal(3, "Blues")[3],
      limits = c(0, 1),
      breaks = seq(0, 1, .25)
    )
  return(list(data = data, plot = plot))
}

#' Loadings Circle
#'
#' This function creates a unity circle aswell as the correlations between the original
#' variables and the principal components of your PCA model. You can choose which dimensions to plot,
#' aswell as the threshold for correlations to be shown.
#' @param model The PCA model from FactoMineR.
#' @param dim The dimensions to be plotted. Defaults to c(1,2).
#' @param min_cor The minimal correlation value to plot arrow. Defaults to .3.
#' @return
#' \item{loadings}{Data of the variable loadings on the dimensions.}
#' \item{plot}{The ggplot object.}
#' @import ggrepel
#' @export
pca_loading_circle <- function(model,
                               dim = c(1, 2),
                               min_cor = .3) {
  if (!(TRUE %in% (class(model) == "PCA"))) {
    stop("You must enter a model of a PCA from the FactoMineR package.")
  }
  loadings_full <- as.tibble(model$var$coord)
  loadings_full$Variable <- rownames(model$var$coord)
  loadings <- loadings_full %>%
    filter(abs(loadings_full[[dim[1]]]) > min_cor |
             abs(loadings_full[[dim[2]]]) > min_cor)

  circleFun <- function(center = c(0, 0),
                        radius = 1,
                        npoints = 100) {
    tt <- seq(0, 2 * pi, length.out = npoints)
    xx <- center[1] + radius * cos(tt)
    yy <- center[2] + radius * sin(tt)
    return(tibble(x = xx, y = yy))
  }
  circle_data <- circleFun()
  plot <- ggplot(loadings, aes(loadings[[dim[1]]],
                               loadings[[dim[2]]])) +
    geom_path(data = circle_data, aes(x, y), col = "grey80") +
    geom_hline(yintercept = 0,
               col = "grey80",
               lty = "dotted") +
    geom_vline(xintercept = 0,
               col = "grey80",
               lty = "dotted") +
    geom_segment(
      aes(
        x = 0,
        y = 0,
        xend = loadings[[dim[1]]],
        yend = loadings[[dim[2]]]
      ),
      col = "grey60",
      arrow = arrow(length = unit(.25, "cm"), type = "closed")
    ) +
    ggrepel::geom_text_repel(aes(label = Variable),
                             col = "grey60") +
    theme_dynge() +
    labs(
      x = paste("Dimension ", dim[1], " (", round(model$eig[dim[1], 2], 1), "%)", sep = ""),
      y = paste("Dimension ", dim[2], " (", round(model$eig[dim[2], 2], 1), "%)", sep = "")
    )
  return(list(data = loadings_full,
              plot = plot))


}

#' Biplot of a PCA model
#'
#' This function creates a biplot of the PCA model. You can adjust which dimensions to plot,
#' aswell as the threshold for correlations to be shown. You can also forego the correlation arrows with biplot = FALSE.
#'
#' @param model The PCA model from FactoMineR.
#' @param point_id A vector of labels to the individual points. Ignore this if you do not want labels.
#' @param dim The dimensions to be plotted. Defaults to c(1,2).
#' @param min_cor The minimal correlation value to plot arrow. Defaults to .3.
#' @param biplot A boolean for whether or not to produce a biplot and therefore add correlation arrows. Defaults to TRUE
#' @return
#' \item{ind}{Data of the individual coordinates on the dimensions.}
#' \item{loadings}{Data of the variable loadings on the dimensions.}
#' \item{plot}{The ggplot object.}
#' @import ggrepel
#' @export
pca_biplot <- function(model,
                       point_id = NULL,
                       dim = c(1, 2),
                       min_cor = .3,
                       biplot = TRUE) {
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
  if (biplot) {
    loadings_full <- as.tibble(model$var$coord)
    loadings_full$Variable <- rownames(model$var$coord)
    loadings <- loadings_full %>%
      filter(abs(loadings_full[[dim[1]]]) > min_cor |
               abs(loadings_full[[dim[2]]]) > min_cor)
    loading_arrow <- function() {
      list(
        geom_segment(
          data = loadings,
          aes(
            x = 0,
            y = 0,
            xend = loadings[[dim[1]]],
            yend = loadings[[dim[2]]]
          ),
          col = "grey60",
          arrow = arrow(length = unit(.15, "cm"), type = "closed")
        ),
        geom_text_repel(
          data = loadings,
          aes(loadings[[dim[1]]],
              loadings[[dim[2]]],
              label = Variable),
          col = "grey60"
        )
      )

    }
  } else {
    loading_arrow <- function() {

    }
  }
  plot <- ggplot(data, aes(data[[dim[1]]] , data[[dim[2]]])) +
    geom_hline(yintercept = 0,
               col = "grey80",
               lty = "dotted") +
    geom_vline(xintercept = 0,
               col = "grey80",
               lty = "dotted") +
    loading_arrow() +
    geom_point() +
    text_id() +
    theme_dynge() +
    labs(
      x = paste("Dimension ", dim[1], " (", round(model$eig[dim[1], 2], 1), "%)", sep = ""),
      y = paste("Dimension ", dim[2], " (", round(model$eig[dim[2], 2], 1), "%)", sep = "")
    )
  return(list(
    ind = data,
    loadings = loadings_full,
    plot = plot
  ))

}
