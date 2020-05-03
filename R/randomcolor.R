# Author: Ron Ammar
# Description: Get pretty random colors in R. Wraps randomColor.js.

#' Get a pretty random color.
#'
#' @param count number of colors (>= 1)
#' @param hue The hue of the colors to be selected.
#' @param luminosity The luminosity of the colors to be selected.
#' @param seed number to be repeatable.
#' @return A character vector of random color hexadecimal codes.

#' @examples
#' randomColor()
#'
#' randomColor(hue="pink")
#'
#' randomColor(10, luminosity="light")
#' @export
#' @import stringr
randomColor <- function(count=1,
                        hue=c(" ", "random", "red", "orange", "yellow", "green", "blue", "purple", "pink", "monochrome"),
                        luminosity=c(" ", "random", "light", "bright", "dark"),
                        seed=42) {
  hue <- match.arg(hue)
  luminosity <- match.arg(luminosity)
  cmd <- sprintf("randomColor({hue:'%s', luminosity:'%s', count:%d, seed:%d})",
                 hue, luminosity, count, seed)  
  str_split(ct$eval(cmd), ",")[[1]]
}


#' Generate palettes of optimally distinct colors.
#' @description Inspired by the the theory from http://tools.medialab.sciences-po.fr/iwanthue/theory.php
#' For more info, also see https://en.wikipedia.org/wiki/Lab_color_space
#'
#' @param k number of colors (>= 1). May be ineffective for k > 40.
#' @param altCol Use an alternate color space
#' @param runTsne Preprocess color space with t-SNE to obtain distinct colors. Reduces performance.
#' @param seed number to be repeatable.
#' @return A character vector of k optimally distinct colors in hexadecimal codes.
#' @export
#' @import cluster Rtsne scales
distinctColorPalette <-function(k=1, altCol=FALSE, runTsne=FALSE, seed=42) {
    
  # Compute a 2000 color spectrum and convert to LAB
  runif(1)
  old_seed <- .Random.seed ## Keep this seed work only locally https://stackoverflow.com/a/14324316/1608734
  on.exit({.Random.seed <<- old_seed})
  set.seed(seed)
  n <- 2e3
  currentColorSpace <- colorspace::RGB(runif(n), runif(n), runif(n))
  currentColorSpace <- as(currentColorSpace, "LAB")
  currentColorSpace <- currentColorSpace@coords
  if (altCol) {
    currentColorSpace <- t(unique(grDevices::col2rgb(scales::hue_pal(l=60:100)(n)))) ## Note: hue_pal no longer accepts multiple l
  }

  if (runTsne) {
    # Run 2D t-SNE before clustering
    tsne <- Rtsne(currentColorSpace, perplexity=50, check_duplicates=FALSE, pca=FALSE, max_iter=500)
    pamx <- pam(tsne$Y, k)  # k-medoids
    if (altCol) {
      colors <- rgb(currentColorSpace[pamx$id.med, ], maxColorValue=255)
    } else {
      colors <- hex(LAB(currentColorSpace[pamx$id.med, ]))
    }
  } else {
    # Set iter.max to 20 to avoid convergence warnings.
    km <- kmeans(currentColorSpace, k, iter.max=20)
    if (altCol) {
      colors <- rgb(round(km$centers), maxColorValue=255)
    } else {
      colors <- unname(hex(LAB(km$centers)))
    }
  }

  return(colors)
}
