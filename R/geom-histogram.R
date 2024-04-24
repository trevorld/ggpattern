#' @rdname geom-docs
#' @export
geom_histogram_pattern <- function (mapping = NULL, data = NULL,
                                    stat = "bin", position = "stack",
                                    ...,
                                    binwidth = NULL,
                                    bins = NULL,
                                    na.rm = FALSE,
                                    orientation = NA,
                                    show.legend = NA,
                                    inherit.aes = TRUE) {

    layer(
          data = data,
          mapping = mapping,
          stat = stat,
          geom = GeomBarPattern,
          position = position,
          show.legend = show.legend,
          inherit.aes = inherit.aes,
    params = list2(
            binwidth = binwidth,
            bins = bins,
            na.rm = na.rm,
            orientation = orientation,
            pad = FALSE,
            ...
        )
    )
}
