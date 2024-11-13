#' spatial pattern correlation
#'
#' @param data A `data.frame`, `tibble` or `sf` object of observation data.
#' @param overlay (optional) Spatial overlay method. One of `and`, `or`, `intersection`.
#' Default is `and`.
#' @param discnum A numeric vector of discretized classes of columns that need to be discretized.
#' Default all `discvar` use `3:8`.
#' @param minsize (optional) The min size of each discretization group. Default all use `1`.
#' @param strategy (optional) Optimal discretization strategy. When `strategy` is `1L`, choose the highest
#' q-statistics to determinate optimal spatial data discretization parameters. When `strategy` is `2L`,
#' The optimal discrete parameters of spatial data are selected by combining LOESS model.
#' @param increase_rate (optional) The critical increase rate of the number of discretization. Default is `5%`.
#' @param cores (optional) Positive integer (default is 1). When cores are greater than 1, use
#' multi-core parallel computing.
#'
#' @return A list.
#' \describe{
#' \item{\code{correlation}}{power of spatial pattern correlation}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' ## The following code needs to configure the Python environment to run:
#' sim1 = sf::st_as_sf(gdverse::sim,coords = c('lo','la'))
#' g = spc(sim1, discnum = 3:6, cores = 1)
#' g
#' }
#'
spc = \(data, overlay = 'and', discnum = 3:8, minsize = 1,
        strategy = 2L, increase_rate = 0.05, cores = 1){
  if (inherits(data,'sf')) {
    data = sf::st_drop_geometry(data)
  }
  xsname = names(data)
  calcul_spcv = \(yname,data,overlay,discnum,minsize,strategy,increase_rate,cores){
    rgd_res = gdverse::rgd(paste0(yname," ~ ."),data = data, discnum = discnum,
                           minsize = minsize, strategy = strategy,
                           increase_rate = increase_rate, cores = cores)
    dti = dplyr::bind_cols(dplyr::select(data,dplyr::all_of(yname)),rgd_res$opt_disc)
    sshmcv = ssh_marginalcontri(paste0(yname," ~ ."), data = dti,
                                overlay = overlay, cores = cores)
    return(sshmcv$spd)
  }
  res = purrr::map_dfr(xsname,
                       \(.x) calcul_spcv(.x,data = data,overlay = overlay,
                              discnum = discnum,minsize = minsize,strategy = strategy,
                              increase_rate = increase_rate, cores = cores) |>
                     dplyr::mutate(yv = .x) |>
                     dplyr::rename(xv = variable,
                                   correlation = spd) |>
                     dplyr::select(yv,dplyr::everything()))
  res = list("correlation" = res)
  class(res) = 'spc_result'
  return(res)
}

#' @title print spc result
#' @export
#' @noRd
#'
print.spc_result = \(x, ...) {
  cat("***   Spatial Pattern Correlation    ")
  print(knitr::kable(x$correlation, format = "markdown", digits = 12, align = 'c', ...))
}

#' @title plot spc result
#' @export
#' @noRd
#'
plot.spc_result = \(x, ...) {
  g = igraph::graph_from_data_frame(x$correlation, directed = TRUE)
  fig_g = ggraph::ggraph(g, layout = "circle") +
    ggraph::geom_edge_arc(ggplot2::aes(width = abs(correlation), color = correlation),
                          arrow = grid::arrow(type = "closed", length = grid::unit(3, "mm")),
                          end_cap = ggraph::circle(3, 'mm')) +
    ggraph::geom_node_point(size = 5) +
    ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE) +
    ggraph::scale_edge_color_gradient2(low = "blue", mid = "gray",
                                       high = "red", midpoint = 0,
                                       guide = "colorbar") +
    ggraph::scale_edge_width(range = c(0.5, 2)) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(edge_color = "Strength", edge_width = "Intensity")
  return(fig_g)
}
