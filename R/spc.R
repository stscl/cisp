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
#' \item{\code{correlation_tbl}}{A tibble with power of spatial pattern correlation}
#' \item{\code{correlation_mat}}{A matrix with power of spatial pattern correlation}
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
    rgd_res = gdverse::rgd(paste0(yname," ~ ."), data = data, discnum = discnum,
                           minsize = minsize, strategy = strategy,
                           increase_rate = increase_rate, cores = cores)
    dti = dplyr::bind_cols(dplyr::select(data,dplyr::all_of(yname)),rgd_res$opt_disc)
    sshmcv = cisp::ssh_marginalcontri(paste0(yname," ~ ."), data = dti,
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
                     dplyr::select(xv,yv,correlation))
  res_mat = res |>
    tidyr::pivot_wider(names_from = yv, values_from = correlation) |>
    tibble::column_to_rownames(var = 'xv') |>
    as.matrix()
  res_mat[is.na(res_mat)] = 1
  res = list("correlation_tbl" = res,"correlation_mat" = res_mat)
  class(res) = 'spc_result'
  return(res)
}

#' @title print spc result
#' @export
#' @noRd
#'
print.spc_result = \(x, ...) {
  cat("***   Spatial Pattern Correlation    ")
  print(knitr::kable(x$correlation_tbl, format = "markdown", digits = 5, align = 'c', ...))
}

#' @title plot spc result
#' @export
#' @noRd
#'
plot.spc_result = \(x, style = c("network","matrix"), ...) {
  style = match.arg(style)
  switch(style,
         "network" = {
           g = igraph::graph_from_data_frame(x$correlation_tbl, directed = TRUE)
           fig_g = ggraph::ggraph(g, layout = "circle") +
             ggraph::geom_edge_arc(ggplot2::aes(width = abs(correlation), color = correlation),
                                   arrow = grid::arrow(type = "closed", length = grid::unit(3, "mm")),
                                   end_cap = ggraph::circle(3, 'mm')) +
             ggraph::geom_node_point(size = 5) +
             ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE) +
             ggraph::scale_edge_color_gradient2(low = "blue", mid = "gray",
                                                high = "red", midpoint = 0,
                                                guide = ggraph::guide_edge_colorbar(
                                                  barwidth = 10,
                                                  barheight = 0.5,
                                                  label.theme = ggplot2::element_text(size = 10,
                                                                                      margin = ggplot2::margin(t = 2, b = 2))
                                                )) +
             ggraph::scale_edge_width(range = c(0.5, 2), guide = 'none') +
             ggplot2::theme_void() +
             ggplot2::theme(legend.position = "bottom") +
             ggplot2::labs(edge_color = "Strength")
         },
         "matrix" = {
           g = x$correlation_tbl
           fig_g = ggplot2::ggplot(data = g,
                                   ggplot2::aes(x = yv, y = xv, fill = correlation)) +
             ggplot2::geom_tile(color = "white") +
             ggplot2::scale_fill_gradientn(colors = RColorBrewer::brewer.pal(11, "RdBu"),
                                           limits = range(g$correlation)) +
             ggplot2::geom_text(ggplot2::aes(label = round(correlation, 3)), color = "black") +
             ggplot2::labs(x = "Dependent Variable", y = "Independent Variable") +
             ggplot2::coord_equal() +
             ggplot2::theme_void() +
             ggplot2::theme(
               axis.text.x = ggplot2::element_text(angle = 90),
               axis.title = ggplot2::element_text(face = "bold"),
               panel.grid = ggplot2::element_blank(),
               panel.border = ggplot2::element_blank()
             )
  })
  return(fig_g)
}
