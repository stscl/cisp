#' spatial pattern correlation
#'
#' @param data A `data.frame`, `tibble` or `sf` object of observation data.
#' @param overlay (optional) Spatial overlay method. One of `and`, `or`, `intersection`.
#' Default is `and`.
#' @param discnum (optional) A vector of number of classes for discretization. Default is `3:8`.
#' @param discmethod (optional) A vector of methods for discretization, default is using
#' `c("sd","equal","geometric","quantile","natural")` by invoking `sdsfun`.
#' @param cores (optional) Positive integer (default is 1). When cores are greater than 1,
#' use parallel computing.
#'
#' @return A list.
#' \describe{
#' \item{\code{cor_tbl}}{A tibble with power of spatial pattern correlation}
#' \item{\code{cor_mat}}{A matrix with power of spatial pattern correlation}
#' }
#' @export
#'
#' @examples
#' sim1 = sf::st_as_sf(gdverse::sim,coords = c('lo','la'))
#' sim1
#' \donttest{
#' g = spc(sim1, discnum = 3:6, cores = 1)
#' g
#' plot(g,"matrix")
#' }
spc = \(data, overlay = 'and', discnum = 3:8,
        discmethod = c("sd","equal","geometric","quantile","natural"),
        cores = 1){
  if (inherits(data,'sf')) {
    data = sf::st_drop_geometry(data)
  }
  xsname = names(data)
  calcul_spcv = \(yname,data,overlay,discn,discm,cores){
    opgd_res = gdverse::gd_optunidisc(
      paste0(yname," ~ ."), data = data, discnum = discn,
      discmethod = discm, cores = cores
    )
    dti = dplyr::bind_cols(dplyr::select(data,dplyr::all_of(yname)),opgd_res$disc)
    sshmcv = cisp::ssh_marginalcontri(paste0(yname," ~ ."), data = dti,
                                      overlay = overlay, cores = cores)
    return(sshmcv$spd)
  }
  res = purrr::map_dfr(xsname,
                       \(.x) calcul_spcv(.x, data = data, overlay = overlay,
                                         discn = discnum, discm = discmethod,
                                         cores = cores) |>
                     dplyr::mutate(yv = .x) |>
                     dplyr::rename(xv = variable,
                                   correlation = spd) |>
                     dplyr::select(xv,yv,correlation))
  res_mat = res |>
    tidyr::pivot_wider(names_from = yv, values_from = correlation) |>
    tibble::column_to_rownames(var = 'xv') |>
    as.matrix()
  res_mat[is.na(res_mat)] = 1
  res = list("cor_tbl" = res,"cor_mat" = res_mat)
  class(res) = 'spc_result'
  return(res)
}

#' @title print spc result
#' @export
#' @noRd
print.spc_result = \(x, ...) {
  cat("***   Spatial Pattern Correlation    ")
  print(knitr::kable(x$cor_tbl, format = "markdown", digits = 5, align = 'c', ...))
}

#' @title plot spc result
#' @export
#' @noRd
plot.spc_result = \(x, style = c("network","matrix"), ...) {
  style = match.arg(style)
  switch(style,
         "network" = {
           g = igraph::graph_from_data_frame(x$cor_tbl, directed = TRUE)
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
           g = x$cor_tbl
           fig_g = ggplot2::ggplot(data = g,
                                   ggplot2::aes(x = yv, y = xv, fill = correlation)) +
             ggplot2::geom_tile(color = "white") +
             ggplot2::scale_fill_gradient2(low = "blue", mid = "gray",
                                           high = "red", midpoint = 0) +
             ggplot2::geom_text(ggplot2::aes(label = round(correlation, 3)), color = "black") +
             ggplot2::labs(x = "Dependent Variable", y = "Independent Variable") +
             ggplot2::coord_equal() +
             ggplot2::theme_void() +
             ggplot2::theme(
               axis.text.x = ggplot2::element_text(angle = 90),
               axis.text.y = ggplot2::element_text(color = "black"),
               axis.title.y = ggplot2::element_text(angle = 90),
               axis.title = ggplot2::element_text(face = "italic", color = "red"),
               panel.grid = ggplot2::element_blank(),
               panel.border = ggplot2::element_blank()
             )
  })
  return(fig_g)
}
