#' spatial association marginal contributions derived from spatial stratified heterogeneity
#'
#' @param formula A formula of ISP model.
#' @param data A `data.frame`, `tibble` or `sf` object of observation data.
#' @param overlay (optional) Spatial overlay method. One of `and`, `or`, `intersection`.
#' Default is `and`.
#' @param cores (optional) Positive integer (default is 1). When cores are greater than 1, use
#' multi-core parallel computing.
#'
#' @return A list.
#' \describe{
#' \item{\code{pd}}{robust power of determinants}
#' \item{\code{spd}}{shap power of determinants}
#' \item{\code{determination}}{determination of the optimal interaction of variables}
#' }
#' @export
#'
#' @examples
#' NTDs1 = sf::st_as_sf(gdverse::NTDs, coords = c('X','Y'))
#' g = ssh_marginalcontri(incidence ~ ., data = NTDs1, cores = 1)
#' g
#'
ssh_marginalcontri = \(formula, data, overlay = 'and', cores = 1){
  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)

  if (inherits(data,'sf')) {
    data = sf::st_drop_geometry(data)
  }
  data = tibble::as_tibble(data)

  if (formula.vars[2] != "."){
    data = dplyr::select(data,dplyr::all_of(formula.vars))
  }

  yname = formula.vars[1]
  xname = colnames(data)[-which(colnames(data) == yname)]
  xs = sdsfun::generate_subsets(xname,empty = FALSE, self = TRUE)
  spfom = overlay

  pd_mc = \(formula, discdata, overlaymethod = 'and'){
    formula = stats::as.formula(formula)
    formulavars = all.vars(formula)
    if (formula.vars[2] != "."){
      discdata = dplyr::select(discdata,dplyr::all_of(formulavars))
    }
    yname = formula.vars[1]
    if (overlaymethod == 'intersection'){
      fuzzyzone = discdata |>
        dplyr::select(-dplyr::any_of(yname)) |>
        purrr::reduce(paste,sep = '_')
    } else {
      fuzzyzone = sdsfun::fuzzyoverlay(formula,discdata,overlaymethod)
    }
    qtheta = sdsfun::geodetector_q(discdata[,yname,drop = TRUE],fuzzyzone)
    return(qtheta)
  }

  calcul_pd = \(.x,dti){
    qv = pd_mc(paste(yname,'~',paste0(.x,collapse = '+')),dti,spfom)
    names(qv) = "pd"
    return(qv)
  }

  doclust = FALSE
  if (cores > 1) {
    doclust = TRUE
    cores = parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cores), add=TRUE)
  }

  if (doclust) {
    parallel::clusterExport(cores)
    out_pdv = parallel::parLapply(cores, xs, calcul_pd, dti = data)
    out_pdv = tibble::as_tibble(do.call(rbind, out_pdv))
  } else {
    out_pdv = purrr::map_dfr(xs, calcul_pd, dti = data)
  }
  out_pdv = dplyr::pull(out_pdv,1)
  m = length(xname)
  mf = factorial(m)

  get_value_by_varname = \(fv,namelist,valuevec){
    for (ni in seq_along(namelist)) {
      if (setequal(fv,namelist[[ni]])) {
        res = valuevec[ni]
        break
      } else {
        res = 0
      }
    }
    return(res)
  }

  calcul_shap = \(xvar){
    fullxvar = xname[-which(xname == xvar)]
    fullvar = sdsfun::generate_subsets(fullxvar,empty = FALSE,self = TRUE)

    calcul_unishap = \(xvar,fullxvar,namelist,valuevec){
      n = length(fullxvar)
      v1 = get_value_by_varname(c(xvar,fullxvar),namelist,valuevec)
      v2 = get_value_by_varname(fullxvar,namelist,valuevec)
      thetax = factorial(n) * factorial(m - n - 1) * (v1 - v2) / mf
      return(thetax)
    }

    thetaxs = purrr::map_dbl(fullvar, \(.x) calcul_unishap(xvar,.x,xs,out_pdv))
    thetax = sum(thetaxs)
    names(thetax) = 'spd'
    return(thetax)
  }

  if (doclust) {
    parallel::clusterExport(cores)
    out_g = parallel::parLapply(cores,xname,calcul_shap)
    out_g = tibble::as_tibble(do.call(rbind, out_g))
  } else {
    out_g = purrr::map_dfr(xname,calcul_shap)
  }
  out_spd = dplyr::pull(out_g,1)

  IntersectionSymbol = rawToChar(as.raw(c(0x20, 0xE2, 0x88, 0xA9, 0x20)))
  xsname = purrr::map_chr(xs,\(.x) paste(.x,collapse = IntersectionSymbol))
  interactvar = xs[[which.max(out_pdv)]]
  res_pd = tibble::tibble(variable = xsname,
                          pd = out_pdv) |>
    dplyr::arrange(dplyr::desc(pd))
  res_spd = tibble::tibble(variable = xname,
                           spd = out_spd) |>
    dplyr::arrange(dplyr::desc(spd))

  step_interaction = sapply(xs, length)
  max_pd_names = sapply(unique(step_interaction), function(.s) {
    step_pd = out_pdv[which(step_interaction == .s)]
    step_indice = which.max(step_pd)
    return(xs[which(step_interaction == .s)][step_indice])
  })
  new_pd_indice = vector("logical",length(xs))
  new_xsname = vector("character",length(xs))
  for (i in seq_along(xs)) {
    if (step_interaction[i] == 1){
      new_pd_indice[i] = TRUE
      new_xsname[i] = xs[[i]]
    } else {
      current_name = max_pd_names[step_interaction[i] - 1]
      if (all(unlist(current_name) %in% unlist(xs[i]))) {
        new_pd_indice[i] = TRUE
        new_xsname[i] = setdiff(xs[[i]], current_name[[1]])
      } else {
        new_pd_indice[i] = FALSE
      }
    }
  }
  determination = tibble::tibble(variable = xsname[new_pd_indice],
                                 pd = out_pdv[new_pd_indice],
                                 step = step_interaction[new_pd_indice],
                                 name = new_xsname[nchar(new_xsname) > 0]) |>
    dplyr::group_by(step) |>
    dplyr::arrange(pd,.by_group=TRUE) |>
    dplyr::ungroup()

  res = list("spd" = res_spd, "pd" = res_pd,
             "determination" = determination)
  class(res) = "sshmc_result"
  return(res)
}

#' @title print ssh_marginalcontri result
#' @export
#' @noRd
#'
print.sshmc_result = \(x, ...) {
  cat("***   Marginal Contributions From SSH     ")
  print(knitr::kable(x$spd, format = "markdown", digits = 12, align = 'c', ...))
}
