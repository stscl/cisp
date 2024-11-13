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
  if (is.null(discvar)) {
    xdiscname = xname
    xundiscname = NULL
  } else {
    xdiscname = discvar
    xundiscname = xname[-which(xname %in% discvar)]
  }
  discdf = dplyr::select(data,dplyr::all_of(c(yname,xdiscname)))
  rgd_res = gdverse::rgd(paste0(yname," ~ ."), data = discdf,
                         discnum = discnum, minsize = minsize, cores = cores)
  qs = rgd_res[[1]]
  dti = rgd_res[[2]]
  qs$variable = factor(qs$variable,levels = xdiscname)
  qs = dplyr::rename(qs,qvalue = `Q-statistic`)

  if (strategy == 1L) {
    opt_discnum = dplyr::group_split(qs,variable) |>
      purrr::map_dbl(\(.df) .df$discnum[which.max(.df$qvalue)])
  } else {
    suppressWarnings({opt_discnum = dplyr::group_split(qs,variable) |>
      purrr::map_dbl(\(.df) sdsfun::loess_optnum(.df$qvalue, .df$discnum,
                                                 increase_rate = increase_rate)[1])})
  }
  res_discdf = purrr::map_dfc(seq_along(opt_discnum),
                              \(.n) {dn = which(dti$discnum == opt_discnum[.n])
                              return(dti[dn,.n])})

  if (!is.null(xundiscname)){
    dti = data %>%
      dplyr::select(dplyr::any_of(c(yname,xundiscname))) %>%
      dplyr::bind_cols(res_discdf)
  } else {
    dti = data %>%
      dplyr::select(dplyr::any_of(yname)) %>%
      dplyr::bind_cols(res_discdf)
  }

  xname = colnames(dti)[-which(colnames(dti) == yname)]
  xs = generate_subsets(xname,empty = FALSE, self = TRUE)
  spfom = overlay

  rpd_mc = \(formula, discdata, overlaymethod = 'and'){
    formula = stats::as.formula(formula)
    formula.vars = all.vars(formula)
    if (formula.vars[2] != "."){
      discdata = dplyr::select(discdata,dplyr::all_of(formula.vars))
    }
    yname = formula.vars[1]
    if (overlaymethod == 'intersection'){
      fuzzyzone = discdata %>%
        dplyr::select(-dplyr::any_of(yname)) %>%
        purrr::reduce(paste,sep = '_')
    } else {
      fuzzyzone = sdsfun::fuzzyoverlay(formula,discdata,overlaymethod)
    }
    qtheta = sdsfun::geodetector_q(discdata[,yname,drop = TRUE],fuzzyzone)
    return(qtheta)
  }

  calcul_rpd = \(.x){
    qv = rpd_isp(paste(yname,'~',paste0(.x,collapse = '+')),dti,spfom)
    return(qv)
  }

  doclust = FALSE
  if (cores > 1) {
    doclust = TRUE
    cores = parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cores), add=TRUE)
  }

  if (doclust) {
    parallel::clusterExport(cores,c('factor_detector'))
    out_rpd = parallel::parLapply(cores, xs, calcul_rpd)
    out_rpd = tibble::as_tibble(do.call(rbind, out_rpd))
  } else {
    out_rpd = purrr::map_dfr(xs, calcul_rpd)
  }
  out_rpdv = dplyr::pull(out_rpd,1)
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
    fullvar = generate_subsets(fullxvar,empty = FALSE,self = TRUE)

    calcul_unishap = \(xvar,fullxvar,namelist,valuevec){
      n = length(fullxvar)
      v1 = get_value_by_varname(c(xvar,fullxvar),namelist,valuevec)
      v2 = get_value_by_varname(fullxvar,namelist,valuevec)
      thetax = factorial(n) * factorial(m - n - 1) * (v1 - v2) / mf
      return(thetax)
    }

    thetaxs = purrr::map_dbl(fullvar, \(.x) calcul_unishap(xvar,.x,xs,out_rpdv))
    thetax = sum(thetaxs)
    names(thetax) = 'SPD'
    return(thetax)
  }

  if (doclust) {
    parallel::clusterExport(cores,c('generate_subsets'))
    out_g = parallel::parLapply(cores,xname,calcul_shap)
    out_g = tibble::as_tibble(do.call(rbind, out_g))
  } else {
    out_g = purrr::map_dfr(xname,calcul_shap)
  }
  out_spd = dplyr::pull(out_g,1)

  IntersectionSymbol = rawToChar(as.raw(c(0x20, 0xE2, 0x88, 0xA9, 0x20)))
  xsname = purrr::map_chr(xs,\(.x) paste(.x,collapse = IntersectionSymbol))
  interactvar = xs[[which.max(out_rpdv)]]
  if (overlay == 'intersection'){
    reszone = dti %>%
      dplyr::select(dplyr::all_of(interactvar)) %>%
      purrr::reduce(paste,sep = '_')
  } else {
    reszone = sdsfun::fuzzyoverlay(paste(yname,'~',paste0(interactvar,collapse = '+')),
                                   dti, overlay)
  }
  zonenum = as.numeric(table(reszone))
  percentzone = length(which(zonenum==1)) / length(reszone)
  risk1 = risk_detector(dti[,yname,drop = TRUE],reszone,alpha)
  res_rpd = tibble::tibble(variable = xsname) %>%
    dplyr::bind_cols(out_rpd) %>%
    dplyr::arrange(dplyr::desc(rpd))
  res_spd = tibble::tibble(variable = xname,
                           spd = out_spd) %>%
    dplyr::arrange(dplyr::desc(spd))

  step_interaction = sapply(xs, length)
  max_rpd_names = sapply(unique(step_interaction), function(.s) {
    step_rpd = out_rpdv[which(step_interaction == .s)]
    step_indice = which.max(step_rpd)
    return(xs[which(step_interaction == .s)][step_indice])
  })
  new_rpd_indice = vector("logical",length(xs))
  new_xsname = vector("character",length(xs))
  for (i in seq_along(xs)) {
    if (step_interaction[i] == 1){
      new_rpd_indice[i] = TRUE
      new_xsname[i] = xs[[i]]
    } else {
      current_name = max_rpd_names[step_interaction[i] - 1]
      if (all(unlist(current_name) %in% unlist(xs[i]))) {
        new_rpd_indice[i] = TRUE
        new_xsname[i] = setdiff(xs[[i]], current_name[[1]])
      } else {
        new_rpd_indice[i] = FALSE
      }
    }
  }
  determination = tibble::tibble(variable = xsname[new_rpd_indice],
                                 rpd = out_rpdv[new_rpd_indice],
                                 step = step_interaction[new_rpd_indice],
                                 name = new_xsname[nchar(new_xsname) > 0]) %>%
    dplyr::group_by(step) %>%
    dplyr::arrange(rpd,.by_group=TRUE) %>%
    dplyr::ungroup()

  res = list("rpd" = res_rpd, "spd" = res_spd, "optdisc" = res_discdf,
             "risk" = risk1, "determination" = determination,
             "number_individual_explanatory_variables" = length(interactvar),
             "number_overlay_zones" = length(zonenum),
             "percentage_finely_divided_zones" =  percentzone)
  class(res) = "cdsp_result"
  return(res)
}
