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
  res = purrr::map(xsname,calcul_spcv,overlay = overlay,discnum = discnum,
                   minsize = minsize, strategy = strategy,
                   increase_rate = increase_rate, cores = cores)
  return(res)
}
