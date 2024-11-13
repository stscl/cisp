spc = \(data){
  if (inherits(data,'sf')) {
    data = sf::st_drop_geometry(data)
  }
}
