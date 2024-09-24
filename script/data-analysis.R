data_hp |>
  rename(mukim_old = mukim) |>
  left_join(
    kpg_sf |>
      sf::st_set_geometry(NULL) |>
      select(kampong, mukim)
    