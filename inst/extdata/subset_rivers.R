rivers <- sf::st_read("~/Documents/Bridge/CoursEau_Carthage2017_CLIP.shp")

adour           <- river[which(river$"NomEntiteH" == "L'Adour"), ]
adour_lambert   <- sf::st_transform(adour, "epsg:2154")

garonne         <- river[which(river$"NomEntiteH" == "La Garonne"), ]
garonne_lambert <- sf::st_transform(garonne, "epsg:2154")

sf::st_write(adour_lambert, here::here("inst", "extdata", 
                                       "adour_lambert93.gpkg"))

sf::st_write(garonne_lambert, here::here("inst", "extdata", 
                                         "garonne_lambert93.gpkg"))
