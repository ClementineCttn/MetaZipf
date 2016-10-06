my_packages = c("ggplot2", "RColorBrewer", "plyr", "shiny", "leaflet",
                "rgdal", "rgeos", "leaflet", "data.table", "plm", "lme4")

###########################################################

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
  else {
    cat(paste("Skipping already installed package:", p, "\n"))
  }
}
invisible(sapply(my_packages, install_if_missing))