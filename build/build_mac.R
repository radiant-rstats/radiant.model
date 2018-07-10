## build for mac
app <- "radiant.model"
path <- "~/GitHub"
devtools::install(file.path(path, app))
f <- devtools::build(file.path(path, app))
curr <- getwd()
setwd(path)
system(paste0("R CMD INSTALL --build ", f))
setwd(curr)
