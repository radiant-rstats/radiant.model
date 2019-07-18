## build for mac
app <- "radiant.model"
path <- "~/gh"
devtools::install(file.path(path, app), upgrade = "never")
f <- devtools::build(file.path(path, app))
curr <- getwd()
setwd(path)
system(paste0("R CMD INSTALL --build ", f))
setwd(curr)
