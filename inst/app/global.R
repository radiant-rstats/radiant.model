## sourcing from radiant.data
options(radiant.path.data = system.file(package = "radiant.data"))
source(file.path(getOption("radiant.path.data"), "app/global.R"), encoding = getOption("radiant.encoding", default = "UTF-8"), local = TRUE)

ifelse (grepl("radiant.model", getwd()) && file.exists("../../inst") , "..", system.file(package = "radiant.model")) %>%
  options(radiant.path.model = .)

## setting path for figures in help files
addResourcePath("figures_model", "tools/help/figures/")

## setting path for www resources 
addResourcePath("www_model", file.path(getOption("radiant.path.model"), "app/www/"))

## loading urls and ui
source("init.R", encoding = getOption("radiant.encoding"), local = TRUE)
options(radiant.url.patterns = make_url_patterns())

## print options
# options(max.print = max(getOption("max.print"), 5000))
