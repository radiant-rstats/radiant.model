## ui for model menu in radiant
navbar_proj(
  suppressWarnings(
    do.call(
      navbarPage,
      c(
        "Radiant for R",
        getOption("radiant.nav_ui"),
        getOption("radiant.model_ui"),
        getOption("radiant.shared_ui"),
        help_menu("help_model_ui")
      )
    )
  )
)
