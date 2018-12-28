expect_doppelganger_if_not_cran <- function(title, fig, path = NULL, ..., verbose = NULL,
                                            writer = vdiffr::write_svg) {
  # skip on cran or vdiffr is old
  if (!identical(Sys.getenv("NOT_CRAN"), "true") &&
      utils::packageVersion("vdiffr") < "0.2.3.9001") {
    return(invisible(NULL))
  }

  vdiffr::expect_doppelganger(title = title,
                              fig = fig,
                              path = path,
                              ...,
                              verbose = verbose,
                              writer = writer)
}

