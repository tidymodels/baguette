# nocov start

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
  # This defines model functions in the parsnip model database
  make_bag_tree()
  make_bag_mars()
  make_bag_mlp()
}

# nocov end
