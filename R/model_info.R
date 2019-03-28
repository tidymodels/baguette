models <- c("CART", "model_rules", "C5.0", "MARS")
# TODO add two-class cost models
# TODO Something different than "model_rules"? Cubist isn't a great idea since
#      we don't allow neighbors or boosting.

# We want to default some arguments for different models
model_defaults <-
  list(
    CART = list(cp = 0, xval = 0),
    "model_rules" = list(),
    "C5.0" = list(trials = 1),
    MARS = list(pmethod = "none", nprune = NULL, degree = 2)
  )

# Enumerate the possible arguments in the fit or control functions that can
# be modified by the user. This could be done programatically to protect against
# changes but each of the underlying pacakges is pretty mature and there is a
# small likelihood of them changing.

model_args <-
  list(
    CART = c('method', 'parms', 'cost', 'minsplit', 'minbucket', 'cp',
             'maxcompete', 'maxsurrogate', 'usesurrogate', 'xval',
             'surrogatestyle', 'maxdepth'),
    "model_rules" = c('unbiased', 'rules', 'extrapolation', 'sample'),
    "C5.0" = c('rules', 'costs', 'subset', 'bands', 'winnow', 'noGlobalPruning',
               'CF', 'minCases', 'fuzzyThreshold', 'sample'),
    MARS = c('pmethod', 'trace', 'glm', 'degree', 'nprune', 'nfold', 'ncross',
             'stratify', 'varmod.method', 'varmod.exponent', 'varmod.conv',
             'varmod.clamp', 'varmod.minspan', 'Scale.y')
  )
