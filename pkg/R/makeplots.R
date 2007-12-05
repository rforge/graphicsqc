# --------------------------------------------------------------------
# makeplots.R
# --------------------------------------------------------------------

# --------------------------------------------------------------------
#
# plotExpr()
#
# plotExpr will take an R expression (or expressions) and produce
# plots in specific file formats of the intended plot.
#
# ALWAYS produce a file containing index of files created
#
# MUST always work (=> tryCatch)
#
# Return value is record of files created
#
# --------------------------------------------------------------------
"plotExpr" <- function(expr, # character vector
                             # R expression(s)
                       format = NULL, # character vector
                                      # (valid) file formats
                       prefix = NULL, # char length 1
                                      # file prefix
                       path = NULL, # char length 1
                                    # directory
                       ) {
    tryCatch(eval(parse(text="plot(1:10)")))
}
