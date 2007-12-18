# --------------------------------------------------------------------
# compare.R
# --------------------------------------------------------------------

# --------------------------------------------------------------------
#
# compare()
#
# compare will take two(?) image files of the same format and identify
# any differences in the files.
# compare has the option of deleting the test file and the model file
# once the comparison has been made, and then to retain files if they
# are different.
#
# --------------------------------------------------------------------
"compare" <- function(test,
                      control,
                      clean = c("full", "files", "none"), # char length 1
                                    # none = don't delete anything
                                    # full = delete everything
                                    # files = delete files (but NOT index)
                                    # AFTER EACH expr
                      ) {

    if (is.character(test)) {
        if ("it's one or more log files") {
        } else {
            # It's a PATH (not a file) !
            # so autodetect log files
        }
        
    } else if (is.qcresult(test)) {
        
    } else {
        stop()
    }

    # DITTO for control
    
    # Compare one plotExpr at a time (using natural order of qcresult
    # OR order specified explicitly by log files OR the natural
    # order of the log files from the autodetect).
}
