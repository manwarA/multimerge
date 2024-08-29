#
# These are necessary functions that I need to search every time, when I
# encounter similar situation
#

#' warning as they appear, otherwise, R throw all the warnings at the end
# options(warn = 1)

#' This package merges multiple dfs into one with a small number of arguments.
#' Apart from the arguments needed in this package, the default values from
#' base::merge are used.
#' The errors/warnings reported from this package are limited, as I have dependent
#' upon the errors/warnings from the base::merge.
#'
#' "byCol" argument can accept the column number used for merging, however, it is
#' not an intended functionality.
#'
#' The merge "type" argument can match the partial merge-type, which is not recommended,
#' although, there are only four types available, the partial match functionality is
#' solely for typos.


#' @param infile The list of data frames to be merged
#' @param byCol A string of the name of the column use to merge the data frames, currently it supports single column to be specified
#' @param type The type of merge as string, either one of intersect, left, right, union.
#' @return A data frame of merged data frames in the list.

#' @export
multi.merge <- function(DFlist,
                        byCol,
                        type = "union") {

    # DFlist should be a list, and this function will return the merged of all the dfs in list

    stopifnot("The dataframes should be in a list." = is.list(DFlist))

    # type can be either union (all = TRUE), intersection (all = FALSE), left (all.x =TRUE) or right (all.y =TRUE)
    # type can be one of these: "intersect", "left", "right", "union"

    types = c("intersect", "left", "right", "union")

    # There is no function that is opposite to "%in%", but you can conveniently negate the original to get the desired output
    `%notin%` = Negate(`%in%`)

    # convert the type argument into lower case
    type = tolower(trimws(type))
    print(paste0("Given merge type is: ", type))

    #check whether the options are valid
    if (tolower(type) %notin% types) {
        warning("\nMerge type is ambiguous; using 'grep' to match the closest
                possible value; please be careful\n")

        type = grep(tolower(type), types, value = TRUE)
        print(paste0("The possible type of merge is: ", type))
        #print(length(type))
    } #else {

    if (length(type) == 0 ) {
        stop("Type is not understandable, please provide the correct merge type")
    }


    if ( tolower(type) == "intersect") {
        df = base::Reduce(function(x,y) base::merge(x, y, all = FALSE, by = byCol, sort = FALSE, allow.cartesian=TRUE ), DFlist)
    } else if ( tolower(type) == "left") {
        df = base::Reduce(function(x,y) base::merge(x, y, all.x = TRUE, by = byCol, sort = FALSE, allow.cartesian=TRUE ), DFlist)
    } else if ( tolower(type) == "right") {
        df = base::Reduce(function(x,y) base::merge(x, y, all.y = TRUE, by = byCol, sort = FALSE, allow.cartesian=TRUE ), DFlist)
    } else {
        df = base::Reduce(function(x,y) base::merge(x, y, all   = TRUE, by = byCol, sort = FALSE, allow.cartesian=TRUE ), DFlist)
        }

    # The return df; the 'return' does not work, this is a workaround
    df
    }

