#' Gene arrow points
#'
#' Calculates the coordinates for drawing a gene arrow, given a set of start/end coordinates
#' and a midline y value.
#'
#' @param start Start coordinate of gene
#' @param end End coordinate of gene
#' @param width Width of gene
#' @param strand Strand of gene (does arrow point left or right)
#' @param base_y y value to center gene around (usually a stepping value from \code{\link[biovizBase]{addStepping}})
#' @param coord Either \code{x} or \code{y} determining which set of coordinates to return
#' @param pct Percentage of gene width to be taken up by the arrow
#' @param hh Half height of gene. Gene arrow will be drawn at \code{base_y +/- hh}
#'
#' @return Vector of coordinates used to plot a gene arrow
#'
#' @examples
arrow_points <- function(start, end, width, strand, base_y, coord = c("x","y"), pct = 0.2, hh = 0.4) {
    if(as.character(strand) == "+") {
        if(coord == "x") return(c(start,start, end-(pct*width), end, end-(pct*width), start))
        if(coord == "y") return(c(base_y-hh, base_y+hh, base_y+hh, base_y, base_y-hh, base_y-hh))
    }

    if(as.character(strand) == "-") {
        if(coord == "x") return(c(end,end,start+(pct*width), start,start+(pct*width), end))
        if(coord == "y") return(c(base_y-hh, base_y+hh, base_y+hh, base_y, base_y-hh, base_y-hh))
    }
}
