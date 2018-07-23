#' Title
#'
#' @param start
#' @param end
#' @param width
#' @param strand
#' @param base_y
#' @param coord
#' @param pct
#' @param hh
#'
#' @return
#' @export
#'
#' @examples
arrow_points <- function(start,end, width, strand, base_y, coord = c("x","y"), pct = 0.2, hh = 0.4) {
    if(strand == "+") {
        if(coord == "x") return(c(start,start, end-(pct*width), end, end-(pct*width), start))
        if(coord == "y") return(c(base_y-hh, base_y+hh, base_y+hh, base_y, base_y-hh, base_y-hh))
    }

    if(strand == "-") {
        if(coord == "x") return(c(end,end,start+(pct*width), start,start+(pct*width), end))
        if(coord == "y") return(c(base_y-hh, base_y+hh, base_y+hh, base_y, base_y-hh, base_y-hh))
    }
}
