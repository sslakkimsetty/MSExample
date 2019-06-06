
#' Mass peak list objects
#'
#' \code{MassPeakList} is a simple S4 class designed to
#' contain a list of \code{MassPeaks} objects.
#'
#' @name MassPeaksList-class
#' @rdname MassPeaksList-class
#'
#' @export
setClass("MassPeaksList",
    contains = "SimpleList")

setAs("ANY", "MassPeaksList",
    function(from) {
        to <- as(from, "SimpleList")
        to <- new("MassPeaksList", to,
            elementType="MassPeak")
        if ( validObject(to) )
            to
    })

setAs("MassSpectraList", "MassPeaksList",
    function(from) {
        to <- lapply(from, function(x) as(x, "MassPeak"))
        to <- as(to, "SimpleList")
        to <- new("MassPeaksList", to,
            elementType="MassPeak")

        if( validObject(to) )
            to
    })
