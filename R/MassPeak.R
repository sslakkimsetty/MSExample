
#' Mass peak objects
#'
#' \code{MassPeak} is a simple S4 class designed to
#' encapsulate basic information about a mass spectrum.
#' It is a special case of MassSpectrum objects.
#'
#' @param object A \code{MassPeak} object
#' @param value A replacement value
#' @param x A \code{MassPeak} object
#' @param i indices specifying elements to extract
#' @param j indices specifying elements to extract
#' @param drop ignored
#' @param ... Additional arguments
#'
#' @slot mz The m/z-values.
#' @slot intensity The intensities observed
#'                  at the associated m/z values.
#' @slot isCentroided Whether the spectrum has
#'                      been centroided or not.
#' @slot peaks The indices of any detected peaks
#'
#' @name MassPeak-class
#' @rdname MassPeak-class
#'
#' @export
setClass("MassPeak",
    contains = "MassSpectrum")

.valid_MassPeak <- function(object) {
    errors <- NULL
    if ( is.unsorted(object@mz) )
        errors <- c(errors , "mz must be sorted in increasing order")
    if ( length(object@mz) != length(object@intensity) ) {
        errors <- c(errors , paste0("length of mz [",
            length(object@mz), "] must match length of intensity [",
            length(object@intensity), "]"))
    }
    outofrange <- object@peaks < 1L | object@peaks > length(object@mz)
    if ( length(object@peaks) > 0L && any(outofrange) ) {
        errors <- c(errors , paste0("out-of-range peak indices [",
            object@peaks[outofrange]))
    }

    if ( length(object@peaks) < 1L )
        errors <- c(errors, "peaks not found")

    if ( is.null(errors) ) TRUE else errors
}

setValidity("MassPeak", .valid_MassPeak)

setAs("MassSpectrum", "MassPeak",
    function(from) {
        to <- new("MassPeak",
            mz = from@mz,
            intensity = from@intensity,
            isCentroided = from@isCentroided,
            peaks = from@peaks)
        if ( validObject(to) )
            to
    })
