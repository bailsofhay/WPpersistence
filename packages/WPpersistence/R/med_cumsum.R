#' @name med_cumsum
#' @title med_cumsum
#' @export
#' @description
#' This function computes the median cumulative sum of a binary set of numbers using Run Length Encoding (RLE). This function was created to compute the median persistence of water primrose invasion in the Sacramento-San Joaquin River Delta.
#'
#' @param dat a dataframe where each column represents a year and each row represents a pixel or observation. Values must be binary, with 1 representing presence and 0 representing absence.No NA values should exist in input dataframe.
#' @return A numeric vector containing the median cumulative sum RLE value across years (input columns) for each row of the input dataframe.
#'
#' @examples
#' # example code
#' \dontrun{
#' x = data.frame(x2014 = 1, x2015 = 0, x2016 = 1, x2017 = 1, x2018 = 1, x2019 = 1, x2020 = 0)
#' med_cumsum(dat = x)
#' }
#'
#' @author Bailey D. Morrison
#'
med_cumsum = function(dat){
  if (any(is.na(dat))){
    stop("Input dataframe contains NA's")
  } else {
    # Compute the lengths and values of runs of equal values in a vector
    rle_dat = rle(dat)

    # A vector of the same length as lengths with the corresponding values
    index = which(rle_dat$values == 1)

    # An integer vector containing the length of each run
    lengths = rle_dat$lengths[index]
    lengths[lengths == 1] = NA # lengths of 1 indicate presence of 1 year, not persistence, which need to be 2 or more consecutive years

    # Calculate the median value of all RLE lengths (median cumulative persistence)
    med_lengths = median(lengths, na.rm = T)
    return(med_lengths)
  }
}