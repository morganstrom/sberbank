# Sberbank housing data
# get_one_hot_encoder.R

# Function that generates a one_hot_encoder function for a character vector
# in the training set
get_one_hot_encoder <- function(vec) {
  
  # Get levels
  lev <- unique(vec)
  
  # Check if there are NA values
  if (any(is.na(lev))) {
    
    # Assuming that NA will be the first element always
    lev <- lev[-1]
    
    # Return ifelse statement function
    # that produces a matrix output
    # with one column per level and one row per observation
    function(vec_new, var_name) {
      out <- cbind(
        ifelse(is.na(vec_new), 1, 0),
        sapply(lev, function(i) ifelse(vec_new == i, 1, 0))
      )
      dimnames(out)[[2]] <- paste0(var_name, c("NA", lev))
      out
    }
  } else {
    
    # Return ifelse statement function
    # that produces a matrix output
    # with one column per level and one row per observation
    function(vec_new, var_name) {
      out <- sapply(lev, function(i) ifelse(vec_new == i, 1, 0))
      dimnames(out)[[2]] <- paste0(var_name, lev)
      out
    }
  }
}
