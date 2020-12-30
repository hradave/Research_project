
################################ NORMALIZE FUNCTION ###################################

normalize <- function(x) {
  # normalize vector x similarly to Eq. 9 in Evaluation of Variance-based Nonconformity
  return ((x - min(x)) / (max(x) - min(x)))
}