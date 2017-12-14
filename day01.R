roll <- function(x, n) {
  c(tail(x, n), head(x, -n))
}

inverse_captcha <- function(input, shift) {
  # Extract the individual digits from the numeric string
  digits <- as.numeric(strsplit(input, "")[[1]])
  
  # Find the indices of matching digits
  matches <- digits == roll(digits, shift)
  
  # Calculate the sum of all the matching digits
  sum(digits[matches])
}

day01a <- function(input) {
  inverse_captcha(input, 1)
}

day01b <- function(input) {
  inverse_captcha(input, nchar(input) / 2)
}