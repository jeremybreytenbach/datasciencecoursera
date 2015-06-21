myfunction <- function()
{
  x <- rnorm(100)
  mean(x)
}

secondfunction <- function(x)
{
  x + rnorm(length(x))
}