repmat <- function (a, n, m)
{
  kronecker(matrix(1, n, m), a)
}
