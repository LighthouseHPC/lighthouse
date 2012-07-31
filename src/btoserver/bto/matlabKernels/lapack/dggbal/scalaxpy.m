scalaxpy
in
  beta : scalar, coef : scalar, b : vector
inout
  a : vector, y : vector
{
  y = beta*a
  a = coef*b + y
}
