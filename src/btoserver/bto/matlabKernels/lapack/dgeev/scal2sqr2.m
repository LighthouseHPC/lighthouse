scal2sqr2
in
  scale : scalar
inout
  a : vector, b : vector, r : vector
{
  a = scale*a
  b = scale*b
  r = a*a + b*b
}
