gemvger
in
  alpha : scalar, v : vector
inout
  w : vector, C : column matrix
{
  w = C*v
  C =  C + alpha*v*w'
}
