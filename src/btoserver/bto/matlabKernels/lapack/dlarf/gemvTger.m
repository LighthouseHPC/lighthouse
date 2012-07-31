gemvTger
in
  a : scalar, v : vector
inout
  w : vector, C : column matrix
{
  w = C'*v
  C =  C + a*v*w'
}
