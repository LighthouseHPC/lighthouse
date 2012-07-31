ddot2sum
in
  a : vector, b : vector
out 
  gamma : scalar
inout
  ew : scalar, ewc: scalar
{
  gamma = a'*a + b'*b
  ew = sum(a)
  ewc = sum(b)
}
