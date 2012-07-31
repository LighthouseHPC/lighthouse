scal2nrm
in
  a : scalar
inout
  x : vector
out
  nrm : scalar
{
  x = a*x
  nrm = x'*x
}
