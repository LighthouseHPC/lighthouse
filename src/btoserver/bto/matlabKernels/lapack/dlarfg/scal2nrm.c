#include <stdlib.h>
void scal2nrm(double a, double* nrm_ptr, double* x, int x_nrows) {
double nrm = *nrm_ptr;
int ii,i;
double t4 = nrm;
t4 = 0.0;
for (i = 0;i < x_nrows; i+=1) {
x[i] = (a*x[i]);
t4 += (x[i]*x[i]);
}
nrm = t4;
*nrm_ptr = nrm;
}
