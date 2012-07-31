#include <stdlib.h>
void DDOT(int x_ncols, double* x, int y_nrows, double* y, double* a_ptr){
double a = *a_ptr;
int ii,i;
double t2 = a;
t2 = 0.0;
for (i = 0;i < x_ncols; i+=1) {
// 4
t2 += (x[i]*y[i]);
}
a = t2;
*a_ptr = a;
}
