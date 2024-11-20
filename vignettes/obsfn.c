#include <R.h>
 #include <math.h>
 void obsfn_a02fv7wz ( double * x, double * y, double * p, int * n, int * k, int * l ) {
 for(int i = 0; i< *n; i++) {
 y[0+i**l] = p[0]*(x[0+i**k]+x[1+i**k]+2.0*x[2+i**k]) ;
y[1+i**l] = p[1]*(x[1+i**k]+2.0*x[2+i**k])+p[2] ;
y[2+i**l] = p[3]*p[4]*pow(((1.0-exp(-x[5+i**k]*p[5]))*exp(-x[5+i**k]*p[6])),3.0) ; 
}
}