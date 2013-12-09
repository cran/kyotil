#ifndef KERNEL_h
#define KERNEL_h
typedef enum {LINEAR = 0,EUCLIDEAN = 1,POLYNOMIAL = 2,RBF = 3,IBS = 4,HAMMING=5}KERNEL_TYPE;
#endif 

void ibsX_kernel(int* _nrx, int* _ncx,double* x,double* weights,int* _order,double* K);
void ibsXY_kernel(int* _nrx, int* _ncx,double* x,int* _nry, int* _ncy,double* y,double* weights,int* _order,double* K);

void edist2(int* _nr1,int* nc1,double* x1,int* _nr2,int* nc2,double* x2,double* dist);
void getKernel(int* _nr1,int* nc1,double* x1,int* _nr2,int* nc2,double* x2,int* kernel,double* para,double* K);
