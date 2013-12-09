#include "kernels.h"
#include "matrix.h"
#include <strings.h>
#include <math.h>
#include <float.h>

#include <R.h>

#define	ABS(A)  	((A) > 0 ? (A) : (-(A)))
#define	MIN(A,B)	((A) < (B) ? (A) : (B))
#define	MAX(A,B)	((A) > (B) ? (A) : (B))
#define EQ(x,y,eps)  ((ABS((x)-(y))) <= (eps) ? 1 : 0) 
#define CX(i,j,ld) (((j)*(ld))+(i)) //column-major index
#define RX(i,j,ld) (((i)*(ld))+(j)) //row-major index
#define I(x,y) ((x) == (y))

// IBS(x,y)   = C - |x - y| where C = 1 for x,y in {0,1}, C = 2 for x,y in {0,1,2}, ..., C = k for x,y in {0,1,..,k}
// K_IBS(i,j) = sum(w * (C - |x[i]-y[i]|))
// Note: IBS1(x,y) = I(x,y)


// IBS kernel for data {0,1,2,..,order}, 1 input matrix x
// x_{nr,nc} in column-major order, type double in {0,1,2,..,order}
// w length nc, weights
// order: integer, 'order' of the kernel, max possible value in data {0..order}
// K_{nr,nr} accessed in column-major order since only
// upper-triangular region is computed and copied to lower-triangular region

void ibsX_kernel(
int* _nr, int* _nc,double* x,
double* weights,int* _order,double* K
){

	int i,j,k;
	int nr = *_nr;
	int nc = *_nc;
	double s;
	double denom;
	double sum_w;
	double* w = NULL;
	double order = 1.0;if(_order)order = (double)(*_order);
	order = MAX(1.0,order);	
	if(weights)
		if(weights[0] != -1.0)
			w = weights;
	
	if(!w){
		for(i = 0;i < nr;i++){
			for(j = i;j < nr;j++){		
				s = 0.0;
				// for(k = 0;k < nc;k++) s += IBS(x[CX(i,k,nr)],x[CX(j,k,nr)]);
				for(k = 0;k < nc;k++) s += ABS(x[CX(i,k,nr)] - x[CX(j,k,nr)]);
				K[CX(i,j,nr)] = s;
			}
		}
	}else{
		for(i = 0;i < nr;i++){
			for(j = i;j < nr;j++){		
				s = 0.0;
				// for(k = 0;k < nc;k++) s += w[k] * IBS(x[CX(i,k,nr)],x[CX(j,k,nr)]);
				for(k = 0;k < nc;k++) s += w[k] * ABS(x[CX(i,k,nr)] - x[CX(j,k,nr)]);
				K[CX(i,j,nr)] = s;
			}
		}	
	}
	
	if(!w){
		sum_w = (double)nc;		
		if(order == 1.0)
			denom = 1.0;
		else
			denom = 2.0 * (double)nc;
	}else{
		sum_w = 0.0;
		for(k = 0;k < nc;k++) sum_w += w[k];
		denom = sum_w;
	}
	
	double C = order * sum_w;
	for(i = 0;i < nr;i++){
		for(j = i;j < nr;j++){	
			K[CX(i,j,nr)] *= -1.0;
			K[CX(i,j,nr)] += C;
			K[CX(i,j,nr)] /= denom;
		}
	}
	
	for(i = 0;i < nr - 1;i++)
		for(j = i+1;j < nr;j++)
			K[CX(j,i,nr)] = K[CX(i,j,nr)];
}

// 2 input matrices x,y pf type double containing values in in {0,1,2,..,order}
// x_{nrx,ncx} in column-major order
// y_{nry,ncy} in column-major order
// w_{MIN(ncx,ncy)}, weights, only the first MIN(ncx,ncy) entries in 'w' are accessed
// order: integer, 'order' of the kernel, max possible value in data {0..order}
// K_{nrx,nry} accessed in column-major 
// if 'x' and 'x2' have differing column dimensions, the firts 1..min(ncol(x),ncol(x2)) are used
void ibsXY_kernel(
int* _nrx, int* _ncx,double* x,
int* _nry, int* _ncy,double* y,
double* weights,int* _order,double* K
){
	int i,j,k;
	int nrx = *_nrx;
	int nry = *_nry;
	int nc = MIN(*_ncx,*_ncy);
	double s;
	double denom;
	double sum_w;
	double* w = NULL;
	double order = 1.0;if(_order)order = (double)(*_order);
	order = MAX(1.0,order);
	if(weights)
		if(weights[0] != -1.0)
			w = weights;
			
	if(!w){
		for(i = 0;i < nrx;i++){
			for(j = 0;j < nry;j++){
				s = 0.0;
				for(k = 0;k < nc;k++) s += ABS(x[CX(i,k,nrx)] - y[CX(j,k,nry)]);
				K[CX(i,j,nrx)] = s;
			}
		}
	}else{
		for(i = 0;i < nrx;i++){
			for(j = 0;j < nry;j++){
				s = 0.0;
				for(k = 0;k < nc;k++) s += w[k] *  ABS(x[CX(i,k,nrx)] - y[CX(j,k,nry)]);
				K[CX(i,j,nrx)] = s;
			}
		}	
	}
	
	if(!w){
		sum_w = (double)nc;		
		if(order == 1.0)
			denom = 1.0;
		else
			denom = 2.0 * (double)nc;
	}else{
		sum_w = 0.0;for(k = 0;k < nc;k++) sum_w += w[k];
		denom = sum_w;
	}
	
	double C = order * sum_w;
	for(i = 0;i < nrx;i++){
		for(j = 0;j < nry;j++){	
			K[CX(i,j,nrx)] *= -1.0;
			K[CX(i,j,nrx)] += C;
			K[CX(i,j,nrx)] /= denom;
		}
	}	
}

// // Hamming *similarty* distance for binary data {0,1} (as opposed to hamming dissimilarity) 
// void hamming_sim(int* _nrx, int* _ncx,double* x,int* _nry,int* _ncy,double* y,double* weights,double* K){
	// int i,j,k;
	// int nrx = *_nrx;
	// int nry = *_nry;
	// int nc = MIN(*_ncx,*_ncy);
	// double* w = NULL;
	// if(weights)
		// if(weights[0] != -1.0)
			// w = weights;	
	// if(!w){
		// for(i = 0;i < nrx;i++){
			// for(j = 0;j < nry;j++){
				// int s = 0;
				// for(k = 0;k < nc;k++) s += I(x[CX(i,k,nrx)],y[CX(j,k,nry)]);
				// K[CX(i,j,nrx)] = (double)s;
			// }
		// }
	// }else{
		// double sum_w = 0.0;
		// for(k = 0;k < nc;k++) sum_w += w[k];
		// for(i = 0;i < nrx;i++){
			// for(j = 0;j < nry;j++){
				// double s = 0.0;
				// for(k = 0;k < nc;k++) s += w[k] * (double)(I( x[CX(i,k,nrx)],y[CX(j,k,nry)]));
				// K[CX(i,j,nrx)] = s / sum_w;
			// }
		// }	
	// }
// }

//(euclidean distance)^2
void edist2(int* _nr1,int* nc1,double* x1,int* _nr2,int* nc2,double* x2,double* dist){
	double s,ss;
	int i1,i2,j;
	int nc = MIN(*nc1,*nc2);
	int nr1 = *_nr1;
	int nr2 = *_nr2;
	memset(dist,0,(size_t)(nr1 * nr2 * sizeof(double)));

	// dist <- matrix(rowSums((X1[rep(1:nrow(X1),nrow(X2)),,drop=F] - X2[rep(1:nrow(X2),each=nrow(X1)),,drop=F])^2),nrow = nrow(X))
	for(i2 = 0;i2 < nr2;i2++){
		for(i1 = 0;i1 < nr1;i1++){
			ss = 0.0;
			for(j = 0;j < nc;j++){
				s = x1[CX(i1,j,nr1)] - x2[CX(i2,j,nr2)];
				s *=s;
				ss += s;
			}
			dist[CX(i1,i2,nr1)] = ss;		
		}
	}	
}

void getKernel(int* nr1,int* nc1,double* x1,int* nr2,int* nc2,double* x2,
			   int* _kernel,double* para,double* K){
	KERNEL_TYPE kernel = (KERNEL_TYPE)*_kernel;
	if(kernel == LINEAR){
		tcrossprod(x1,nr1,nc1,x2,nr2,nc2,K);	
		return;
	}else if(kernel == EUCLIDEAN){
		edist2(nr1,nc1,x1,nr2,nc2,x2,K);
		return;
	}else if(kernel == POLYNOMIAL){
		tcrossprod(x1,nr1,nc1,x2,nr2,nc2,K);	
		for(int i = 0;i < (*nr1) * (*nr2);i++) 
			K[i] = pow(K[i] + 1.0,para[0]);
		return;
	}else if(kernel == RBF){
		edist2(nr1,nc1,x1,nr2,nc2,x2,K);
		for(int i = 0;i < (*nr1) * (*nr2);i++) 
			K[i] = exp(-para[0] * K[i]);
		for(int i = 0;i < (*nr1) * (*nr2);i++)
			if(EQ(K[i],0.0,DBL_EPSILON))
				K[i] = 0.0;			
		return;		
	}else if(kernel == IBS){
		int order = 2;
		if((*nr2 == 0) || (*nc2 == 0) || (!x2)){
			ibsX_kernel(nr1,nc1,x1,para,&order,K);	
			return;		
		}
		ibsXY_kernel(nr1,nc1,x1,nr2,nc2,x2,para,&order,K);	
		return;
	}else if(kernel == HAMMING){
		int order = 1;
		if((*nr2 == 0) || (*nc2 == 0) || (!x2)){
			ibsX_kernel(nr1,nc1,x1,para,&order,K);	
			return;		
		}
		ibsXY_kernel(nr1,nc1,x1,nr2,nc2,x2,para,&order,K);	
		return;
	}
}


// for(i2 in 1:nrow(X2))
	// for(i1 in 1:nrow(X))
		// cat(i1," ",i2,"\n")

//rowSums((X[rep(1:nrow(X),nrow(X)),,drop=F] - X[rep(1:nrow(X),each=nrow(X)),,drop=F])^2)
// getK=function(X,kernel,para=NULL,X2=NULL){
    // kernel=substr(kernel,1,1)
    // if (kernel=="r" | kernel=="e") {
        // if (!is.null(X2)) {
            // aux = X[rep(1:nrow(X),nrow(X2)),,drop=F] - X2[rep(1:nrow(X2),each=nrow(X)),,drop=F]
            // dist.mat = matrix(rowSums(aux^2), nrow=nrow(X))
// #            aux=X2[rep(1:nrow(X2),nrow(X)),] - X[rep(1:nrow(X),each=nrow(X2)),]
// #            dist.mat = matrix(rowSums(aux^2), nrow=nrow(X2))
        // } else {
            // dist.mat = as.matrix(dist(X))^2
        // }
    // }
    
    // if (is.null(X2)) X2=X
    // switch(kernel, 
        // p=(tcrossprod(X,X2)+1)^para, # polynomial
        // r=exp(-para*dist.mat), # rbf
        // e=dist.mat, # Euclidean edist
        // l=tcrossprod(X,X2), # linear
        // i = ibs(X,X2), # IBS 
		// stop(kernel %+% " kernel not supported")
    // )
// }


