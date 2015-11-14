#include <float.h> //DBL_EPSILON
#include <R_ext/Lapack.h>
#include <R.h>
#include <Rinternals.h>
#include "matrix.h"
//#include <R_ext/Applic.h>
//#include <R_ext/BLAS.h>
//#include <R_ext/RS.h> //R definitions for 'extending' R, registering functions,...

#define PRINTF Rprintf
#define MAX(A,B)    ((A) > (B) ? (A) : (B))
#define MIN(A,B)    ((A) < (B) ? (A) : (B))



SEXP outer_3_mean_all(SEXP _X, SEXP _Y, SEXP _Z){
    
    int m=length(_X);
    int n=length(_Z);
    double *X=REAL(_X), *Y=REAL(_Y), *Z=REAL(_Z);
    
    SEXP _ans=PROTECT(allocVector(REALSXP, 9));
    double *ans=REAL(_ans);
    //PRINTF("%f %f \n",dil_r,sigma_sq);
    //PRINTF("%i %i \n",m,n);
    
    // R code
//    tmp   =outer.3.mean(1:m,1:m,1:n, function(i,j,k) ifelse(i==j,NA,(X[i]>Y[j])*(Z[k]>Y[j])) )
//    tmp_1 =outer.3.mean(1:m,1:m,1:m, function(i,j,k) ifelse(i==j|i==k|j==k,NA,(X[i]>Y[j])*(X[i]>Y[k])) )
//    tmp_11=outer.3.mean(1:m,1:m,1:m, function(i,j,k) ifelse(i==j|i==k|j==k,NA,(X[i]>Y[j])*(X[k]>Y[i])) )
//    tmp_2= outer.3.mean(1:m,1:m,1:m, function(i,j,k) ifelse(i==j|i==k|j==k,NA,(X[i]>Y[j])*(X[k]>Y[j])) )
    int tmp=0, tmp_1=0, tmp_11=0, tmp_2=0, xgry=0, tmp_4=0;
    int xy;
    int i, j, k, t, kp, jp;    
    for(i = 0;i < m;i++){
    for(j = 0;j < m;j++){
          if(i==j) continue;
          xy=X[i]>Y[j];
          xgry += xy;
          for(k = 0;k < n;k++){
              tmp  +=xy*(Z[k]>Y[j]);
              tmp_4+=xy*(Z[k]>Y[i]);
          }
          for(t = 0;t < m;t++){
              if (t==i || t==j) continue;
              tmp_1 +=xy*(X[i]>Y[t]);
              tmp_11+=xy*(X[t]>Y[i]);
              tmp_2 +=xy*(X[t]>Y[j]);
          }
    }}    
    ans[0]=1.0*tmp   /(m*(m-1)*n);
    ans[1]=1.0*tmp_1 /(m*(m-1)*(m-2));
    ans[2]=1.0*tmp_11/(m*(m-1)*(m-2));
    ans[3]=1.0*tmp_2 /(m*(m-1)*(m-2));
    ans[4]=1.0*xgry/(m*(m-1));
    ans[8]=1.0*tmp_4 /(m*(m-1)*n);
        
    int zgry=0, tmp_3=0, tmp_31=0;
    int zy;
    for(j = 0;j < m;j++){
          for(k = 0;k < n;k++){
              zy=Z[k]>Y[j];
              zgry+=zy;
              for(kp = 0;kp < n;kp++){
                  if (kp==k) continue;   
                  tmp_3 += zy*(Z[kp]>Y[j]);
              }
              for(jp = 0;jp < m;jp++){
                  if (jp==j) continue;   
                  tmp_31+= zy*(Z[k]>Y[jp]);
              }
          }
    } 
    ans[5]=1.0*zgry/(m*n);
    ans[6]=1.0*tmp_3 /(m*n*(n-1));
    ans[7]=1.0*tmp_31/(n*m*(m-1));

    UNPROTECT(1);
    return _ans;
}

