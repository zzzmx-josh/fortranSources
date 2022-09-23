#include <stdio.h>
float summation(float *x,int n);

int main()
{
  const int n=10;
  float x[n]; 
  int i;
  for (i=0;i<n;i++)
    x[i]=1.0;
  printf(" C calling Fortran\n");
  printf(" 1 d array as parameter\n");
  printf(" Sum is = %f \n " ,summation(x,n));
  return(0);
}
