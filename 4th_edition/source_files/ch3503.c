#include <stdio.h>
float reciprocal(float *x);

int main()
{
  float x;
  x=10.0f;
  printf(" C calling a Fortran function\n");
  printf(" (1 / %f ) = %f \n" ,x,reciprocal(&x));
  return(0);
}
