#include <stdio.h>
void reciprocal(int nr,int nc,
                float x[nr][nc],
                float y[nr][nc]);
int main()
{
  const int nr=2;
  const int nc=5;
  float x[nr][nc];
  float y[nr][nc];
  int r;
  int c;
  int i=1;
  for (r=0;r<nr;r++)
    for (c=0;c<nc;c++)
    {
      x[r][c]=(float)(i);
      i++;
    }
  printf(" C calling Fortran\n");
  printf(" 2 d array as parameter\n");
  printf(" C99 vla\n");
  for (r=0;r<nr;r++)
  {
    for (c=0;c<nc;c++)
    {
      printf(" %5.2f " , x[r][c]);
    }
    printf("\n");
  }
  reciprocal(nr,nc,x,y);
  for (r=0;r<nr;r++)
  {
    for (c=0;c<nc;c++)
    {
      printf(" 1 / %5.2f = %6.3f \n" 
			  , x[r][c],y[r][c]);
    }
    printf("\n");
  }
  return(0);
}
