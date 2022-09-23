#include <stdio.h>
void sums(int nr,int nc,int x[nr][nc], 
  int * rsum, int * csum);
int main()
{
  const int nr=2;
  const int nc=6;
  int x[nr][nc];
  int rsum[nr];
  int csum[nc];
  int r;
  int c;
  int i=1;
  for (r=0;r<nr;r++)
    rsum[r]=0;
  for (c=0;c<nc;c++)
    csum[c]=0;
  for (r=0;r<nr;r++)
    for (c=0;c<nc;c++)
    {
      x[r][c]=i;
      i++;
    }
  printf(" C calling Fortran\n");
  printf(" 2 d array as parameter\n");
  printf(" c99 vla\n");
  for (r=0;r<nr;r++)
  {
    for (c=0;c<nc;c++)
    {
      printf(" %3d " , x[r][c]);
    }
    printf("\n");
  }
  printf("\n");
  sums(nr,nc,x,rsum,csum);
  for (r=0;r<nr;r++)
  {
    for (c=0;c<nc;c++)
    {
      printf(" %3d " , x[r][c]);
    }
    printf(" %3d ",rsum[r]);
    printf("\n");
  }
  printf("\n");
  for (c=0;c<nc;c++)
    printf(" %3d ",csum[c]);
  printf("\n");
  return(0);
}
