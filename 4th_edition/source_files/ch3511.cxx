#include <iostream>
#include <iomanip>
using namespace std;
extern "C" void sums(int nr,int nc,
  int *x,int *rsum, int *csum);
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
    for (c=0;c<nc;c++)
    {
      x[r][c]=i;
      i++;
    }
  for (r=0;r<nr;r++)
    rsum[r]=0;
  for (c=0;c<nc;c++)
    csum[c]=0;
  cout << " C++ calling Fortran" << endl;
  cout << " 2 d array as parameter\n";
  cout << " Original 2 d array" << endl;
  cout << endl;
  for (r=0;r<nr;r++)
  {
    for (c=0;c<nc;c++)
    {
      cout << setw(3) << x[r][c] << " ";
    }
    cout << endl;
  }
  cout << endl;
  sums(nr,nc,(int*)x,rsum,csum);
  for (r=0;r<nr;r++)
  {
    for (c=0;c<nc;c++)
    {
      cout << setw(3) << x[r][c] << " ";
    }
    cout << " = " << rsum[r] << endl;
  }
  cout << endl;
  for (c=0;c<nc;c++)
    cout << setw(3) << csum[c] << " " ;
  cout << endl;
  return(0);
}
