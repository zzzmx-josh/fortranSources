#include <iostream>
using namespace std;
extern "C" float summation(float *,int );
int main()
{
  const int n=10;
  float *x;
  int i;
  x = new  float[n];
  for (i=0;i<n;i++)
    x[i]=1.0f;
  cout << " C++ calling Fortran" << endl;
  cout << " 1 d array as parameter" << endl;
  cout << " Sum is " << summation(x,n) << endl;
  return(0);
}
