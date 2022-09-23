#include <iostream>
using namespace std;
extern "C" { float reciprocal(float *); }
int main()
{
  float x;
  x=10.0f;
  cout << " C++ calling a Fortan function" << endl;
  cout << " x = " << x << " reciprocal = "; 
  cout << reciprocal(&x) << endl;
  return(0);
}
