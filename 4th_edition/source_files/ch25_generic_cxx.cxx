template <class Type>
void swap(Type array[],int i, int j)
{ 
  Type tmp=array[i];
  array[i]=array[j];
  array[j]=tmp;
}

template <class Type>
void quicksort( Type array[], int l, int r)
{
  int i=l;
  int j=r;
  Type v=array[int((l+r)/2)];
  for (;;)
  {
    while (array[i] < v) i=i+1;
    while (v < array[j]) j=j-1;
    if (i<=j) 
      { swap(array,i,j); i=i+1 ; j=j-1; }
    if (i>j) goto ended ;
  }
  ended: ;
  if (l<j) quicksort(array,l,j);
  if (i<r) quicksort(array,i,r);
}

template <class Type>
void print(Type array[],int size)
{
  cout << " [ " ;
  for (int ix=0;ix<size; ++ix)
    cout << array[ix] << " ";
  cout << "] \n";
}

#include <iostream>
using namespace std;
int main()
{
  double da[] = 
  {1.9,8.2,3.7,6.4,5.5,1.8,9.2,3.6,7.4,5.5};
  int ia[] = {1,10,2,9,3,8,4,7,6,5};
  int size=sizeof(da)/sizeof(double);
  cout << " Quicksort of double array is \n";
  quicksort(da,0,size-1);
  print(da,size);
  size=sizeof(ia)/sizeof(int);
  cout << " Quicksort of integer array is \n";
  quicksort(ia,0,size-1);
  print(ia,size);
  return(0);
}
