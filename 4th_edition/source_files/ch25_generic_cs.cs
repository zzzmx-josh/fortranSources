using System;
public static class generic
{

  public static void 
	swap< Type > (Type[] array,int i, int j)
  {
    Type tmp=array[i];
    array[i]=array[j];
    array[j]=tmp;
  }

  public static void 
	quicksort< Type > ( Type[] array, int l, int r)
    where Type : IComparable< Type >
  {
    int i=l;
    int j=r;
    Type v=array[(int)((l+r)/2)];
    for (;;)
    {
      while (array[i].CompareTo( v) < 0 ) i=i+1;
      while (v.CompareTo(array[j]) < 0) j=j-1;
      if (i<=j) 
        { swap(array,i,j); i=i+1 ; j=j-1; }
      if (i>j) goto ended ;
    }
    ended: ;
    if (l<j) quicksort(array,l,j);
    if (i<r) quicksort(array,i,r);
  }

  public static void 
	print< Type > (Type[] array,int size)
  {
    int i;
    int l;
    l=array.Length;
    for (i=0;i<l;i++)
      Console.WriteLine(array[i]);
  }

  public static int Main()
  {
    double[] da =
    {1.9,8.2,3.7,6.4,5.5,1.8,9.2,3.6,7.4,5.5};
    int[]    ia = {1,10,2,9,3,8,4,7,6,5};
    int size;
    size=da.Length;
    Console.WriteLine("Original array");
    print(da,size);
    quicksort(da,0,size-1);
    Console.WriteLine("Sorted array");
    print(da,size);
    size=ia.Length;
    Console.WriteLine("Original array");
    print(ia,size);
    quicksort(ia,0,size-1);
    Console.WriteLine("Sorted array");
    print(ia,size);
    return(0);
  }

}
