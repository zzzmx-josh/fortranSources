float summation(float *x,int n)
{
  int i;
  float t;
    t=0.0f;
    for (i=0;i<n;i++)
    {
      t+=x[i];
    }
    return(t);
}
