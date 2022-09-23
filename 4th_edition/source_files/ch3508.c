void reciprocal(int nrow,int ncol,
  float matrix1[nrow][ncol], 
  float matrix2[nrow][ncol])
{
  int i;
  int j;
  for (i=0;i<nrow;i++)
    for (j=0;j<ncol;j++)
      matrix2[i][j]=1.0f/matrix1[i][j];
}
