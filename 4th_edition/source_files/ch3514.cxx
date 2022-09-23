#include <cstring>
#include <cstdio>

using namespace std;

extern "C" 
{
void print_string(char *);
}

extern "C" 
{
void replace_string(char *);
}

extern "C" 
{
void concatenate_string(char *);
}

void print_string(char * string)
{
  printf("                     %s\n",string);
}

void replace_string(char * string)
{
  strcpy(string,"Hello Hello");
}

void concatenate_string(char * string)
{
  strcat(string," Hello Hello");
}
