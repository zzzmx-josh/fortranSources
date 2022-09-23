#include <stdio.h>
#include <string.h>

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
