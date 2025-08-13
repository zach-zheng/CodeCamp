#include <stdio.h>

int LENGTH = 10;
int WIDTH = 5;
#define NEWLINE '\n'

int main() {
  int area;

  area = LENGTH * WIDTH;
  printf("value of area: %d", area);
  printf("%c", NEWLINE);

  return 0;
}