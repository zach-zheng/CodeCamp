#include <stdio.h>

#define PI 3.1415926

int add(int a, int b);

int main() {
  int num1, num2, sum;

  printf("Enter two integers: ");
  scanf("%d %d", &num1, &num2);

  sum = add(num1, num2);

  printf("Sum: %d\n", sum);

  return 0;
}

int add(int a, int b) {
  return a + b;
}