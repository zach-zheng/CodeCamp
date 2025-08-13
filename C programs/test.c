#include <stdio.h>
  
/*定义两个全局变量*/
int x=6;
int y=2;
int addtwonum();
int main()
{
    int result;
    result = addtwonum();
    printf("result 为: %d\n",result);
    return 0;
}