#include <stdio.h>
#include <stdlib.h>

int main (void)
{
    char name[128];
    int  age;

    printf("Your name >> ");
    fgets(name, 128, stdin);

    printf("Your age >> ");
    scanf("%d", &age);

    printf("Your name is %s", name);
    printf("Your age  is %d\n", age);
    return 0;
}
