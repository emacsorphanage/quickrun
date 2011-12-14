#include <stdio.h>
#include <unistd.h>

int main(void)
{
    int i;

    for (i = 0; i < 10; i++) {
        printf("hello %d\n", i);
        sleep(1);
    }

    return 0;
}
