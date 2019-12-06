#include <stdio.h>

int main (void)
{
    int i = 0;
    int intarray[] = {4,8,5,44};
    char hello[] = "Hello!"

    // While loop
    while (i < 4){
        printf("%i\n", intarray[i]);
        ++i;
    }
    printf("\n")

    i = 0;
    do {
        printf("%i\n", intarray[i]);
        ++i;
    } while (i < 4);
    printf("\n")
    printf("using a for loop\n");
    for (i = 0 ; i < 4 ; i++){
        printf("%i\n", intarray[i]);
    }
    printf("\n")
    printf("Reading backwards\n");
    for (i = 3; i >= 0; --i){
        printf("%i\n", intarray[i]);
    }
    printf("\n")
    printf("Reading backwards\n");
    for (i = 4; i -- ; ){ //Executes printf then decrements
        printf("%i\n", intarray[i]);
    }

    // char hello[] = "Helllo!";

    for (i = 0; i < 6; ++i){
        putchar(hello[i]);
    }
    printf("\n")
    for (i = 0; hello[i]; ++i){
        putchar(hello[i]);
    }
    printf("\n")
    printf("%s",hello);

    return 0;
}
