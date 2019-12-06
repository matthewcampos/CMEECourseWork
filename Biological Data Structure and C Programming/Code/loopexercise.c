#include <stdio.h>

int main()
{
  int i = 3, count, c;

  printf("2\n");

  for (count = 2; count <= 25;)
  {
    for (c = 2; c <= i - 1; c++) {
      if (i%c == 0)
        break;
    }

    if (c == i) {
      printf("%d\n", i);
      count++;
    }

    i++;
  }

  return 0;
}

printf("\n");

int main (void)
{
    int x = 10, y = 7, i;
    for (i=1 ; i<=100; i++){
        if (i%x == 0 || i%y ==0){
            printf("%i\n", i);
        }
    }
}

printf("\n");

int a = 0;
int i = 0;
while ( a == 0 ) {
    ++i;
} //infinite loop that will continue to run and keep incrementing i

printf("\n");


int main (void)
{
    int i;
    for (i = 0; i < 10; ++i) {
    if (i % 2 == 0) {
    	printf("%i is an even number\n", i);
	continue;
    }

    printf("%i is an odd number\n", i);
    }
}
