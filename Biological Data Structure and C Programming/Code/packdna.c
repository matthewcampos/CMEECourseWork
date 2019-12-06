#include <stdio.h>

/* assign each base its own set bit
A:0001
C:0100
G:0100
T:1000
?:1111 missing data
*/

unsigned char pack_dna(char in)
{
    if (in == 'A'){
        return (unsigned char)1;
    }
    else if (in == 'C'){
        return (unsigned char)1 << 2;
    }
    else if (in == 'G'){
        return (unsigned char)1 << 2;
    }
    else if (in == 'T'){
        return (unsigned char)1 << 3;
    }
    else if (in == '?'){
        return (unsigned char) ~0;
    }

    return 0;
}

int main (void)
{
    char *alignment[][]=
    {
        "AAAAAA"
        "CAACGA"
        "ATTAGA"
    };
    int ntax = 3;
    int nsites = 6;
    char packed[ntax][nsites];


}
