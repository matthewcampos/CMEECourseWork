#include <stdlib.h>
#include <stdio.h>

struct listobj {
    int data;
    struct listobj* next; //listobj is the data type and next is a pointer to the address of a listobj instance
}; //takes two values, int and pointer

void traverse_list(struct listobj* lobj) //takes a pointer of a listobj strucuutre and returns nothing (void)
{
    //function traverses a list recursively and calls out the integer stored inside
    if ((*lobj).next != NULL){
        printf("int data: %i\n", (*lobj).data); //prints forwards
        traverse_list((*lobj).next)
        printf("int data: %i\n", (*lobj).data); //prints backwards
    }
}

int main (void)
{
    struct listobj l1;
    struct listobj l2;
    struct listobj l3; //declare instances of datatype
    struct listobj l4;

    int intarray[3] = {10,21,33};
    l1.data = 10;
    l2.data = 21;
    l3.data = 33; //3 independent objects setting int for the listobj
    l4.data = 41;

    l1.next = &l2; // sets the pointers for each of the datastructures
    l2.next = &l3;
    l3.next = NULL;

    // Loop through linked list
    struct listobj * p = NULL;
    p = &l1;

    // Number selection
    int data = 0;
    data = (*p).data; //member takes precedence over astericks
    //or
    data = p -> data;

    // Looping to print values
    while (p!=NULL){
        printf("%i\n", (*p).data);
        p = p -> next;
    }
    printf("\n" );

    // Insert new element
    l4.next = &l2; //address to address of l2 list obj
    l1.next = &l1;

    p = &l1;
    while (p!=NULL){
        printf("%i\n", (*p).data);
        p = p -> next;
    }
    printf("\n" );

    // Traverse list recursively using Functions
        printf("recursively: \n");
        traverse_list(&l1);
        printf("\n");

}
