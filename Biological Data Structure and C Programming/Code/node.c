#include "node.h"
#include <stdlib.h>
#include <stdio.h>

void node_traverse(node_t* n)
{/*printf("mem)index of node: %i\n", n->mem_index)*/
    printf("(" );
    if (n->tip != 0){
        return;
    }

    node_traverse(n->left);
    printf("," );
    node_traverse(n->right);
    printf(")" );

    return;
}
