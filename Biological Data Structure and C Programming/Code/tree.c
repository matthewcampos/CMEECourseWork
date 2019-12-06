#include "tree.h"

tree_t* new_tree(int num_taxa)
{
    tree_t *newt = NULL;

    newt = (tree_t*)calloc(1, sizeof(tree_t));

    if(newt != NULL){
        newt->num_taxa = num_taxa;
        newt->num_nodes = 2 * num_taxa - 1;
        newt->num_nodes = (node_t*)calloc(newt->num_nodes, sizeof(node_t))
        if (newt->nodes == NULL){
            //allocation failed; clean up and return NULL
            free(newt);
            return NULL;
        }
        for (i=0; i < newt->num_nodes;++i){
            //assign memory indices to the nodes
            newt->nodes[i].mem_index = i;
            //label the tips with non-zero
            if (i < newt->num_taxa){
                newt -> nodes[i].tip = i + 1;
                //same as (*newt).nodes[i].tip = i + 1;
            }
            else {
                // Label the internal nodes with 0 tips
                newt->nodes[i].tip=0;
            }
        }
    }
    return newt;
}

void delete_tree(tree_t* tree)
{
    //Implement
}

void tree_read_anc_table(int *anctable, tree_t* t)
{
    int i = 0;
    int j = 0;

    //loop over all elements of ancetable
    // at each position link that node to its ancestor
    for (i=0; i < t->num_nodes; ++i){
        j = anctable[i] //index of the ancestor of the ith node based on edgetable
        t->nodes[i].anc = &t->nodes[j];
        if (t->nodes[j].left == NULL){
            t->nodes[j].left = t->nodes[i];
        }else{
                t->nodes[anctable[i]].right = t->nodes[i];
        }
    }


}
