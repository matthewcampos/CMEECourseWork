#ifndef _NODE_H
#define _NODE_H
//typedef to save us keystrokes
typedef struct _node node_t;
typedef struct _node{
    node_t *left; //pointer to left descendant
    node_t *right;
    node_t *anc; //defining struct type within type def which gives alias
    int tip;
    int mem_index;
    char *label;
} node_t;

#endif
