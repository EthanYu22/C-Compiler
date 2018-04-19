#ifndef __ASTREE_H__
#define __ASTREE_H__

#include <string>
#include <vector>
#include "auxlib.h"
using namespace std;

extern stringstream outfile_ast;

struct astree
{
    int symbol;               // token code
    size_t filenr;            // index into filename stack
    size_t linenr;            // line number from source code
    size_t offset;            // offset of token with current line
    const string* lexinfo;    // pointer to lexical information
    vector<astree*> children; // children of this n-way node
    int block_number;
    string typeinfo;
};

astree* new_astree (int symbol, int filenr, int linenr,
                    int offset, const char* lexinfo);
astree* adopt1 (astree* root, astree* child);
astree* adopt2 (astree* root, astree* left, astree* right);
void pt_node(astree* node);
astree* adopt(astree* grandparent, astree* parent);
astree* adopt1sym (astree* root, astree* child, int symbol);
void dump_astree (FILE* outfile, astree* root);
void yyprint (FILE* outfile, unsigned short toknum,
              astree* yyvaluep);
void free_ast (astree* tree);
void free_ast2 (astree* tree1, astree* tree2);
void astreeRec(astree* root, int depth);
void pt_ast();

#endif
