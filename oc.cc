#include <errno.h>
#include <fstream>
#include <getopt.h>
#include <iomanip>
#include <iostream>
#include <libgen.h>
#include <string>
#include <sstream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unordered_set>
#include <wait.h>
#include <vector>
#include "astree.h"
#include "auxlib.h"
#include "lyutils.h"
#include "stringset.h"
#include "symtable.h"
using namespace std;

const string CPP = "/usr/bin/cpp";
const size_t LINESIZE = 1024;
stringstream outfile_tok;
stringstream outfile_ast;
SymbolTable* type_table = new SymbolTable(NULL);
SymbolTable* sym_table = new SymbolTable(NULL);

string check_oil_type (string type);
string check_functions (astree* n);

FILE *oil_file;

bool compare_to(string type)
{
    if(type_table->lookup(type) == NULL) return false;

    if(type_table->lookup(type)->symbol == TOK_STRUCTDEF|| type == "string") return true;

    size_t x = type.find("[]");

    if(x != string::npos) return true;
    return false;
}
//--------------------------------------------------------------------------------
// CHECK TYPE FOR ALL TYPES IN SYMBOL TABLE
string type_check(astree* tree, SymbolTable* sym)
{
// CONSTANTS
    if(tree->symbol == TOK_CONSTANT)
    {
        switch(tree->children[0]->symbol)
        {
            //case TOK_TRUE:
            case TOK_FALSE:
            {
                tree->typeinfo = "bool";
                return "bool";
            }
            case TOK_INTCON:
            {
                tree->typeinfo = "int";
                return "int";
            }
            case TOK_STRINGCON:
            {
                tree->typeinfo = "string";
                return "string";
            } 
            case TOK_CHARCON:
            {
                tree->typeinfo = "char";
                return "char";
            }           
            case TOK_NULL:
            {
                tree->typeinfo = "null";
                return "null";
            }
        }
    }
// BINOPS
    else if(tree->symbol == TOK_BINOP)
    {
        switch(tree->children[1]->symbol)
        {
            case '=':
            {
                string flag1 = type_check(tree->children[0],sym);
                string flag2 = type_check(tree->children[2],sym);
                if(flag1 != flag2 && !(compare_to(flag1) &&flag2 == "null"))
                {
                    set_exitstatus(1);
                    errprintf("Incompatible Types");     
                }
                tree->typeinfo = "flag1";
                return flag1;
            }
            case TOK_EQ:
            case TOK_NE:
            {
                string flag1 = type_check(tree->children[0],sym);
                string flag2 = type_check(tree->children[2],sym);
                if(flag1 != flag2 && !(compare_to(flag1) &&flag2 == "null")
                   && !(compare_to(flag2) && flag1 == "null"))
                {
                    errprintf("Incompatible Types");
                }
                tree->typeinfo = "bool";
                return "bool";
            }
            case '<':
            case TOK_LE:
            case '>':
            case TOK_GE:{
                string flag1 = type_check(tree->children[0],sym);
                string flag2 = type_check(tree->children[2],sym);
                if(flag1 != flag2 && (flag1 == "bool" ||flag1 == "char" || flag1 == "int")
                   &&(flag2 == "bool" || flag2 == "char" ||flag2 == "int"))
                {
                    errprintf("Incompatible Types"); 
                }
                else if(flag1 != flag2)
                {
                    set_exitstatus(1);
                    errprintf("Incompatible Types");
                }
                tree->typeinfo = "bool";
                return "bool";
            }
            case '+':
            case '-':
            case '*':
            case '/':
            case '%':
            {
                string flag1 = type_check(tree->children[0],sym);
                string flag2 = type_check(tree->children[2],sym);
                if(flag1 != "int")
                {
                    set_exitstatus(1);
                    errprintf("Incompatible Types");
                }
                if(flag2 != "int")
                {
                    set_exitstatus(1);
                    errprintf("Incompatible Types");
                }
                tree->typeinfo = "int";
                return "int";
            }
        }
    }
// UNOPS
    else if(tree->symbol == TOK_UNOP)
    {
        switch(tree->children[0]->symbol)
        {
            case '+':
            case '-':
            {
                string flag = type_check(tree->children[1], sym);
                if(flag != "int")
                {
                    set_exitstatus(1);
                    errprintf("Incompatible Types");
                }
                tree->typeinfo = "int";
                return "int";
            }
            case '!':
            {
                string flag = type_check(tree->children[1],sym);
                if(flag != "bool")
                {
                    set_exitstatus(1);
                    errprintf("Incompatible Types");
                }
                tree->typeinfo = "bool";
                return "bool";
            }
            case TOK_ORD:
            {
                string flag = type_check(tree->children[1],sym);
                if(flag != "char")
                {
                    set_exitstatus(1);
                    errprintf("Incompatible Types");
                }
                tree->typeinfo = "int";
                return "int";
            }
            case TOK_CHR:
            {
                string flag = type_check(tree->children[1], sym);
                if(flag != "int")
                {
                    set_exitstatus(1);
                    errprintf("Incompatible Types");
                }
                tree->typeinfo = "char";
                return "char";
            }
        }
    }
// VARIABLES
    else if(tree->symbol == TOK_VAR)
    {
        string name = *((tree->children[0])->lexinfo);
        astree* type = sym->lookup(name);
        if(type == NULL)
        {
            set_exitstatus(1);
            errprintf("Unknown Variables");
        }
        else
        {
            string string_type = SymbolTable::get_Type(type);
            tree->typeinfo = string_type;
            tree->block_number = type->block_number;
            return string_type;
        }
    }
// ARRAYS
    else if(tree->symbol == TOK_ARRAY)
    {
        string flag1_type = type_check(tree->children[0], sym);
        if((*(flag1_type.end()-2) =='[' && *(flag1_type.end()-1) == ']')
            || flag1_type == "string")
        {
            string flag2_type = type_check(tree->children[1], sym);
            if(flag2_type != "int")
            {
                set_exitstatus(1);
                errprintf("Variable Declaration");
            }
        }
        else
        {
            set_exitstatus(1);
            errprintf("Variable Declaration");
        }
        if(flag1_type == "string")
        {
            tree->typeinfo = "char";
            return "char";
        }
        else if(flag1_type == "")
        {
            tree->typeinfo = "";
            return "";   
        }
        else
        {
            tree->typeinfo = string(flag1_type.begin(), flag1_type.end()-2);
            return string(flag1_type.begin(), flag1_type.end()-2);
        }
    }
// FIELDS
    else if(tree->symbol == TOK_FIELD)
    {
        astree* ret_T = NULL;
        string type = type_check(tree->children[0], sym);
        astree* struct_T = type_table->lookup(type);
        if(struct_T->symbol != TOK_STRUCTDEF)
        {
            set_exitstatus(1);
            errprintf("Unknown Types");
        }
        else
        {
            string field_n = *(tree->children[1]->lexinfo);
            bool ffind = false;
            vector<astree*>::iterator st = struct_T->children.begin() + 1;
            vector<astree*>::iterator end = struct_T->children.end();
            for(; st != end; st++)
            {
                if(*((*st)->children[1]->lexinfo) == field_n)
                {
                    ffind = true;
                    ret_T = (*st)->children[0];
                    break;
                }
            }
            if(!ffind)
            {
                set_exitstatus(1);
                errprintf("Unknown Types");
            }
        }
        if(ret_T == NULL)
        {
            tree->typeinfo = "void";
            return "void";
        }
        tree->typeinfo = "return";
        return SymbolTable::get_Type(ret_T);

        tree->typeinfo = SymbolTable::get_Type(ret_T);
        return SymbolTable::get_Type(ret_T);
    }
// CALL
    else if(tree->symbol == TOK_CALL)
    {
        vector<string> sig_v; 
        vector<string> f_sig;
        string funcName = *(tree->children[0]->lexinfo);
        sig_v.push_back("");
        vector<astree*>::iterator expr = tree->children.begin() + 1;
        vector<astree*>::iterator end = tree->children.end();

        for(; expr != end; expr++)
        {
            sig_v.push_back(type_check(*expr, sym));
        }
        
        astree* funcAst = sym->lookup(funcName);
        if(funcAst == NULL){
            set_exitstatus(1);
            errprintf("Calls");
        }
        else{
            f_sig = SymbolTable::parseSignature(SymbolTable::fun_sig(funcAst));
            sig_v[0] = f_sig[0];
            if(f_sig != sig_v){
                set_exitstatus(1);
                errprintf("Calls");
                for(auto i = sig_v.begin()+1; i != sig_v.end(); i++)
                {
                    if(i == sig_v.end() - 1) cerr << *i;
                    else cerr << *i << ", ";
                }
                errprintf(")\n");
            }
        }        
        tree->typeinfo = sig_v[0];
        return sig_v[0];
    }
// ALLOCATOR
    else if(tree->symbol == TOK_ALLOCATOR)
    {
        if(tree->children.size() == 2)
        {
            if(*(tree->lexinfo) == string("parentheses_alloc"))
            {
                string basetype = *(tree->children[0]->children[0]->lexinfo);
                string flag = type_check(tree->children[1], sym);
                if(flag != basetype)
                {
                    set_exitstatus(1);
                    errprintf("Types Incompatible");
                }
                tree->typeinfo = basetype;
                return "string";
            }
            else
            {
                string basetype = *(tree->children[0]->children[0]->lexinfo);
                string flag = type_check(tree->children[1], sym);
                if(flag != "int")
                {
                    set_exitstatus(1);
                    errprintf("Variable Declaration");
                }
                tree->typeinfo = basetype + "[]";
                return (basetype + "[]");
            }
        }
        else 
        {
            tree->typeinfo = "basetype";
            string basetype = *(tree->children[0]->children[0]->lexinfo);
            return basetype;
        }
    }
    else if(tree->symbol == TOK_STRUCTDEF)
    {
        tree->typeinfo = *(tree->children[0]->lexinfo);
        return *(tree->children[0]->lexinfo);
    }
    return "";
}
//--------------------------------------------------------------------------------
// TRAVERSES AND ASSEMBLES SYMBOL TABLE
void recursive_call(astree* tree, SymbolTable* sym){
    if(tree->symbol == TOK_BLOCK)
    {
        SymbolTable* newBlock = sym->enterBlock();
        for(vector<astree*>::iterator i = tree->children.begin(); i != tree->children.end(); i++)
        {
            recursive_call(*i, newBlock);
        }
    }
    else if(tree->symbol == TOK_VARDECL)
    {
        string name = *(tree->children[1]->lexinfo);
        astree* type = tree->children[0]; 
        if(type_table->lookup(SymbolTable::get_Type(type)) != NULL)
        {
            sym->addSymbol(name, type);
            type->block_number = sym->get_number();
        }
        else
        {
            set_exitstatus(1);
            errprintf("Unknown Types");
        }
        string TypeStr = SymbolTable::get_Type(type);
        string expressionType = type_check(tree->children[2], sym);
        if(TypeStr != expressionType && !((compare_to(TypeStr) && expressionType == "null")))
        {
            set_exitstatus(1);
            errprintf("Incompatible Types");
        }
    }
    else if(tree->symbol == TOK_IFELSE)
    {
        vector<astree*>::iterator i = tree->children.begin();
        string conditionalType = type_check(*i, sym);
        if(conditionalType != "bool")
        {
            set_exitstatus(1);
            errprintf("Incompatible Types");
        }
        i++;
        vector<astree*>::iterator j = (tree->children.end());
        for(; i != j; i++) 
        {
            recursive_call(*i, sym);
        }
    }
    else if(tree->symbol == TOK_WHILE)
    {
        string conditionalType = type_check(tree->children[0], sym);
        if(conditionalType != "bool")
        {
            set_exitstatus(1);
            errprintf("Incompatible Types");
        }
        recursive_call(tree->children[1], sym);
    }
    else if(tree->symbol == TOK_RET)
    {
        astree* funcAST = sym->parentFunction(NULL);
        string funcType;
        if(funcAST == NULL)
        {
            funcType = "void"; 
        }
        else 
        {
            funcType = (SymbolTable::parseSignature(SymbolTable::fun_sig(funcAST)))[0];
        }
        if(tree->children[0]->symbol == ';')
        {
            string toRetType = "void";
            if(funcType != toRetType)
            {
                set_exitstatus(1);
                errprintf("Calls");
            }
        }
        else
        {
            string toRetType = type_check(tree->children[0], sym);
            if(funcType != toRetType)
            {
                set_exitstatus(1);
                if(funcAST == NULL) errprintf("Calls");
                else errprintf("Calls");
                errprintf("Calls");
            }
        }
    }
    else
    {
        type_check(tree, sym);
    }
}
//--------------------------------------------------------------------------------
// STRUCTS
void struct_call(astree* structdefAstree)
{
    string name = *(structdefAstree->children[0]->lexinfo);
    SymbolTable* st_block = type_table->enterFunction(name, structdefAstree);
    type_table->enterFunction(name + "[]", structdefAstree);
    vector<astree*>::iterator st = structdefAstree->children.begin() + 1;
    vector<astree*>::iterator end = structdefAstree->children.end();
    for(; st != end; st++)
    {
        st_block->addSymbol(*((*st)->children[1]->lexinfo), (*st)->children[0]);
    }
}
//--------------------------------------------------------------------------------
// FUNCTIONS
void function_call(astree* funcAstree, SymbolTable* sym)
{   
    string name = *(funcAstree->children[1]->lexinfo); 
    astree* sig = funcAstree; 
    SymbolTable* f_block = sym->enterFunction(name,sig); 
    vector<astree*>::iterator st = funcAstree->children.begin() + 2;
    vector<astree*>::iterator end = funcAstree->children.end() - 1;
    for(; st != end; st++)
    {
        f_block->addSymbol(*((*st)->children[1]->lexinfo), (*st)->children[0]);
        (*st)->children[0]->block_number = f_block->get_number();
    }
    vector<astree*>::iterator i = (*(funcAstree->children.end() - 1))->children.begin();
    vector<astree*>::iterator j = (*(funcAstree->children.end() - 1))->children.end();
    for(; i != j; i++)
    {
        recursive_call(*i, f_block);
    }
}
//--------------------------------------------------------------------------------
// REMOVES FILE EXTENSIONS TO ALLOW NEW FILES WITH NEW EXTENSIONS
string Remove_EXT()
{
    string ext(get_execname());
    unsigned int remove = ext.length();
    for(unsigned int i = 0; i < ext.length(); ++i)
    {
        if(ext[i] == '.')
        {
            remove = i;
        }
    }
    return ext.substr(0, remove);
}
//--------------------------------------------------------------
// CHECKS WHAT TO PRINT FOR OIL FORMAT
string check_oil_type (string type)
{
   if (type == "bool" || type == "char")
   {
      return "ubyte";
   }
   else if (type == "int")
   {
      return "int";
   }
   else if (type == "int[]")
   {
      return "int*";
   }
   else if (type == "string" ||
            type == "bool[]" ||
            type == "char[]")
   {
      return "ubyte*";
   }
   else if (type == "void")
   {
      return "void";
   }
   else
   {
      // IF ALL OTHER TYPES END WITH []
      if (*(type.end()-1) == ']')
      {
         return "struct " + string (type.begin(), type.end()-2) + " **";
      }
      else
      {
         return "struct " + type + " *";
      }
   }
}
int count = 0;
string check_functions (astree* n)
{

// TRAVERSE THROUGH THE FUNCTIONS WITHIN THE BLOCK
// CALL
   if ((n)->symbol == TOK_CONSTANT)
   {
      if (((n)->children[0]->symbol == TOK_NULL) ||
          ((n)->children[0]->symbol == TOK_FALSE))
      {
         //fprintf (oil_file, "0 ");
         return "0";
      }
      if ((n)->children[0]->symbol == TOK_TRUE)
      {
         //fprintf (oil_file, "1 ");
         return "1";
      }
      string w = *((n)->children[0]->lexinfo);
      //fprintf (oil_file, "%s\n", w.c_str());
      return w.c_str();
   }
// BLOCK
   if ((n)->symbol == TOK_BLOCK)
   {
      vector<astree*>::iterator x = (n)->children.begin();
      vector<astree*>::iterator x_end = (n)->children.end();
      for (; x != x_end; ++x)
      {
         check_functions(*x);
      }
   }
// VARDECL
   if (n->symbol == TOK_VARDECL)
   {
      string type = check_oil_type(n->children[2]->typeinfo);
      string type2 = check_functions(n->children[2]);
      string word = *((n)->children[1]->lexinfo);
      int number = n->children[0]->block_number;
      fprintf(oil_file, "        %s_%d_%s = %s;\n",
               type.c_str(), number, word.c_str(), type2.c_str());
   }
// ARRAY
   if (n->symbol == TOK_ARRAY)
   {
      string word = check_functions(n->children[0]);
      string word2 = check_functions(n->children[1]);
      return word + "[" + word2 + "]";
   }
   if (n->symbol == TOK_FIELD)
   {
      string word = check_functions (n->children[0]);
      string type = check_oil_type (n->typeinfo);
      string temp;
      if (type == "ubtye ")
      {
         stringstream str;
         str << ++count;
         temp = "b" + str.str();
      }
      else if (type == "int ")
      {
         stringstream str;
         str << ++count;
         temp = "i" + str.str();
      }
      else
      {
         stringstream str;
         str << ++count;
         temp = "p" + str.str();
      }
      return temp;
   }
// CALL
   if (n->symbol == TOK_CALL)
   {
      if (n->typeinfo == "void")
      {
         stringstream word;
         vector<astree*>::iterator i = n->children.begin();
         vector<astree*>::iterator i_end = n->children.end()-1;
         for (; i_end != i; --i_end)
         {
            if (i_end == n->children.begin()+1)
            {
               word << check_functions(*i_end);
            }
            else
            {
               word << check_functions(*i_end) << ", ";
            }
         }
         string word2 = *(n->children[0]->lexinfo);
         string word3 = word.str();
         fprintf (oil_file, "        __%s(%s);\n", word2.c_str(), word3.c_str());
      }
      else
      {
         stringstream word;
         vector<astree*>::iterator j = n->children.begin();
         vector<astree*>::iterator j_end = n->children.end()-1;
         for (; j_end != j; --j_end)
         {
            if (j_end == n->children.begin()+1)
            {
               word << check_functions(*j_end);
            }
            else
            {
               word << check_functions(*j_end) << ", ";
            }
         }
         string type = check_oil_type (n->typeinfo);
         string temp;
         string word2 = *(n->children[0]->lexinfo);
         if (type == "ubtye ")
         {
            stringstream str;
            str << ++count;
            temp = "b" + str.str();
         }
         else if (type == "int ")
         {
            stringstream str;
            str << ++count;
            temp = "i" + str.str();
         }
         else
         {
            stringstream str;
            str << ++count;
            temp = "p" + str.str();
         }
         fprintf (oil_file, "        %s %s = __%s(%s);\n",
                  type.c_str(), temp.c_str(), word2.c_str(),
                  word.str().c_str());
         return temp;
      }
   }
// WHILE
   if (n->symbol == TOK_WHILE)
   {
      stringstream str, str2;
      str << ++count;
      string whilelabel = "while_" + str.str();
      str2 << ++count;
      string breaklabel = "break_" + str2.str();
      fprintf (oil_file, "%s:;\n", whilelabel.c_str());
      string word = check_functions (n->children[0]);
      fprintf (oil_file, "        goto %s;\n", whilelabel.c_str());
      fprintf (oil_file, "%s:;\n", breaklabel.c_str());
   }
// ALLOCATOR
   if (n->symbol == TOK_ALLOCATOR)
   {
      if (n->children.size() == 2)
      {
         if (*(n->lexinfo) == string("paren_alloc"))
         {
            string type = check_oil_type(n->typeinfo);
            string temp;
            if (type == "ubtye ")
            {
               stringstream str;
               str << ++count;
               temp = "b" + str.str();
            }
            else if (type == "int ")
            {
               stringstream str;
               str << ++count;
               temp = "i" + str.str();
            }
            else
            {
               stringstream str;
               str << ++count;
               temp = "p" + str.str();
            }
            string word = check_functions(n->children[1]);
            fprintf(oil_file, "        %s %s = xcalloc(%s, sizeof(ubtye)\n",
                    type.c_str(), temp.c_str(), word.c_str() );
            return temp;
         }
         else
         {
            string type = check_oil_type (n->typeinfo);
            string temp;
            if (type == "ubtye ")
            {
               stringstream str;
               str << ++count;
               temp = "b" + str.str();
            }
            else if (type == "int ")
            {
               stringstream str;
               str << ++count;
               temp = "i" + str.str();
            }
            else
            {
               stringstream str;
               str << ++count;
               temp = "p" + str.str();
            }
            string type1 = string(type.begin(), type.end()-2);
            string word = check_functions(n->children[1]);
            fprintf(oil_file, "        %s = xcalloc(%s, sizeof(%s));\n",
                     word.c_str(), temp.c_str(), type1.c_str());
         }
      }
      else
      {
         string type = check_oil_type (n->typeinfo);
         string temp;
         if (type == "ubtye ")
         {
            stringstream str;
            str << ++count;
            temp = "b" + str.str();
         }
         else if (type == "int ")
         {
            stringstream str;
            str << ++count;
            temp = "i" + str.str();
         }
         else
         {
            stringstream str;
            str << ++count;
            temp = "p" + str.str();
         }
         string type1 = string(type.begin(), type.end()-2);
         fprintf (oil_file, "        %s %s = xcalloc(1, sizeof(%s));\n",
                  type.c_str(), temp.c_str(), type1.c_str());
      }
   }
// RETURN
   if (n->symbol == TOK_RETURN)
   {
      string word = check_functions(n->children[0]);
      fprintf (oil_file, "        return%s;\n)", word.c_str());
   }
// IFELSE
   if (n->symbol == TOK_IFELSE)
   {
      string word = check_functions(n->children[0]);
      stringstream str1, str2;
      str1 << ++count;
      string word2 = "else_" + str1.str();
      fprintf (oil_file, "        if (!%s) goto %s;\n",
               word.c_str(), word2.c_str());
      check_functions(n->children[1]);
      str2 << ++count;
      string word3 = "fi_" + str2.str();
      fprintf (oil_file, "        goto %s;\n", word3.c_str());
      fprintf (oil_file, "%s:;\n", word2.c_str());
      if (n->children.size() > 2)
      {
         check_functions(n->children[2]);
      }
      fprintf(oil_file, "%s:;\n", word3.c_str());
   }
// BINOP
   if (n->symbol == TOK_BINOP)
   {
      if(n->children[1]->symbol == '=')
      {
         string word = check_functions(n->children[0]);
         string word2 = check_functions(n->children[2]);
         fprintf (oil_file, "        %s = %s\n", word.c_str(), word2.c_str());
         return word2;
      }
      else
      {
         string word = check_functions(n->children[0]);
         string word2 = check_functions(n->children[2]);
         string word3 = *(n->children[1]->lexinfo);
         string type = check_oil_type(n->typeinfo);
         string temp;
         if (type == "ubtye ")
         {
            stringstream str;
            str << ++count;
            temp = "b" + str.str();
         }
         else if (type == "int ")
         {
            stringstream str;
            str << ++count;
            temp = "i" + str.str();
         }
         else
         {
            stringstream str;
            str << ++count;
            temp = "p" + str.str();
         }
         fprintf (oil_file, "        %s %s = %s %s %s;\n",
                  type.c_str(), temp.c_str(), word.c_str(),
                  word3.c_str(), word2.c_str());
         return temp;
      }
   }
// UNOP
   if (n->symbol == TOK_UNOP)
   {
      if(*(n->children[0]->lexinfo) == "ord")
      {
         string word = check_functions(n->children[1]);
         string type = check_oil_type(n->typeinfo);
         string temp;
         if (type == "ubtye ")
         {
            stringstream str;
            str << ++count;
            temp = "b" + str.str();
         }
         else if (type == "int ")
         {
            stringstream str;
            str << ++count;
            temp = "i" + str.str();
         }
         else
         {
            stringstream str;
            str << ++count;
            temp = "p" + str.str();
         }
         fprintf (oil_file, "        %s %s = (int)%s;\n",
                  type.c_str(), temp.c_str(), word.c_str());
         return temp;
      }
      else if(*(n->children[0]->lexinfo) == "chr")
      {
         string word = check_functions(n->children[1]);
         string type = check_oil_type(n->typeinfo);
         string temp;
         if (type == "ubtye ")
         {
            stringstream str;
            str << ++count;
            temp = "b" + str.str();
         }
         else if (type == "int ")
         {
            stringstream str;
            str << ++count;
            temp = "i" + str.str();
         }
         else
         {
            stringstream str;
            str << ++count;
            temp = "p" + str.str();
         }
         fprintf (oil_file, "        %s %s = (ubtye)%s;\n",
                  type.c_str(), temp.c_str(), word.c_str());
         return temp;
      }
      else
      {
         string word = check_functions(n->children[1]);
         string type = check_oil_type(n->typeinfo);
         string temp;
         if (type == "ubtye ")
         {
            stringstream str;
            str << ++count;
            temp = "b" + str.str();
         }
         else if (type == "int ")
         {
            stringstream str;
            str << ++count;
            temp = "i" + str.str();
         }
         else
         {
            stringstream str;
            str << ++count;
            temp = "p" + str.str();
         }
         fprintf (oil_file, "        %s %s = %s;\n",
                  type.c_str(), temp.c_str(), word.c_str());
         return temp;
      }
   }
// VAR
   if (n->symbol == TOK_VAR)
   {
      stringstream str;
      str << "_";
      if (n->block_number != 1)
      {
         str << n->block_number;
      }
      str << "_" << *(n->children[0]->lexinfo);
      return str.str();
   }

   return "";
}
//--------------------------------------------------------------------------------
// MAIN
int main (int argc, char **argv)
{
   set_execname (argv[0]);
   char* d_option=NULL;
   char* filename=NULL;
   string command;
   yy_flex_debug = 0;
   yydebug = 0;
   int c;

   while ((c = getopt(argc,argv, "@:D:ly")) != -1)

// SCANS OPTIONS
   switch(c)
   {
     case '@': set_debugflags (optarg);   break;
     case 'D': d_option=optarg;   break;
     case 'l': yy_flex_debug = 1;         break;
     case 'y': yydebug = 1;               break;
     default: errprintf("bad option %c\n", optopt); break;      
   }
// IF NO FILES SPECIFIED
    if(optind == argc)
    {
        errprintf("Usage: %s [-ly] [-@flag...] [-D string] program.oc\n", get_execname());
        set_exitstatus(1);
        pclose(yyin); 
        exit(get_exitstatus());
    }
    else
    {   
        filename = argv[optind];
        set_execname(filename);

// COMMAND TO BE PASSED INTO CPP
        if(d_option != NULL)
        {
           command = CPP + " " + "-D" + d_option + " " + filename;
        }
        else
        {
           command = CPP + " " + filename;
        }
        yyin = popen (command.c_str(), "r");  
    }
    yyparse();
    int close = pclose (yyin);
    if (close!=0)
    {
       set_exitstatus(1);
    }
    if(get_exitstatus() == 0)
    {
       type_table->addSymbol ("bool",new_astree(TOK_BOOL, 0, 0, 0, "bool"         ));
       type_table->addSymbol ("char",new_astree(TOK_CHAR, 0, 0, 0, "char"         ));
       type_table->addSymbol ("void",new_astree(TOK_VOID, 0, 0, 0, "void"         )); 
       type_table->addSymbol ("int", new_astree(TOK_INT, 0, 0, 0, "int"           ));
       type_table->addSymbol ("string",new_astree(TOK_STRING, 0, 0, 0, "string"   ));
       type_table->addSymbol ("string[]",new_astree(TOK_ARRAY, 0, 0, 0, "string[]"));
       type_table->addSymbol ("bool[]",new_astree(TOK_ARRAY, 0, 0, 0, "bool[]"    ));
       type_table->addSymbol ("char[]",new_astree(TOK_ARRAY, 0, 0, 0, "char[]"    ));
       type_table->addSymbol ("int[]", new_astree(TOK_ARRAY, 0, 0, 0, "int[]"     ));

// INITIATES SYMBOL TABLE
       for (vector<astree*>::iterator i = yyparse_astree->children.begin(); i !=
                                             yyparse_astree->children.end(); i++)
       {
          if((*i)->symbol == TOK_FUNCTION)
          {
             function_call(*i, sym_table);
          } 
          else if((*i)->symbol == TOK_STRUCTDEF)
          {
             struct_call(*i);
          }
          else
          {
             recursive_call(*i, sym_table);
          }
       }

//FILES---------------------------------------------------------
// .STR
       char *new_filename = filename;
       new_filename[strlen(new_filename)-2]='s';
       new_filename[strlen(new_filename)-1]='t';
       strcat (new_filename, "r");
       FILE* strFile = fopen(new_filename, "w");
       dump_stringset(strFile);
       fclose(strFile);
// .TOK      
       ofstream tokFileStream;
       tokFileStream.open(Remove_EXT() + ".tok");
       tokFileStream << outfile_tok.str();
       tokFileStream.close();
// .AST
       ofstream astFileStream;
       astFileStream.open(Remove_EXT() + ".ast");
       pt_ast();
       astFileStream << outfile_ast.str();
       astFileStream.close();
// .SYM       
       char *new_filenames = filename;
       new_filenames[strlen(new_filenames)-3]='s';
       new_filenames[strlen(new_filenames)-2]='y';
       new_filenames[strlen(new_filenames)-1]='m';
       FILE* outfile_sym = fopen(new_filenames, "w");
       sym_table->dump(outfile_sym, 0);
       fclose(outfile_sym);

//--------------------------------------------------------------
// ASG 5
//--------------------------------------------------------------
// .OIL
       new_filename[strlen(filename)-4] = '\0';
       strcat(filename, ".oil");
       oil_file = fopen (filename, "w");
       filename[strlen(filename)-4] = '\0';
//       cout << filename << endl;

// FILE PREAMBLE
       fprintf (oil_file, "#define __OCLIB_C__");
       fprintf (oil_file, "\n");
       fprintf (oil_file, "#include \"oclib.oh\"\n\n");
//--------------------------------------------------------------
// TRAVERSE AND GENERATE STRUCTURE DEFINITION CODE
   for (vector<astree*>::iterator i = yyparse_astree->children.begin();
         i != yyparse_astree->children.end(); ++i)
   {
      if ((*i)->symbol == TOK_STRUCTDEF)
      {
         fprintf (oil_file, "struct ");
         string word = *((*i)->children[0]->lexinfo);
         fprintf (oil_file, "%s", word.c_str());
         fprintf (oil_file, " {\n");
         vector<astree*>::iterator begin = (*i)->children.begin() + 1;
         vector<astree*>::iterator end = (*i)->children.end();
         for (; begin != end; ++begin)
         {
            // 8 spaces (tab-8)
            fprintf (oil_file, "        ");
            string word2 = SymbolTable::get_Type((*begin)->children[0]);
            string word3 = check_oil_type(word2);
            fprintf (oil_file, "%s ", word3.c_str());
            string word4 = *((*begin)->children[1]->lexinfo);
            fprintf(oil_file, "%s;\n", word4.c_str());
         }
         fprintf(oil_file, "};\n\n");
      }
   }
//--------------------------------------------------------------
// TRAVERSE and GENERATE GLOBAL VARIABLES ETC.
   for (vector<astree*>::iterator i = yyparse_astree->children.begin();
         i != yyparse_astree->children.end(); ++i)
   {
      if ((*i)->symbol == TOK_VARDECL)
      {
         string word = SymbolTable::get_Type((*i)->children[0]);
         string word2 = check_oil_type(word);
         string word3 = *((*i)->children[1]->lexinfo);
         fprintf (oil_file, "%s __%s;\n", word2.c_str(), word3.c_str());
      }
   }
   fprintf (oil_file, "\n");
//--------------------------------------------------------------
// TRAVERSE FUNCTIONS ETC.
   for (vector<astree*>::iterator i = yyparse_astree->children.begin();
         i != yyparse_astree->children.end(); ++i)
   {
      // CHECKS IF FUNCTION IS VALID
      if ((*i)->symbol == TOK_FUNCTION &&
            *((*((*i)->children.end()-1))->children[0]->lexinfo) != ";")
      {
         string word = SymbolTable::fun_sig (*i);
         vector<string> word2 = SymbolTable::parseSignature(word);
         string word3 = check_oil_type(word2[0]);
         fprintf(oil_file, "%s\n", word3.c_str());
         fprintf(oil_file, "__");
         string word4 = *((*i)->children[1]->lexinfo);
         fprintf(oil_file, "%s(\n", word4.c_str());
         vector<astree*>::iterator begin = (*i)->children.end()-2;
         vector<astree*>::iterator end = (*i)->children.begin()+1;
         for (; begin != end; --begin)
         {
            fprintf (oil_file, "        ");
            string w = SymbolTable::get_Type((*begin)->children[0]);
            string w2 = check_oil_type(w);
            fprintf(oil_file, w2.c_str());
            int number = (*begin)->children[0]->block_number;
            string w3 = *((*begin)->children[1]->lexinfo);
            fprintf(oil_file, "_%d_%s", number, w3.c_str());
            if (begin == (*i)->children.begin()+2)
            {
            }
            else
            {
               fprintf(oil_file, ",\n");
            }
         }
         fprintf(oil_file, ")\n");
         fprintf(oil_file, "{\n");

// CHECK FUNCTIONS AND RECURSIVELY CHECK ITSELF
         vector<astree*>::iterator start = (*((*i)->children.end()-1))->children.begin();
         vector<astree*>::iterator stop =(*((*i)->children.end()-1))->children.end();
         for(; start != stop; ++start)
         {
            check_functions(*start);
         }
         fprintf(oil_file, "}\n\n");
      }
   }
//--------------------------------------------------------------
// TRAVERSE OCMAIN
   fprintf (oil_file, "void __ocmain ()\n{\n");
   for (vector<astree*>::iterator i = yyparse_astree->children.begin();
         i != yyparse_astree->children.end(); ++i)
   {
      check_functions(*i);
   }
   fprintf (oil_file, "}\n");   
//--------------------------------------------------------------
   //const char* n_filename = filename;
   //string a =  string(n_filename);
   //system(("gcc -g -o " + a + " -x c " + a + ".oil " + " oclib.c").c_str());
   //int exit = system(("gcc -g -o " + a + " -x c " + a + ".oil " + " oclib.c").c_str());
   //if(exit != 0) set_exitstatus(EXIT_FAILURE);
   //fclose(oil_file);
//--------------------------------------------------------------
    }
    return get_exitstatus();
}

