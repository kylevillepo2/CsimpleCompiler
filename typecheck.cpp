#include <iostream>
#include <cstdio>
#include <cstring>

#include "ast.hpp"
#include "symtab.hpp"
#include "primitive.hpp"
#include "assert.h"

// WRITEME: The default attribute propagation rule
#define default_rule(X) (X)->visit_children(this);

#include <typeinfo>

class Typecheck : public Visitor
{
private:
    FILE *m_errorfile;
    SymTab *m_st;

    // The set of recognized errors
    enum errortype
    {
        no_main,
        nonvoid_main,
        dup_proc_name,
        dup_var_name,
        proc_undef,
        call_type_mismatch,
        narg_mismatch,
        expr_type_err,
        var_undef,
        ifpred_err,
        whilepred_err,
        incompat_assign,
        who_knows,
        ret_type_mismatch,
        array_index_error,
        no_array_var,
        arg_type_mismatch,
        expr_pointer_arithmetic_err,
        expr_abs_error,
        expr_addressof_error,
        invalid_deref
    };

    // Print the error to file and exit
    void t_error(errortype e, Attribute a)
    {
        fprintf(m_errorfile, "on line number %d, ", a.lineno);

        switch (e)
        {
        case no_main:
            fprintf(m_errorfile, "error: no main\n");
            exit(2);
        case nonvoid_main:
            fprintf(m_errorfile, "error: the Main procedure has arguments\n");
            exit(3);
        case dup_proc_name:
            fprintf(m_errorfile, "error: duplicate procedure names in same scope\n");
            exit(4);
        case dup_var_name:
            fprintf(m_errorfile, "error: duplicate variable names in same scope\n");
            exit(5);
        case proc_undef:
            fprintf(m_errorfile, "error: call to undefined procedure\n");
            exit(6);
        case var_undef:
            fprintf(m_errorfile, "error: undefined variable\n");
            exit(7);
        case narg_mismatch:
            fprintf(m_errorfile, "error: procedure call has different number of args than declartion\n");
            exit(8);
        case arg_type_mismatch:
            fprintf(m_errorfile, "error: argument type mismatch\n");
            exit(9);
        case ret_type_mismatch:
            fprintf(m_errorfile, "error: type mismatch in return statement\n");
            exit(10);
        case call_type_mismatch:
            fprintf(m_errorfile, "error: type mismatch in procedure call args\n");
            exit(11);
        case ifpred_err:
            fprintf(m_errorfile, "error: predicate of if statement is not boolean\n");
            exit(12);
        case whilepred_err:
            fprintf(m_errorfile, "error: predicate of while statement is not boolean\n");
            exit(13);
        case array_index_error:
            fprintf(m_errorfile, "error: array index not integer\n");
            exit(14);
        case no_array_var:
            fprintf(m_errorfile, "error: attempt to index non-array variable\n");
            exit(15);
        case incompat_assign:
            fprintf(m_errorfile, "error: type of expr and var do not match in assignment\n");
            exit(16);
        case expr_type_err:
            fprintf(m_errorfile, "error: incompatible types used in expression\n");
            exit(17);
        case expr_abs_error:
            fprintf(m_errorfile, "error: absolute value can only be applied to integers and strings\n");
            exit(17);
        case expr_pointer_arithmetic_err:
            fprintf(m_errorfile, "error: invalid pointer arithmetic\n");
            exit(18);
        case expr_addressof_error:
            fprintf(m_errorfile, "error: AddressOf can only be applied to integers, chars, and indexed strings\n");
            exit(19);
        case invalid_deref:
            fprintf(m_errorfile, "error: Deref can only be applied to integer pointers and char pointers\n");
            exit(20);
        default:
            fprintf(m_errorfile, "error: no good reason\n");
            exit(21);
        }
    }

    // Helpers
    // WRITEME: You might want write some helper functions.

    // Extract identifier from left-hand side expression
    const char *lhs_to_id(Lhs *p)
    {
        Variable *v = dynamic_cast<Variable *>(p);
        if (v)
        {
            return v->m_symname->spelling();
        }
        DerefVariable *dv = dynamic_cast<DerefVariable *>(p);
        if (dv)
        {
            return dv->m_symname->spelling();
        }
        ArrayElement *ae = dynamic_cast<ArrayElement *>(p);
        if (ae)
        {
            return ae->m_symname->spelling();
        }
        return nullptr;
    }
    // Convert expression to identifier if possible
    const char *exprCast(Expr *expr)
    {
        if (expr == NULL)
        {
            return nullptr;
        }
        Ident *i = dynamic_cast<Ident *>(expr);
        if (i)
        {
            return i->m_symname->spelling();
        }
        return nullptr;
    }
    bool are_equality_types_compatible(Basetype type1, Basetype type2)
    {
        // Check for exact type matches
        if (type1 == type2)
        {
            return is_equality_comparable_type(type1);
        }

        // Check for pointer compatibility
        if (is_pointer_type(type1) && is_pointer_type(type2))
        {
            return true;
        }

        return false;
    }
    bool is_equality_comparable_type(Basetype type)
    {
        return (type == bt_integer ||
                type == bt_boolean ||
                type == bt_char ||
                type == bt_charptr ||
                type == bt_intptr);
    }

    bool is_pointer_type(Basetype type)
    {
        return (type == bt_charptr ||
                type == bt_intptr ||
                type == bt_ptr);
    }

    // Type Checking
    // WRITEME: You need to implement type-checking for this project

    // Verify program has exactly one Main procedure
    void check_for_one_main(ProgramImpl *p)
    {
        char *name;
        Symbol *s;
        name = strdup("Main");
        s = m_st->lookup(name);

        if (!s)
        {
            this->t_error(no_main, p->m_attribute);
        }

        // Only check arguments if we found the Main procedure
        if (s && (s->m_arg_type).size() != 0)
        {
            this->t_error(nonvoid_main, p->m_attribute);
        }

        free(name); // Clean up allocated memory
    }

    // Register variable declarations in symbol table
    void add_decl_symbol(DeclImpl *p)
    {
        for (auto it = p->m_symname_list->begin(); it != p->m_symname_list->end(); it++)
        {
            Symbol *s = new Symbol();
            char *name = strdup((*it)->spelling());
            // fprintf(stderr, "DEBUG: Adding variable '%s' to scope %p\n", name, m_st->get_scope());

            s->m_basetype = p->m_type->m_attribute.m_basetype;

            // Set string size if this is a string variable
            if (s->m_basetype == bt_string) {
                TString* tstr = dynamic_cast<TString*>(p->m_type);
                if (tstr) {
                    s->m_string_size = tstr->m_primitive->m_data;
                }
            }

            // Try to insert the variable symbol in current scope
            if (!m_st->insert(name, s))
            {
                // If insert failed, there's already a symbol with this name in current scope
                delete s; // Clean up the symbol we created
                free(name);
                t_error(dup_var_name, p->m_attribute);
            }

            free(name); // Clean up allocated memory
        }
    }

    // Validate procedure return type matches declaration
    void check_proc(ProcImpl *p)
    {
        // Get procedure block and validate main procedure
        auto *pb = dynamic_cast<Procedure_blockImpl *>(p->m_procedure_block);
        const auto *proc_name = p->m_symname->spelling();

        // Validate main procedure
        if (strcmp(proc_name, "Main") == 0 && !p->m_decl_list->empty())
        {
            t_error(nonvoid_main, p->m_attribute);
            return;
        }

        // Extract types for clarity
        const auto actual_type = pb->m_return_stat->m_attribute.m_basetype;
        const auto declared_type = p->m_type->m_attribute.m_basetype;

        // Early return for exact type matches
        if (actual_type == declared_type)
        {
            return;
        }

        // Handle pointer type compatibility
        const bool is_null_pointer = actual_type == bt_ptr;
        const bool is_pointer_type = declared_type == bt_charptr ||
                                     declared_type == bt_intptr ||
                                     declared_type == bt_ptr;

        if (is_null_pointer && is_pointer_type)
        {
            return;
        }

        // Type mismatch error
        t_error(ret_type_mismatch, p->m_attribute);
    }

    // Ensure return statement type is valid
    void check_return(Return *p)
    {
        if (p->m_expr->m_attribute.m_basetype == bt_string)
        {
            this->t_error(ret_type_mismatch, p->m_attribute);
        }
    }

    // Validate procedure call arguments and return type
    void check_call(Call *p)
    {
        Symbol *sf, *sid;
        const char *id = lhs_to_id(p->m_lhs);
        const char *f = p->m_symname->spelling();
        // fprintf(stderr, "DEBUG: Looking up variable '%s' in scope %p\n", id, p->m_attribute.m_scope);

        if ((sid = m_st->lookup(id)) == 0)
        {
            this->t_error(var_undef, p->m_attribute);
        }
        if ((sf = m_st->lookup(f)) == 0)
        {
            this->t_error(proc_undef, p->m_attribute);
        }

        // Check number of arguments
        if (p->m_expr_list->size() != sf->m_arg_type.size())
        {
            this->t_error(narg_mismatch, p->m_attribute);
        }

        // Check argument types
        std::vector<Basetype>::iterator formal_args_iter = sf->m_arg_type.begin();
        for (std::list<Expr_ptr>::iterator iter = p->m_expr_list->begin(); iter != p->m_expr_list->end(); iter++, formal_args_iter++)
        {
            Basetype act_type = (*iter)->m_attribute.m_basetype;
            Basetype formal_type = *formal_args_iter;
            if (act_type != formal_type)
            {
                // Allow null to be passed to any pointer type
                if (act_type == bt_ptr && (formal_type == bt_intptr || formal_type == bt_charptr || formal_type == bt_ptr))
                {
                    continue; // This is valid
                }
                // Allow generic pointer to specific pointer types
                if ((act_type == bt_ptr && formal_type == bt_intptr) || (act_type == bt_ptr && formal_type == bt_charptr))
                {
                    continue; // This is valid
                }
                // Allow string to charptr conversion
                if (act_type == bt_string && formal_type == bt_charptr)
                {
                    continue; // This is valid
                }
                this->t_error(arg_type_mismatch, p->m_attribute);
            }
        }

        // Check return type compatibility with LHS - be more restrictive
        Basetype lhs_type = sid->m_basetype;
        Basetype return_type = sf->m_return_type;

        // Allow exact matches
        if (lhs_type == return_type)
        {
            return;
        }

        // Allow null (bt_ptr) to be assigned to any pointer type
        if (return_type == bt_ptr && (lhs_type == bt_intptr || lhs_type == bt_charptr || lhs_type == bt_ptr))
        {
            return;
        }

        // If we get here, the types are incompatible
        this->t_error(call_type_mismatch, p->m_attribute);
    }

    // For checking that this expressions type is boolean used in if/else
    void check_pred_if(Expr *p)
    {
        if (p->m_attribute.m_basetype != bt_boolean)
        {
            this->t_error(ifpred_err, p->m_attribute);
        }
    }

    // For checking that this expressions type is boolean used in while
    void check_pred_while(Expr *p)
    {
        if (p->m_attribute.m_basetype != bt_boolean)
        {
            this->t_error(whilepred_err, p->m_attribute);
        }
    }

    // Validate assignment type compatibility
    void check_assignment(Assignment *p)
    {
        Symbol *sf;
        const char *f = lhs_to_id(p->m_lhs);

        if ((sf = m_st->lookup(f)) == 0)
        {
            this->t_error(var_undef, p->m_attribute);
        }

        // Determine the actual type of the LHS based on its class
        Basetype lhs_type;

        // Check if this is a DerefVariable (like *ptr)
        DerefVariable *dv = dynamic_cast<DerefVariable *>(p->m_lhs);
        if (dv)
        {
            // For DerefVariable, the type is what the pointer points to
            if (sf->m_basetype == bt_intptr)
            {
                lhs_type = bt_integer;
            }
            else if (sf->m_basetype == bt_charptr)
            {
                lhs_type = bt_char;
            }
            else
            {
                // Invalid dereference - should have been caught elsewhere
                this->t_error(invalid_deref, p->m_attribute);
                return;
            }
        }
        // Check if this is an ArrayElement (like arr[i])
        else
        {
            ArrayElement *ae = dynamic_cast<ArrayElement *>(p->m_lhs);
            if (ae)
            {
                // For ArrayElement, if the array is a string, the element type is char
                if (sf->m_basetype == bt_string)
                {
                    lhs_type = bt_char;
                }
                else
                {
                    // Invalid array access - should have been caught elsewhere
                    this->t_error(no_array_var, p->m_attribute);
                    return;
                }
            }
            // Otherwise it's a regular Variable
            else
            {
                // For regular variables, use the symbol's base type
                lhs_type = sf->m_basetype;
            }
        }

        Basetype rhs_type = p->m_expr->m_attribute.m_basetype;

        if (lhs_type != rhs_type)
        {
            // Allow compatible pointer assignments
            if ((lhs_type == bt_intptr && rhs_type == bt_ptr) ||
                (lhs_type == bt_charptr && rhs_type == bt_ptr) ||
                (lhs_type == bt_ptr && rhs_type == bt_intptr) ||
                (lhs_type == bt_ptr && rhs_type == bt_charptr))
            {
                // These are valid assignments
                return;
            }
            // Allow null (bt_ptr) to be assigned to any pointer type
            else if (rhs_type == bt_ptr && (lhs_type == bt_intptr || lhs_type == bt_charptr || lhs_type == bt_ptr))
            {
                // This is valid
                return;
            }
            else
            {
                this->t_error(incompat_assign, p->m_attribute);
            }
        }
    }

    // Validate string assignment type compatibility
    void check_string_assignment(StringAssignment *p)
    {
        const char *name;
        Symbol *s;
        name = lhs_to_id(p->m_lhs);
        s = m_st->lookup(name);

        // Checks for variable undefined errors
        if (!s)
        {
            this->t_error(var_undef, p->m_attribute);
        }

        // Checks for incompatible assignment errors
        if (bt_string != s->m_basetype)
        {
            this->t_error(incompat_assign, p->m_attribute);
        }
    }

    // Validate array access operation
    void check_array_access(ArrayAccess *p)
    {

        if (p->m_expr->m_attribute.m_basetype != bt_integer)
        {
            this->t_error(array_index_error, p->m_attribute);
        }

        Symbol *array_sym = m_st->lookup(p->m_symname->spelling());
        if (!array_sym || array_sym->m_basetype != bt_string)
        {
            this->t_error(no_array_var, p->m_attribute);
        }

        p->m_attribute.m_basetype = bt_char;
    }

    // Validate array element access
    void check_array_element(ArrayElement *p)
    {
    }

    // For checking boolean operations(and, or ...)
    void checkset_boolexpr(Expr *parent, Expr *child1, Expr *child2)
    {

        if (child1->m_attribute.m_basetype != bt_boolean || child2->m_attribute.m_basetype != bt_boolean)
        {
            t_error(expr_type_err, parent->m_attribute);
        }
        parent->m_attribute.m_basetype = bt_boolean;
    }

    // For checking arithmetic expressions(plus, times, ...)
    void checkset_arithexpr(Expr *parent, Expr *child1, Expr *child2)
    {
        if (child1->m_attribute.m_basetype != bt_integer || child2->m_attribute.m_basetype != bt_integer)
        {
            t_error(expr_type_err, parent->m_attribute);
        }
        parent->m_attribute.m_basetype = bt_integer;
    }

    // Called by plus and minus: in these cases we allow pointer arithmetics
    void checkset_arithexpr_or_pointer(Expr *parent, Expr *child1, Expr *child2)
    {

        if (child2->m_attribute.m_basetype == bt_integer && child1->m_attribute.m_basetype == bt_integer)
        {
            parent->m_attribute.m_basetype = bt_integer;
        }
        else if (child2->m_attribute.m_basetype == bt_integer && child1->m_attribute.m_basetype == bt_charptr)
        {
            parent->m_attribute.m_basetype = bt_charptr;
        }
        else
        {
            t_error(expr_pointer_arithmetic_err, parent->m_attribute);
        }
    }

    // For checking relational(less than , greater than, ...)
    void checkset_relationalexpr(Expr *parent, Expr *child1, Expr *child2)
    {
        if (child1->m_attribute.m_basetype != bt_integer || child2->m_attribute.m_basetype != bt_integer)
        {
            t_error(expr_type_err, parent->m_attribute);
        }
        parent->m_attribute.m_basetype = bt_boolean;
    }

    // For checking equality ops(equal, not equal)
    void checkset_equalityexpr(Expr *parent, Expr *child1, Expr *child2)
    {
        Basetype type1 = child1->m_attribute.m_basetype;
        Basetype type2 = child2->m_attribute.m_basetype;

        if (are_equality_types_compatible(type1, type2))
        {
            parent->m_attribute.m_basetype = bt_boolean;
        }
        else
        {
            t_error(expr_type_err, parent->m_attribute);
        }
    }

    // For checking not
    void checkset_not(Expr *parent, Expr *child)
    {
        if (child->m_attribute.m_basetype == bt_boolean)
        {
            parent->m_attribute.m_basetype = bt_boolean;
        }
        else
        {
            t_error(expr_type_err, parent->m_attribute);
        }
    }

    // For checking unary minus
    void checkset_uminus(Expr *parent, Expr *child)
    {
        if (child->m_attribute.m_basetype == bt_integer)
        {
            parent->m_attribute.m_basetype = bt_integer;
        }
        else
        {
            t_error(expr_type_err, parent->m_attribute);
        }
    }

    void checkset_absolute_value(Expr *parent, Expr *child)
    {
        if (child->m_attribute.m_basetype == bt_integer || child->m_attribute.m_basetype == bt_string)
        {
            parent->m_attribute.m_basetype = bt_integer;
        }
        else
        {
            t_error(expr_abs_error, parent->m_attribute);
        }
    }

    void checkset_addressof(Expr *parent, Lhs *child)
    {
        const char *id = lhs_to_id(child);
        Symbol *s = m_st->lookup(id);
        if (!s)
        {
            t_error(var_undef, child->m_attribute);
        }

        // Check if this is an array element (like &y[5])
        ArrayElement *ae = dynamic_cast<ArrayElement *>(child);
        if (ae)
        {
            // This is &array[index] - should be valid for strings
            if (s->m_basetype == bt_string)
            {
                parent->m_attribute.m_basetype = bt_charptr;
                return;
            }
            else
            {
                t_error(expr_addressof_error, parent->m_attribute);
            }
        }

        // Check if this is a regular variable (like &x)
        Variable *v = dynamic_cast<Variable *>(child);
        if (v)
        {
            if (s->m_basetype == bt_integer)
            {
                parent->m_attribute.m_basetype = bt_intptr;
            }
            else if (s->m_basetype == bt_char)
            {
                parent->m_attribute.m_basetype = bt_charptr;
            }
            else
            {
                // Can't take address of whole string variable or other types
                t_error(expr_addressof_error, parent->m_attribute);
            }
            return;
        }

        // DerefVariable case - not allowed
        DerefVariable *dv = dynamic_cast<DerefVariable *>(child);
        if (dv)
        {
            t_error(expr_addressof_error, parent->m_attribute);
        }
    }

    void checkset_deref_expr(Deref *parent, Expr *child)
    {
        if (child->m_attribute.m_basetype == bt_intptr)
        {
            parent->m_attribute.m_basetype = bt_integer;
        }
        else if (child->m_attribute.m_basetype == bt_charptr)
        {
            parent->m_attribute.m_basetype = bt_char;
        }
        else
        {
            t_error(invalid_deref, parent->m_attribute);
        }
    }

    // Validate dereferenced variable type compatibility
    void checkset_deref_lhs(DerefVariable *p)
    {
        // Lookup symbol in symbol table
        const char *var_name = p->m_symname->spelling();
        Symbol *var_symbol = m_st->lookup(var_name);

        // Check if variable exists
        if (!var_symbol)
        {
            t_error(var_undef, p->m_attribute);
            return;
        }

        // Validate pointer type
        Basetype var_type = var_symbol->m_basetype;
        if (var_type != bt_charptr && var_type != bt_intptr)
        {
            t_error(invalid_deref, p->m_attribute);
            return;
        }

        // Set the attribute type
        p->m_attribute.m_basetype = var_type;
    }

    void checkset_variable(Variable *p)
    {
        if (!m_st->lookup(p->m_symname->spelling()))
        {
            this->t_error(var_undef, p->m_attribute);
        }
    }

public:
    Typecheck(FILE *errorfile, SymTab *st)
    {
        m_errorfile = errorfile;
        m_st = st;
    }

    void visitProgramImpl(ProgramImpl *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);

        check_for_one_main(p);
    }

    void visitProcImpl(ProcImpl *p)
    {
        // Create procedure symbol
        Symbol *proc_symbol = new Symbol();
        proc_symbol->m_basetype = bt_procedure;

        // Get procedure name
        const char *proc_name = p->m_symname->spelling();
        char *name_copy = strdup(proc_name);

        // Process parameter types 
        for (const auto &decl : *p->m_decl_list)
        {
            DeclImpl *param_decl = dynamic_cast<DeclImpl *>(decl);
            param_decl->m_type->accept(this);

            // Add each parameter's type to the procedure's argument list
            for (auto it = param_decl->m_symname_list->begin(); 
                 it != param_decl->m_symname_list->end(); 
                 ++it)
            {
                proc_symbol->m_arg_type.push_back(param_decl->m_type->m_attribute.m_basetype);
                
            }
        }

        // Set return type
        p->m_type->accept(this);
        proc_symbol->m_return_type = p->m_type->m_attribute.m_basetype;

        // Try to insert procedure into symbol table
        if (!m_st->insert(name_copy, proc_symbol))
        {
            delete proc_symbol;
            free(name_copy);
            t_error(dup_proc_name, p->m_attribute);
            return;
        }

        // Create new scope for procedure body
        m_st->open_scope();
        p->m_attribute.m_scope = m_st->get_scope();

        // Add parameters to new scope
        // int param_offset = 8;  // First parameter starts at 8(%ebp)
        for (const auto &decl : *p->m_decl_list)
        {
            decl->accept(this);

            // Now update the offsets for the parameters we just added
            // DeclImpl* param_decl = dynamic_cast<DeclImpl*>(decl);
            // for (auto it = param_decl->m_symname_list->begin(); 
            //     it != param_decl->m_symname_list->end(); 
            //     ++it)
            // {
            //     Symbol* param_sym = m_st->lookup((*it)->spelling());
            //     if (param_sym) {
            //         param_sym->set_offset(param_offset);
            //         param_offset += 4;  // Each parameter is 4 bytes
            //     }
            // }
        }

        // Process procedure body
        p->m_procedure_block->accept(this);

        // Clean up scope
        m_st->close_scope();

        // Validate procedure
        check_proc(p);

        // Clean up
        free(name_copy);
    }

    void visitCall(Call *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        // fprintf(stderr, "DEBUG: Processing call in scope %p\n", p->m_attribute.m_scope);
        default_rule(p);
        check_call(p);
    }

    void visitNested_blockImpl(Nested_blockImpl *p)
    {
        m_st->open_scope();
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        m_st->close_scope();
    }

    void visitProcedure_blockImpl(Procedure_blockImpl *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        p->m_parent_attribute->m_basetype = p->m_attribute.m_basetype;
    }

    void visitDeclImpl(DeclImpl *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        // fprintf(stderr, "DEBUG: Processing declaration in scope %p\n", p->m_attribute.m_scope);
        add_decl_symbol(p);
    }

    void visitAssignment(Assignment *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        check_assignment(p);
    }

    void visitStringAssignment(StringAssignment *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        check_string_assignment(p);
    }

    void visitIdent(Ident *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        const char *name = exprCast(p);

        if (!m_st->lookup(name))
        {
            this->t_error(var_undef, p->m_attribute);
        }

        p->m_attribute.m_basetype = m_st->lookup(name)->m_basetype;
        p->m_parent_attribute->m_basetype = p->m_attribute.m_basetype;
    }

    void visitReturn(Return *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        p->m_attribute.m_basetype = p->m_expr->m_attribute.m_basetype;

        check_return(p);
    }

    void visitIfNoElse(IfNoElse *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        check_pred_if(p->m_expr);
    }

    void visitIfWithElse(IfWithElse *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        check_pred_if(p->m_expr);
    }

    void visitWhileLoop(WhileLoop *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        check_pred_while(p->m_expr);
    }

    void visitCodeBlock(CodeBlock *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
    }

    void visitTInteger(TInteger *p)
    {
        default_rule(p);
        p->m_attribute.m_basetype = bt_integer;
    }

    void visitTBoolean(TBoolean *p)
    {
        default_rule(p);
        p->m_attribute.m_basetype = bt_boolean;
    }

    void visitTCharacter(TCharacter *p)
    {
        default_rule(p);
        p->m_attribute.m_basetype = bt_char;
    }

    void visitTString(TString *p)
    {
        default_rule(p);
        p->m_attribute.m_basetype = bt_string;
    }

    void visitTCharPtr(TCharPtr *p)
    {
        default_rule(p);
        p->m_attribute.m_basetype = bt_charptr;
    }

    void visitTIntPtr(TIntPtr *p)
    {
        default_rule(p);
        p->m_attribute.m_basetype = bt_intptr;
    }

    void visitAnd(And *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);

        checkset_boolexpr(p, p->m_expr_1, p->m_expr_2);
    }

    void visitDiv(Div *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        // Check if any operand is a pointer - this should be error 18
        if (p->m_expr_1->m_attribute.m_basetype == bt_charptr || p->m_expr_1->m_attribute.m_basetype == bt_intptr || p->m_expr_1->m_attribute.m_basetype == bt_ptr ||
            p->m_expr_2->m_attribute.m_basetype == bt_charptr || p->m_expr_2->m_attribute.m_basetype == bt_intptr || p->m_expr_2->m_attribute.m_basetype == bt_ptr)
        {
            t_error(expr_pointer_arithmetic_err, p->m_attribute);
        }
        else
        {
            checkset_arithexpr(p, p->m_expr_1, p->m_expr_2);
        }
    }

    void visitCompare(Compare *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        checkset_equalityexpr(p, p->m_expr_1, p->m_expr_2);
    }

    void visitGt(Gt *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        checkset_relationalexpr(p, p->m_expr_1, p->m_expr_2);
    }

    void visitGteq(Gteq *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        checkset_relationalexpr(p, p->m_expr_1, p->m_expr_2);
    }

    void visitLt(Lt *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        checkset_relationalexpr(p, p->m_expr_1, p->m_expr_2);
    }

    void visitLteq(Lteq *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        checkset_relationalexpr(p, p->m_expr_1, p->m_expr_2);
    }

    void visitMinus(Minus *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);

        // Case 1: Both operands are integers - valid arithmetic
        if (p->m_expr_1->m_attribute.m_basetype == bt_integer && p->m_expr_2->m_attribute.m_basetype == bt_integer)
        {
            checkset_arithexpr(p, p->m_expr_1, p->m_expr_2);
        }
        // Case 2: Valid pointer arithmetic - only charptr - integer
        else if (p->m_expr_1->m_attribute.m_basetype == bt_charptr &&
                 p->m_expr_2->m_attribute.m_basetype == bt_integer)
        {
            checkset_arithexpr_or_pointer(p, p->m_expr_1, p->m_expr_2);
        }
        // Case 3: Invalid pointer arithmetic - anything else involving pointers
        else if (p->m_expr_1->m_attribute.m_basetype == bt_charptr || p->m_expr_1->m_attribute.m_basetype == bt_intptr || p->m_expr_1->m_attribute.m_basetype == bt_ptr ||
                 p->m_expr_2->m_attribute.m_basetype == bt_charptr || p->m_expr_2->m_attribute.m_basetype == bt_intptr || p->m_expr_2->m_attribute.m_basetype == bt_ptr)
        {
            t_error(expr_pointer_arithmetic_err, p->m_attribute);
        }
        // Case 4: Other type errors (like bool - char, etc.)
        else
        {
            t_error(expr_type_err, p->m_attribute);
        }
    }

    void visitNoteq(Noteq *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        checkset_equalityexpr(p, p->m_expr_1, p->m_expr_2);
    }

    void visitOr(Or *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        checkset_boolexpr(p, p->m_expr_1, p->m_expr_2);
    }

    void visitPlus(Plus *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);

        // Case 1: Both operands are integers - valid arithmetic
        if (p->m_expr_1->m_attribute.m_basetype == bt_integer && p->m_expr_2->m_attribute.m_basetype == bt_integer)
        {
            checkset_arithexpr(p, p->m_expr_1, p->m_expr_2);
        }
        // Case 2: Valid pointer arithmetic - only charptr + integer
        else if (p->m_expr_1->m_attribute.m_basetype == bt_charptr &&
                 p->m_expr_2->m_attribute.m_basetype == bt_integer)
        {
            checkset_arithexpr_or_pointer(p, p->m_expr_1, p->m_expr_2);
        }
        // Case 3: Invalid pointer arithmetic - anything else involving pointers
        else if (p->m_expr_1->m_attribute.m_basetype == bt_charptr || p->m_expr_1->m_attribute.m_basetype == bt_intptr || p->m_expr_1->m_attribute.m_basetype == bt_ptr ||
                 p->m_expr_2->m_attribute.m_basetype == bt_charptr || p->m_expr_2->m_attribute.m_basetype == bt_intptr || p->m_expr_2->m_attribute.m_basetype == bt_ptr)
        {
            t_error(expr_pointer_arithmetic_err, p->m_attribute);
        }
        // Case 4: Other type errors (like bool + char, etc.)
        else
        {
            t_error(expr_type_err, p->m_attribute);
        }
    }

    void visitTimes(Times *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        // Check if any operand is a pointer - this should be error 18
        if (p->m_expr_1->m_attribute.m_basetype == bt_charptr || p->m_expr_1->m_attribute.m_basetype == bt_intptr || p->m_expr_1->m_attribute.m_basetype == bt_ptr ||
            p->m_expr_2->m_attribute.m_basetype == bt_charptr || p->m_expr_2->m_attribute.m_basetype == bt_intptr || p->m_expr_2->m_attribute.m_basetype == bt_ptr)
        {
            t_error(expr_pointer_arithmetic_err, p->m_attribute);
        }
        else
        {
            checkset_arithexpr(p, p->m_expr_1, p->m_expr_2);
        }
    }

    void visitNot(Not *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        checkset_not(p, p->m_expr);
    }

    void visitUminus(Uminus *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        checkset_uminus(p, p->m_expr);
    }

    void visitArrayAccess(ArrayAccess *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        check_array_access(p);
    }

    void visitIntLit(IntLit *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        p->m_attribute.m_basetype = bt_integer;
    }

    void visitCharLit(CharLit *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        p->m_attribute.m_basetype = bt_char;
    }

    void visitBoolLit(BoolLit *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        p->m_attribute.m_basetype = bt_boolean;
    }

    void visitNullLit(NullLit *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        p->m_attribute.m_basetype = bt_ptr;
    }

    void visitAbsoluteValue(AbsoluteValue *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        checkset_absolute_value(p, p->m_expr);
    }

    void visitAddressOf(AddressOf *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        checkset_addressof(p, p->m_lhs);
    }

    void visitVariable(Variable *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);

        checkset_variable(p);
        p->m_attribute.m_basetype = m_st->lookup(p->m_symname->spelling())->m_basetype;
    }

    void visitDeref(Deref *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        checkset_deref_expr(p, p->m_expr);
    }

    void visitDerefVariable(DerefVariable *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        checkset_deref_lhs(p);
    }

    void visitArrayElement(ArrayElement *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
        default_rule(p);
        check_array_element(p);
    }

    // Special cases
    void visitPrimitive(Primitive *p) {}
    void visitSymName(SymName *p) {}
    void visitStringPrimitive(StringPrimitive *p) {}
};

void dopass_typecheck(Program_ptr ast, SymTab *st)
{
    Typecheck *typecheck = new Typecheck(stderr, st);
    ast->accept(typecheck); // Walk the tree with the visitor above
    delete typecheck;
}
