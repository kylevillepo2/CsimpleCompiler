#include <cassert>
#include <typeinfo>
#include <unordered_map>
#include <cstring>

#include "ast.hpp"
#include "symtab.hpp"
#include "primitive.hpp"



class Codegen : public Visitor
{
  private:
    FILE* m_outputfile;
    SymTab *m_st;

    // Basic size of a word (integers and booleans) in bytes
    static const int wordsize = 4;

    int label_count; // Access with new_label
    int string_count; // Access with new_string_label

    // Add a map to track string literals and their labels
    std::unordered_map<std::string, std::string> string_literal_map;

    // Helpers
    // This is used to get new unique labels (cleverly names label1, label2, ...)
    int new_label()
    {
        return label_count++;
    }

    // used to get new unique string labels
    int new_string_label()
    {
        return string_count++;
    }

    void set_text_mode()
    {
        fprintf(m_outputfile, ".text\n\n");
    }

    void set_data_mode()
    {
        fprintf(m_outputfile, ".data\n\n");
    }

    // PART 1:
    // 1) get arithmetic expressions on integers working:
    //  you wont really be able to run your code,
    //  but you can visually inspect it to see that the correct
    //  chains of opcodes are being generated.
    // 2) get procedure calls working:
    //  if you want to see at least a very simple program compile
    //  and link successfully against gcc-produced code, you
    //  need to get at least this far
    // 3) get boolean operation working
    //  before we can implement any of the conditional control flow
    //  stuff, we need to have booleans worked out.
    // 4) control flow:
    //  we need a way to have if-elses and while loops in our language.
    // 5) arrays: just like variables, but with an index

    // Hint: the symbol table has been augmented to track an offset
    //  with all of the symbols.  That offset can be used to figure
    //  out where in the activation record you should look for a particuar
    //  variable


    ///////////////////////////////////////////////////////////////////////////////
    //
    //  function_prologue
    //  function_epilogue
    //
    //  Together these two functions implement the callee-side of the calling
    //  convention.  A stack frame has the following layout:
    //
    //                         <- SP (before pre-call / after epilogue)
    //  high -----------------
    //       | actual arg 1  |
    //       |    ...        |
    //       | actual arg n  |
    //       -----------------
    //       |  Return Addr  |
    //       =================
    //       | temporary 1   | <- SP (when starting prologue)
    //       |    ...        |
    //       | temporary n   |
    //   low ----------------- <- SP (when done prologue)
    //
    //
    //              ||
    //              ||
    //             \  /
    //              \/
    //
    //
    //  The caller is responsible for placing the actual arguments
    //  and the return address on the stack. Actually, the return address
    //  is put automatically on the stack as part of the x86 call instruction.
    //
    //  On function entry, the callee
    //
    //  (1) allocates space for the callee's temporaries on the stack
    //
    //  (2) saves callee-saved registers (see below) - including the previous activation record pointer (%ebp)
    //
    //  (3) makes the activation record pointer (frmae pointer - %ebp) point to the start of the temporary region
    //
    //  (4) possibly copies the actual arguments into the temporary variables to allow easier access
    //
    //  On function exit, the callee:
    //
    //  (1) pops the callee's activation record (temporay area) off the stack
    //
    //  (2) restores the callee-saved registers, including the activation record of the caller (%ebp)
    //
    //  (3) jumps to the return address (using the x86 "ret" instruction, this automatically pops the
    //      return address off the stack
    //
    //////////////////////////////////////////////////////////////////////////////
    //
    // Since we are interfacing with code produced by GCC, we have to respect the
    // calling convention that GCC demands:
    //
    // Contract between caller and callee on x86:
    //    * after call instruction:
    //           o %eip points at first instruction of function
    //           o %esp+4 points at first argument
    //           o %esp points at return address
    //    * after ret instruction:
    //           o %eip contains return address
    //           o %esp points at arguments pushed by caller
    //           o called function may have trashed arguments
    //           o %eax contains return value (or trash if function is void)
    //           o %ecx, %edx may be trashed
    //           o %ebp, %ebx, %esi, %edi must contain contents from time of call
    //    * Terminology:
    //           o %eax, %ecx, %edx are "caller save" registers
    //           o %ebp, %ebx, %esi, %edi are "callee save" registers
    ////////////////////////////////////////////////////////////////////////////////


    void emit_prologue(SymName *name, unsigned int size_locals, unsigned int num_args)
    {
        // Make the function globally visible and label it
        fprintf(m_outputfile, ".globl %s\n", name->spelling());
        fprintf(m_outputfile, "%s:\n", name->spelling());

        // Save old base pointer and set up new stack frame
        fprintf(m_outputfile, "\tpushl\t%%ebp\n");
        fprintf(m_outputfile, "\tmovl\t%%esp, %%ebp\n");

        

        // Allocate space for local variables if needed
        if (size_locals > 0) {
            fprintf(m_outputfile, "\tsubl\t$%u, %%esp\n", size_locals);
        }

        // Copy arguments from calling convention locations to local stack
        for(unsigned int i = 0; i < num_args; i++) {
            int arg_offset = 8 + (i * 4);  // First arg at %ebp + 20, second at %ebp + 24, etc.
            int local_offset = -4 * (i + 1);  // Local variables start at %ebp - 4
            fprintf(m_outputfile, "\tmovl\t%d(%%ebp), %%eax\n", arg_offset);
            fprintf(m_outputfile, "\tmovl\t%%eax, %d(%%ebp)\n", local_offset);
        }

        // Save callee-saved registers
        fprintf(m_outputfile, "\tpushl\t%%ebx\n");
        fprintf(m_outputfile, "\tpushl\t%%esi\n");
        fprintf(m_outputfile, "\tpushl\t%%edi\n");

        // for Main, add exit point for error handling
        if (strcmp(name->spelling(), "Main") == 0) {
            fprintf(m_outputfile, "\tjmp\tmain_start\n");
            fprintf(m_outputfile, "program_exit:\n");
            fprintf(m_outputfile, "\tmovl\t$0, %%eax\n");  // Set exit code to 0
            emit_epilogue();
            fprintf(m_outputfile, "main_start:\n");
        }
    }

    void emit_epilogue()
    {

        // Restore callee-saved registers
        fprintf(m_outputfile, "\tpopl\t%%edi\n");
        fprintf(m_outputfile, "\tpopl\t%%esi\n");
        fprintf(m_outputfile, "\tpopl\t%%ebx\n");

        // restore stack and base pointer, then return
        fprintf(m_outputfile, "\tmovl\t%%ebp, %%esp\n");
        fprintf(m_outputfile, "\tpopl\t%%ebp\n");
        fprintf(m_outputfile, "\tret\n");
    }

  // WRITEME: more functions to emit code

  public:

    Codegen(FILE* outputfile, SymTab* st)
    {
        m_outputfile = outputfile;
        m_st = st;
        label_count = 0;
    }

    void visitProgramImpl(ProgramImpl* p)
    {
        set_text_mode();
        p->visit_children(this);
    }

    void visitProcImpl(ProcImpl* p)
    {
        // Lookup the procedure symbol in the symbol table
        Symbol* proc_symbol = m_st->lookup(p->m_symname->spelling());
        unsigned int num_args = 0;
        if (proc_symbol) {
            num_args = proc_symbol->m_arg_type.size();
        }



        // 1. Emit the prologue (sets up stack frame, allocates locals)
        emit_prologue(p->m_symname, m_st->scopesize(p->m_attribute.m_scope), num_args);

       

        // 3. Generate code for the function body
        p->visit_children(this);


        // 5. Emit the epilogue (restores stack and returns)
        emit_epilogue();

    
    }

    void visitProcedure_blockImpl(Procedure_blockImpl* p)
    {
        p->visit_children(this);
    }

    void visitNested_blockImpl(Nested_blockImpl* p)
    {
        p->visit_children(this);
    }

    void visitAssignment(Assignment* p)
    {
        // Visit the right-hand side expression first
        p->m_expr->accept(this);
        
        // Cast Lhs to Variable to access m_symname
        Variable* var = dynamic_cast<Variable*>(p->m_lhs);
        if (var) {
            const char* var_name = var->m_symname->spelling();
            
            // Look up the variable in the symbol table
            Symbol* sym = m_st->lookup(p->m_attribute.m_scope, var_name);
            if (sym) {
                int offset = sym->get_offset();
                // Pop the value into %eax
                fprintf(m_outputfile, "\tpopl\t%%eax\n");
                // Store the value at the variable's offset from %ebp
                fprintf(m_outputfile, "\tmovl\t%%eax, %d(%%ebp)\n", -offset);
            }
        } else if (ArrayElement* arr_elem = dynamic_cast<ArrayElement*>(p->m_lhs)) {
            arr_elem->accept(this);
        }
        else {
            p->m_lhs->accept(this);
        }
    }

    void visitCall(Call* p)
    {
        // Save caller-saved registers
        fprintf(m_outputfile, "\tpushl\t%%eax\n");
        fprintf(m_outputfile, "\tpushl\t%%ecx\n");
        fprintf(m_outputfile, "\tpushl\t%%edx\n");

        // 1. Evaluate and push arguments (right-to-left)
        // (Assume p->m_expr_list is a list of argument expressions)
        std::vector<Expr_ptr> args(p->m_expr_list->begin(), p->m_expr_list->end());
        for (auto it = args.rbegin(); it != args.rend(); ++it) {
            (*it)->accept(this); // This should push the argument value onto the stack
        }

        // 3. Call the function
        fprintf(m_outputfile, "\tcall\t%s\n", p->m_symname->spelling());

        // 4. Clean up the stack (remove arguments)
        if (!args.empty()) {
            fprintf(m_outputfile, "\taddl\t$%lu, %%esp\n", args.size() * 4);
        }

        // After the call, the return value is in %eax
        if (p->m_lhs) {
            // Assume LHS is a simple variable for now
            Variable* var = dynamic_cast<Variable*>(p->m_lhs);
            if (var) {
                const char* var_name = var->m_symname->spelling();
                // Look up the variable in the typecheck scope
                Symbol* sym = m_st->lookup(p->m_attribute.m_scope, var_name);
                if (sym) {
                    int offset = sym->get_offset();
                    fprintf(m_outputfile, "\tmovl\t%%eax, %d(%%ebp)\n", -offset);
                }
            }
            // (Handle other LHS types if needed)
        }


        // Restore caller-saved registers 
        fprintf(m_outputfile, "\tpopl\t%%edx\n");
        fprintf(m_outputfile, "\tpopl\t%%ecx\n");
        fprintf(m_outputfile, "\tpopl\t%%eax\n");
    }

    void visitReturn(Return* p)
    {
        p->visit_children(this);
        fprintf(m_outputfile, "\tpopl\t%%eax\n");
    }

    // Control flow
    void visitIfNoElse(IfNoElse* p)
    {
        // Generate unique labels for this if statement
        int end_label = new_label();
        
        // Visit the condition first
        p->m_expr->accept(this);
        
        // Pop the condition result and compare with 0
        fprintf(m_outputfile, "\tpopl\t%%eax\n");
        fprintf(m_outputfile, "\tcmpl\t$0, %%eax\n");
        
        // Jump to end if condition is false (0)
        fprintf(m_outputfile, "\tje\tlabel%d\n", end_label);
        
        // Visit the body of the if statement
        p->m_nested_block->accept(this);
        
        // Label for the end of the if statement
        fprintf(m_outputfile, "label%d:\n", end_label);
    }

    void visitIfWithElse(IfWithElse* p)
    {
        // Generate unique labels for this if-else statement
        int else_label = new_label();
        int end_label = new_label();
        
        // Visit the condition first
        p->m_expr->accept(this);
        
        // Pop the condition result and compare with 0
        fprintf(m_outputfile, "\tpopl\t%%eax\n");
        fprintf(m_outputfile, "\tcmpl\t$0, %%eax\n");
        
        // Jump to else if condition is false (0)
        fprintf(m_outputfile, "\tje\tlabel%d\n", else_label);
        
        // Visit the if body
        p->m_nested_block_1->accept(this);
        
        // Jump to end after if body
        fprintf(m_outputfile, "\tjmp\tlabel%d\n", end_label);
        
        // Label for the else branch
        fprintf(m_outputfile, "label%d:\n", else_label);
        
        // Visit the else body
        p->m_nested_block_2->accept(this);
        
        // Label for the end of the if-else statement
        fprintf(m_outputfile, "label%d:\n", end_label);
    }

    void visitWhileLoop(WhileLoop* p)
    {
        // Generate unique labels for this while loop
        int start_label = new_label();
        int end_label = new_label();
        
        // Label for the start of the loop (condition check)
        fprintf(m_outputfile, "label%d:\n", start_label);
        
        // Visit the condition
        p->m_expr->accept(this);
        
        // Pop the condition result and compare with 0
        fprintf(m_outputfile, "\tpopl\t%%eax\n");
        fprintf(m_outputfile, "\tcmpl\t$0, %%eax\n");
        
        // Jump to end if condition is false (0)
        fprintf(m_outputfile, "\tje\tlabel%d\n", end_label);
        
        // Visit the loop body
        p->m_nested_block->accept(this);
        
        // Jump back to condition check
        fprintf(m_outputfile, "\tjmp\tlabel%d\n", start_label);
        
        // Label for the end of the while loop
        fprintf(m_outputfile, "label%d:\n", end_label);
    }

    void visitCodeBlock(CodeBlock *p) 
    {
        p->visit_children(this);
    }

    // Variable declarations (no code generation needed)
    void visitDeclImpl(DeclImpl* p)
    {
        p->visit_children(this);
    }

    void visitTInteger(TInteger* p)
    {
        p->visit_children(this);
    }

    void visitTIntPtr(TIntPtr* p)
    {
        p->visit_children(this);
    }

    void visitTBoolean(TBoolean* p)
    {
        // Visit any children of the boolean type node
        p->visit_children(this);
    }

    void visitTCharacter(TCharacter* p)
    {
        p->visit_children(this);
    }

    void visitTCharPtr(TCharPtr* p)
    {
        p->visit_children(this);
    }

    void visitTString(TString* p)
    {
        p->visit_children(this);
    }

    // Comparison operations
    void visitCompare(Compare* p)
    {
        // Visit both operands
        p->visit_children(this);
        
        // Pop the second operand into %ebx
        fprintf(m_outputfile, "\tpopl\t%%ebx\n");
        // Pop the first operand into %eax
        fprintf(m_outputfile, "\tpopl\t%%eax\n");
        
        // Compare operands (eax == ebx)
        fprintf(m_outputfile, "\tcmpl\t%%ebx, %%eax\n");

        fprintf(m_outputfile, "\tmovl\t$0, %%eax\n");
        
        // Set %al to 1 if equal, 0 otherwise
        fprintf(m_outputfile, "\tsete\t%%al\n");
        
        // Push the result (0 or 1) onto the stack
        fprintf(m_outputfile, "\tpushl\t%%eax\n");
    }

    void visitNoteq(Noteq* p)
    {
        // Visit both operands
        p->visit_children(this);
        
        // Pop the second operand into %ebx
        fprintf(m_outputfile, "\tpopl\t%%ebx\n");
        // Pop the first operand into %eax
        fprintf(m_outputfile, "\tpopl\t%%eax\n");
        
        // Compare operands (eax != ebx)
        fprintf(m_outputfile, "\tcmpl\t%%ebx, %%eax\n");

        fprintf(m_outputfile, "\tmovl\t$0, %%eax\n");
        
        // Set %al to 1 if not equal, 0 otherwise
        fprintf(m_outputfile, "\tsetne\t%%al\n");
        
        // Push the result (0 or 1) onto the stack
        fprintf(m_outputfile, "\tpushl\t%%eax\n");
    }

    void visitGt(Gt* p)
    {
        // Visit both operands
        p->visit_children(this);
        
        // Pop the second operand into %ebx
        fprintf(m_outputfile, "\tpopl\t%%ebx\n");
        // Pop the first operand into %eax
        fprintf(m_outputfile, "\tpopl\t%%eax\n");
        
        // Compare operands (eax > ebx)
        fprintf(m_outputfile, "\tcmpl\t%%ebx, %%eax\n");

        fprintf(m_outputfile, "\tmovl\t$0, %%eax\n");
        
        // Set %al to 1 if greater than, 0 otherwise
        fprintf(m_outputfile, "\tsetg\t%%al\n");
        
        // Push the result (0 or 1) onto the stack
        fprintf(m_outputfile, "\tpushl\t%%eax\n");
    }

    void visitGteq(Gteq* p)
    {
        // Visit both operands
        p->visit_children(this);
        
        // Pop the second operand into %ebx
        fprintf(m_outputfile, "\tpopl\t%%ebx\n");
        // Pop the first operand into %eax
        fprintf(m_outputfile, "\tpopl\t%%eax\n");
        
        // Compare operands (eax >= ebx)
        fprintf(m_outputfile, "\tcmpl\t%%ebx, %%eax\n");

         fprintf(m_outputfile, "\tmovl\t$0, %%eax\n");
        
        // Set %al to 1 if greater than or equal, 0 otherwise
        fprintf(m_outputfile, "\tsetge\t%%al\n");

        
        // Push the result (0 or 1) onto the stack
        fprintf(m_outputfile, "\tpushl\t%%eax\n");
    }

    void visitLt(Lt* p)
    {
        // Visit both operands
        p->visit_children(this);
        
        // Pop the second operand into %ebx
        fprintf(m_outputfile, "\tpopl\t%%ebx\n");
        // Pop the first operand into %eax
        fprintf(m_outputfile, "\tpopl\t%%eax\n");
        
        // Compare operands (eax < ebx)
        fprintf(m_outputfile, "\tcmpl\t%%ebx, %%eax\n");

         fprintf(m_outputfile, "\tmovl\t$0, %%eax\n");
        
        // Set %al to 1 if less than, 0 otherwise
        fprintf(m_outputfile, "\tsetl\t%%al\n");
        
        // Push the result (0 or 1) onto the stack
        fprintf(m_outputfile, "\tpushl\t%%eax\n");
    }

    void visitLteq(Lteq* p)
    {
        // Visit both operands
        p->visit_children(this);
        
        // Pop the second operand into %ebx
        fprintf(m_outputfile, "\tpopl\t%%ebx\n");
        // Pop the first operand into %eax
        fprintf(m_outputfile, "\tpopl\t%%eax\n");
        
        // Compare operands (eax <= ebx)
        fprintf(m_outputfile, "\tcmpl\t%%ebx, %%eax\n");

         fprintf(m_outputfile, "\tmovl\t$0, %%eax\n");
        
        // Set %al to 1 if less than or equal, 0 otherwise
        fprintf(m_outputfile, "\tsetle\t%%al\n");
        
        // Push the result (0 or 1) onto the stack
        fprintf(m_outputfile, "\tpushl\t%%eax\n");
    }

    // Arithmetic and logic operations
    void visitAnd(And* p)
    {
        // Visit both operands
        p->visit_children(this);
        
        // Pop the second operand into %ebx
        fprintf(m_outputfile, "\tpopl\t%%ebx\n");
        // Pop the first operand into %eax
        fprintf(m_outputfile, "\tpopl\t%%eax\n");
        
        // Perform logical AND (eax && ebx)
        fprintf(m_outputfile, "\tandl\t%%ebx, %%eax\n");
        
        // Push the result (0 or 1) onto the stack
        fprintf(m_outputfile, "\tpushl\t%%eax\n");
    }

    void visitOr(Or* p)
    {
        // Visit both operands
        p->visit_children(this);
        
        // Pop the second operand into %ebx
        fprintf(m_outputfile, "\tpopl\t%%ebx\n");
        // Pop the first operand into %eax
        fprintf(m_outputfile, "\tpopl\t%%eax\n");
        
        // Perform logical OR (eax || ebx)
        fprintf(m_outputfile, "\torl\t%%ebx, %%eax\n");
        
        // Push the result (0 or 1) onto the stack
        fprintf(m_outputfile, "\tpushl\t%%eax\n");
    }

    void visitMinus(Minus* p)
    {
        p->visit_children(this);
        fprintf(m_outputfile, "\tpopl\t%%ebx\n");
        fprintf(m_outputfile, "\tpopl\t%%eax\n");
        fprintf(m_outputfile, "\tsubl\t%%ebx, %%eax\n");
        fprintf(m_outputfile, "\tpushl\t%%eax\n");

    }

    void visitPlus(Plus* p)
    {
        p->visit_children(this);
        fprintf(m_outputfile, "\tpopl\t%%ebx\n");
        fprintf(m_outputfile, "\tpopl\t%%eax\n");
        fprintf(m_outputfile, "\taddl\t%%ebx, %%eax\n");
        fprintf(m_outputfile, "\tpushl\t%%eax\n");
    }

    void visitTimes(Times* p)
    {
        p->visit_children(this);
        fprintf(m_outputfile, "\tpopl\t%%ebx\n");
        fprintf(m_outputfile, "\tpopl\t%%eax\n");
        fprintf(m_outputfile, "\timul\t%%ebx, %%eax\n");
        fprintf(m_outputfile, "\tpushl\t%%eax\n");
    }

    void visitDiv(Div* p)
    {
        p->visit_children(this);
        fprintf(m_outputfile, "\tpopl\t%%ebx\n");
        fprintf(m_outputfile, "\tpopl\t%%eax\n");
        fprintf(m_outputfile, "\tcdq\n");
        fprintf(m_outputfile, "\tidiv\t%%ebx\n");
        fprintf(m_outputfile, "\tpushl\t%%eax\n");
    }

    void visitNot(Not* p)
    {
        // Visit the operand
        p->visit_children(this);
        
        // Pop the operand into %eax
        fprintf(m_outputfile, "\tpopl\t%%eax\n");
        
        // Compare with 0 to get the opposite boolean value
        fprintf(m_outputfile, "\tcmpl\t$0, %%eax\n");

        fprintf(m_outputfile, "\tmovl\t$0, %%eax\n");
        
        // Set %al to 1 if equal to 0 (original was false), 0 otherwise
        fprintf(m_outputfile, "\tsete\t%%al\n");
        
        // Push the result (0 or 1) onto the stack
        fprintf(m_outputfile, "\tpushl\t%%eax\n");
    }

    void visitUminus(Uminus* p)
    {
        p->visit_children(this);
        fprintf(m_outputfile, "\tpopl\t%%eax\n");
        fprintf(m_outputfile, "\tnegl\t%%eax\n");
        fprintf(m_outputfile, "\tpushl\t%%eax\n");
    }

    // Variable and constant access
    void visitIdent(Ident* p)
    {
        // Get the variable name
        const char* var_name = p->m_symname->spelling();
        
        // Look up the variable in the symbol table
        Symbol* sym = m_st->lookup(p->m_attribute.m_scope, var_name);
        if (sym) {
            int offset = sym->get_offset();
            // Load the value from the variable's offset from %ebp
            fprintf(m_outputfile, "\tmovl\t%d(%%ebp), %%eax\n", -offset);
            // Push the value onto the stack
            fprintf(m_outputfile, "\tpushl\t%%eax\n");
            return;
        }
        else {
            fprintf(stderr, "Error: Variable %s not found in scope\n", var_name);
            return;
        }
    }

    void visitBoolLit(BoolLit* p)
    {
        p->visit_children(this);
        // Push 1 for true, 0 for false onto the stack
        fprintf(m_outputfile, "\tpushl\t$0x%x\n", p->m_primitive->m_data);
    }

    void visitCharLit(CharLit* p)
    {
         p->visit_children(this);
        fprintf(m_outputfile, "\tpushl\t$0x%x\n", p->m_primitive->m_data);
    }

    void visitIntLit(IntLit* p)
    {
        p->visit_children(this);
        fprintf(m_outputfile, "\tpushl\t$0x%x\n", p->m_primitive->m_data);
    }

    void visitNullLit(NullLit* p)
    {
        p->visit_children(this);
        fprintf(m_outputfile, "\tpushl\t$0\n");
    }

    void visitArrayAccess(ArrayAccess* p)
    {
        // First evaluate the index expression
        p->m_expr->accept(this);

        const char* var_name = p->m_symname->spelling();
        Symbol* sym = m_st->lookup(p->m_attribute.m_scope, var_name);
        if (!sym) {
            fprintf(stderr, "Error: Array %s not found in scope\n", var_name);
            fprintf(m_outputfile, "\tpopl\t%%eax\n");  // Clean up stack
            fprintf(m_outputfile, "\tjmp\tprogram_exit\n");
            return;
        }

        int offset = sym->get_offset();
        int bounds_ok_label = new_label();
        int end_label = new_label();

        // Pop the index into eax
        fprintf(m_outputfile, "\tpopl\t%%eax\n");

        // Bounds checking
        fprintf(m_outputfile, "\tcmpl\t$0, %%eax\n");
        fprintf(m_outputfile, "\tjl\tprogram_exit\n");  // Exit if negative index

        // For string arrays, check against string size
        if (sym->m_basetype == bt_string) {
            fprintf(m_outputfile, "\tcmpl\t$%d, %%eax\n", sym->m_string_size);
            fprintf(m_outputfile, "\tjge\tprogram_exit\n");  // Exit if index >= size
        }

        // Calculate array element address
        fprintf(m_outputfile, "\tleal\t%d(%%ebp), %%ebx\n", -1*offset);
        fprintf(m_outputfile, "\taddl\t%%eax, %%ebx\n");

        // Load the value based on array type
        if (sym->m_basetype == bt_integer) {
            fprintf(m_outputfile, "\tmovl\t(%%ebx), %%eax\n");  // Load word for integers
        } else {
            fprintf(m_outputfile, "\tmovzbl\t(%%ebx), %%eax\n");  // Load byte for chars/strings
        }

        // Push the loaded value
        fprintf(m_outputfile, "\tpushl\t%%eax\n");
    }

    // LHS
    void visitVariable(Variable* p)
    {
        p->visit_children(this);
    }

    void visitDerefVariable(DerefVariable* p)
    {
        // For assignment: value to store is on stack
        const char* var_name = p->m_symname->spelling();
        Symbol* sym = m_st->lookup(p->m_attribute.m_scope, var_name);
        
        if (!sym) {
            fprintf(stderr, "Error: Pointer variable %s not found in scope\n", var_name);
            fprintf(m_outputfile, "\tjmp\tprogram_exit\n");
            return;
        }

        // Get the pointer value from the variable
        int offset = sym->get_offset();
        // Pop value to store into %eax
        fprintf(m_outputfile, "\tpopl\t%%eax\n");
        // Load pointer value into %ebx
        fprintf(m_outputfile, "\tmovl\t%d(%%ebp), %%ebx\n", -offset);
        // Check for null pointer
        fprintf(m_outputfile, "\tcmpl\t$0, %%ebx\n");
        fprintf(m_outputfile, "\tje\tprogram_exit\n");
        // Store value at address pointed to by pointer
        if (sym->m_basetype == bt_intptr) {
            // Integer pointer: store word
            fprintf(m_outputfile, "\tmovl\t%%eax, (%%ebx)\n");
        } else if (sym->m_basetype == bt_charptr) {
            // Char pointer: store byte
            fprintf(m_outputfile, "\tmovb\t%%al, (%%ebx)\n");
        } else {
            fprintf(stderr, "Error: Invalid pointer type for dereference assignment\n");
            fprintf(m_outputfile, "\tjmp\tprogram_exit\n");
        }
    }

    void visitArrayElement(ArrayElement* p)
    {
        // First evaluate the index expression
        p->m_expr->accept(this);
        
        const char* var_name = p->m_symname->spelling();
        Symbol* sym = m_st->lookup(p->m_attribute.m_scope, var_name);
        if (!sym) {
            fprintf(stderr, "Error: Array %s not found in scope\n", var_name);
            fprintf(m_outputfile, "\tpopl\t%%eax\n");  // Clean up stack
            fprintf(m_outputfile, "\tjmp\tprogram_exit\n");
            return;
        }
        
        int offset = sym->get_offset();
        
        // Pop the index into eax
        fprintf(m_outputfile, "\tpopl\t%%eax\n");
        // Pop the value to store into %ebx (so %bl is correct for char)
        fprintf(m_outputfile, "\tpopl\t%%ebx\n");
        
        // Bounds checking
        fprintf(m_outputfile, "\tcmpl\t$0, %%eax\n");
        fprintf(m_outputfile, "\tjl\tprogram_exit\n");  // Exit if negative index
        
        // For string arrays, check against string size
        if (sym->m_basetype == bt_string) {
            fprintf(m_outputfile, "\tcmpl\t$%d, %%eax\n", sym->m_string_size);
            fprintf(m_outputfile, "\tjge\tprogram_exit\n");  // Exit if index >= size
        }
        
        // Calculate array element address
        fprintf(m_outputfile, "\tleal\t%d(%%ebp), %%ecx\n", -1*offset);
        fprintf(m_outputfile, "\taddl\t%%eax, %%ecx\n");
        
        // Store the value based on array type
        if (sym->m_basetype == bt_integer) {
            fprintf(m_outputfile, "\tmovl\t%%ebx, (%%ecx)\n");  // Store word for integers
        } else {
            fprintf(m_outputfile, "\tmovb\t%%bl, (%%ecx)\n");  // Store byte for chars/strings
        }
    }

    // Special cases
    void visitSymName(SymName* p)
    {
        
    }

    void visitPrimitive(Primitive* p)
    {
        
    }

    // Strings
    void visitStringAssignment(StringAssignment* p)
    {
        // Get the destination variable and its info
        Variable* var = dynamic_cast<Variable*>(p->m_lhs);
        if (!var) {
            fprintf(stderr, "Error: Invalid LHS for string assignment\n");
            fprintf(m_outputfile, "\tjmp\tprogram_exit\n");
            return;
        }

        const char* var_name = var->m_symname->spelling();
        Symbol* dest_sym = m_st->lookup(var->m_attribute.m_scope, var_name);
        if (!dest_sym) {
            fprintf(stderr, "Error: String variable %s not found in scope\n", var_name);
            fprintf(m_outputfile, "\tjmp\tprogram_exit\n");
            return;
        }

        int dest_offset = dest_sym->get_offset();
        unsigned int array_size = dest_sym->m_string_size;
        
        // Get the source string's length (including null terminator)
        unsigned int src_length = strlen(p->m_stringprimitive->m_string) + 1;
        
        // Check if the string fits in the destination array
        if (src_length > array_size) {
            fprintf(stderr, "Error: String literal \"%s\" (length %d) too large for destination array (size %d)\n", 
                    p->m_stringprimitive->m_string, src_length, array_size);
            fprintf(m_outputfile, "\tjmp\tprogram_exit\n");
            return;
        }
        
        // Emit the string literal in the .data section with a unique label
        set_data_mode();
        int str_label = new_string_label();
        fprintf(m_outputfile, "string%d:\n", str_label);
        fprintf(m_outputfile, "\t.asciz\t\"%s\"\n", p->m_stringprimitive->m_string);
        set_text_mode();
        
        // Use rep movsb for efficient string copy
        fprintf(m_outputfile, "\tmovl\t$string%d, %%esi\n", str_label);  // Source
        fprintf(m_outputfile, "\tleal\t%d(%%ebp), %%edi\n", -1*dest_offset);  // Destination
        fprintf(m_outputfile, "\tmovl\t$%d, %%ecx\n", src_length);  // Length
        fprintf(m_outputfile, "\tcld\n");  // Clear direction flag (forward copy)
        fprintf(m_outputfile, "\trep movsb\n");  // Copy string
    }

    void visitStringPrimitive(StringPrimitive* p)
    {
        // Check if we've already emitted this string literal
        std::string str = p->m_string;
        auto it = string_literal_map.find(str);
        if (it != string_literal_map.end()) {
            // String already exists, just push its address
            fprintf(m_outputfile, "\tpushl\t$%s\n", it->second.c_str());
            return;
        }

        // Generate a new label for this string
        int str_label = new_string_label();
        std::string label_name = "string" + std::to_string(str_label);
        string_literal_map[str] = label_name;

        // Emit the string in the data section
        set_data_mode();
        fprintf(m_outputfile, "%s:\n", label_name.c_str());
        fprintf(m_outputfile, "\t.asciz\t\"%s\"\n", str.c_str());
        set_text_mode();

        // Push the address of the string
        fprintf(m_outputfile, "\tpushl\t$%s\n", label_name.c_str());
    }

    void visitAbsoluteValue(AbsoluteValue* p)
    {
        // If the operand is a string variable
        Variable* var = dynamic_cast<Variable*>(p->m_expr);
        Ident* ident = dynamic_cast<Ident*>(p->m_expr);
        if (var || ident) {
            const char* var_name = var ? var->m_symname->spelling() : ident->m_symname->spelling();
            Symbol* sym = m_st->lookup(p->m_attribute.m_scope, var_name);
            if (sym && sym->m_basetype == bt_string) {
                fprintf(m_outputfile, "\tpushl\t$%d\n", sym->m_string_size);
                return;
            }
        }
        // Otherwise, handle as integer
        p->m_expr->accept(this);
        
        // Pop the value into eax
        fprintf(m_outputfile, "\tpopl\t%%eax\n");
        
        // Generate labels for the conditional
        int positive_label = new_label();
        int end_label = new_label();
        
        // Check if value is negative
        fprintf(m_outputfile, "\tcmpl\t$0, %%eax\n");
        fprintf(m_outputfile, "\tjge\tlabel%d\n", positive_label);  // Skip negation if positive
        
        // Negate the value if negative
        fprintf(m_outputfile, "\tnegl\t%%eax\n");
        
        // Label for positive values
        fprintf(m_outputfile, "label%d:\n", positive_label);
        
        // Push the result
        fprintf(m_outputfile, "\tpushl\t%%eax\n");
    }

    // Pointer
    void visitAddressOf(AddressOf* p)
    {
        // Handle address of variable
        if (Variable* var = dynamic_cast<Variable*>(p->m_lhs)) {
            const char* var_name = var->m_symname->spelling();
            Symbol* sym = m_st->lookup(var->m_attribute.m_scope, var_name);
            if (!sym) {
                fprintf(stderr, "Error: Variable %s not found in scope\n", var_name);
                fprintf(m_outputfile, "\tjmp\tprogram_exit\n");
                return;
            }
            int offset = sym->get_offset();
            fprintf(m_outputfile, "\tleal\t%d(%%ebp), %%eax\n", -1*offset);
            fprintf(m_outputfile, "\tpushl\t%%eax\n");
        }
        // Handle address of array element
        else if (ArrayElement* arr_elem = dynamic_cast<ArrayElement*>(p->m_lhs)) {
            // First evaluate the index expression
            arr_elem->m_expr->accept(this);
            
            const char* var_name = arr_elem->m_symname->spelling();
            Symbol* sym = m_st->lookup(arr_elem->m_attribute.m_scope, var_name);
            if (!sym) {
                fprintf(stderr, "Error: Array %s not found in scope\n", var_name);
                fprintf(m_outputfile, "\tpopl\t%%eax\n");  // Clean up stack
                fprintf(m_outputfile, "\tjmp\tprogram_exit\n");
                return;
            }
            
            int offset = sym->get_offset();
            
            // Pop the index into eax
            fprintf(m_outputfile, "\tpopl\t%%eax\n");
            
            // Calculate the address: base + index
            fprintf(m_outputfile, "\tleal\t%d(%%ebp), %%ebx\n", -1*offset);
            fprintf(m_outputfile, "\taddl\t%%eax, %%ebx\n");
            fprintf(m_outputfile, "\tpushl\t%%ebx\n");
        }
        // Handle address of dereferenced variable
        else if (DerefVariable* deref = dynamic_cast<DerefVariable*>(p->m_lhs)) {
            const char* var_name = deref->m_symname->spelling();
            Symbol* sym = m_st->lookup(deref->m_attribute.m_scope, var_name);
            if (!sym) {
                fprintf(stderr, "Error: Pointer variable %s not found in scope\n", var_name);
                fprintf(m_outputfile, "\tjmp\tprogram_exit\n");
                return;
            }
            int offset = sym->get_offset();
            fprintf(m_outputfile, "\tleal\t%d(%%ebp), %%eax\n", -1*offset);
            fprintf(m_outputfile, "\tpushl\t%%eax\n");
        }
        else {
            fprintf(stderr, "Error: Invalid operand for address-of operator\n");
            fprintf(m_outputfile, "\tjmp\tprogram_exit\n");
        }
    }

    void visitDeref(Deref* p)
    {
        // First evaluate the pointer expression
        p->m_expr->accept(this);
        
        // Pop the pointer value into eax
        fprintf(m_outputfile, "\tpopl\t%%eax\n");
        
        // Check for null pointer
        fprintf(m_outputfile, "\tcmpl\t$0, %%eax\n");
        fprintf(m_outputfile, "\tje\tprogram_exit\n");  // Exit if null pointer
        
        // Load the value based on the type being dereferenced
        if (p->m_attribute.m_basetype == bt_integer) {
            // For integer pointers, load a word
            fprintf(m_outputfile, "\tmovl\t(%%eax), %%ebx\n");
        } else if (p->m_attribute.m_basetype == bt_char) {
            // For character pointers, load a byte
            fprintf(m_outputfile, "\tmovzbl\t(%%eax), %%ebx\n");
        } else {
            fprintf(stderr, "Error: Invalid type for dereference operation\n");
            fprintf(m_outputfile, "\tjmp\tprogram_exit\n");
            return;
        }
        
        // Push the dereferenced value
        fprintf(m_outputfile, "\tpushl\t%%ebx\n");
    }
};


void dopass_codegen(Program_ptr ast, SymTab* st)
{
    Codegen* codegen = new Codegen(stdout, st);
    ast->accept(codegen);
    delete codegen;
}