package chocopy.pa2;

import chocopy.common.analysis.AbstractNodeAnalyzer;
import chocopy.common.analysis.types.*;
import chocopy.common.astnodes.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

/**
 * Analyzes declarations to create a top-level symbol table.
 */
public class CheckDeclarations extends AbstractNodeAnalyzer<Type> {

    /** Current symbol table.  Changes with new declarative region. */
    private SymbolTable<Type> sym = null;
    /** Global symbol table. */
    private SymbolTable<Type> globals;
    /** Receiver for semantic error messages. */
    private final Errors errors;

    /** Symbol table stack. To track the parent symbol table. */
    private Stack<SymbolTable<Type>> symbol_table_stack = new Stack<>();

    /** Creates a type checker using GLOBALSYMBOLS for the initial global
     *  symbol table and ERRORS0 to receive semantic errors. */
    public CheckDeclarations(SymbolTable<Type> globalSymbols, Errors errors0) {
        globals = globalSymbols;
        errors = errors0;
    }

    @Override
    public Type analyze(Program program) {
        
        symbol_table_stack.push(globals);
        sym = symbol_table_stack.peek();

        for (Stmt statement : program.statements)
        {
            if (statement instanceof ReturnStmt)
            {
                errors.semError(statement, "Return statement cannot appear at the top level");
            }
        }

        for (Declaration declaration : program.declarations)
        {
            declaration.dispatch(this);
        }

        return null;
    }

    @Override
    public Type analyze(VarDef varDef) {

        TypedVar varDef_variable = varDef.var;
        return varDef_variable.dispatch(this);
    }

    @Override
    public Type analyze(TypedVar typedVar) {

        TypeAnnotation typedVar_type = typedVar.type;
        return typedVar_type.dispatch(this);
    }

    @Override
    public Type analyze(ListType listType) {

        return ValueType.annotationToValueType(listType);
    }

    @Override
    public Type analyze(ClassType classType) {

        String class_name = classType.className;

        if (class_name.equals("<None>"))
        {
            return Type.NONE_TYPE;
        }

        if (!checkClassNameExist(class_name))
        {
            errors.semError(classType, "Invalid type annotation; " + "there is no class named: %s", class_name);
            return null;
        }

        return ValueType.annotationToValueType(classType);
    }
    
    @Override
    public Type analyze(GlobalDecl globalDeclaration)
    {
        Identifier global_declaration_identifier = globalDeclaration.getIdentifier();
        String global_declaration_identifier_name = global_declaration_identifier.name;

        if (!globals.declares(global_declaration_identifier_name)  || !globals.get(global_declaration_identifier_name).isValueType())
        {
            errors.semError(global_declaration_identifier, "Not a global variable: %s", global_declaration_identifier_name);
            return null;
        }

        if (sym.declares(global_declaration_identifier_name))
        {
            errors.semError(global_declaration_identifier, "Duplicate declaration of identifier in same " + "scope: %s", global_declaration_identifier_name);
            return null;
        }

        return globals.get(global_declaration_identifier_name);
    }

    @Override
    public Type analyze(NonLocalDecl nonLocalDeclaration) {

        SymbolTable<Type> outerParentSymbol = sym.getParent();
        Identifier nonLocalDeclaration_identifier = nonLocalDeclaration.getIdentifier();
        String nonLocalDeclaration_identifier_name = nonLocalDeclaration_identifier.name;
        
        if (outerParentSymbol == globals || !outerParentSymbol.declares(nonLocalDeclaration_identifier_name) || !outerParentSymbol.get(nonLocalDeclaration_identifier_name).isValueType())
        {
            errors.semError(nonLocalDeclaration_identifier, "Not a nonlocal variable: %s", nonLocalDeclaration_identifier_name);
            return null;
        }

        if (sym.declares(nonLocalDeclaration_identifier_name))
        {
            errors.semError(nonLocalDeclaration_identifier, "Duplicate declaration of identifier in same " + "scope: %s", nonLocalDeclaration_identifier_name);
            return null;
        }

        return outerParentSymbol.get(nonLocalDeclaration_identifier_name);
    }


    @Override
    public Type analyze(FuncDef funcDef) {

        Identifier function_ID = funcDef.getIdentifier();
        String function_Name = function_ID.name;
        sym = sym.getScope(function_Name);

        if (sym == null)
        {
            System.out.println(function_Name + " scope is empty!!!!");
        }

        symbol_table_stack.push(sym);

        for (TypedVar variable : funcDef.params)
        {
            Identifier variable_identifier = variable.identifier;
            String variable_name = variable_identifier.name;
            Type variable_type = variable.dispatch(this);
            
            if (checkClassNameExist(variable_name))
            {
                // name is the defined type
                errors.semError(variable_identifier, "Cannot shadow class name: %s", variable_name);
            }
        }

        for (Declaration declaration : funcDef.declarations)
        {
            Identifier declaration_identifier = declaration.getIdentifier();
            String declaration_name = declaration_identifier.name;
            Type declaration_type = declaration.dispatch(this);

            // global and nonlocal variable
            if (declaration_type != null && !sym.declares(declaration_name))
            {
                sym.put(declaration_name, declaration_type);
            }

            if (checkClassNameExist(declaration_name))
            {
                // name is the defined type
                errors.semError(declaration_identifier, "Cannot shadow class name: %s", declaration_name);
            }
        }

        for (Stmt statement : funcDef.statements)
        {
            statement.dispatch(this);
        }

        funcDef.returnType.dispatch(this);

        symbol_table_stack.pop();
        sym = symbol_table_stack.peek();

        return sym.get(function_Name);
    }

    @Override 
    public Type analyze(AssignStmt statement) {
        
        for (Expr expression : statement.targets)
        {
            if (expression instanceof Identifier)
            {
                Identifier expression_identifier = (Identifier) expression ;
                String expression_name = expression_identifier.name;

                if (!sym.declares(expression_name))
                {
                    errors.semError(expression_identifier, "Cannot assign to variable that is " + "not explicitly declared in this scope: %s", expression_name);
                }
            }
        }
        return null;
    }

    @Override 
    public Type analyze(ClassDef classDef) {
        
        String class_Name = classDef.name.name;

        sym = globals.getScope(class_Name);

        if (sym == null)
        {
            sym = symbol_table_stack.peek();
            System.out.println(class_Name + " scope is empty!!!!");
            return null;
        }
        symbol_table_stack.push(sym);

        //check attributes and methods
        for (Declaration declaration : classDef.declarations)
        {
            Identifier declaration_identifier = declaration.getIdentifier();
            Type declaration_type = declaration.dispatch(this);

            if (declaration_type == null)
            {
                continue;
            }

            if (declaration_type instanceof FuncType)
            {
                checkClassMethodValid(class_Name, declaration_identifier, (FuncType) declaration_type);
            }
        }
        
        symbol_table_stack.pop();
        sym = symbol_table_stack.peek();
        return globals.get(class_Name);
    }

    

    private boolean checkClassNameExist(String class_name) {
        
        // avoid shadow class name in previous declaration analyzer
        if (globals.declares(class_name) && (globals.get(class_name) instanceof ClassTypeSuper))
        {
            return true;
        }

        return sym.get(class_name) != null && (sym.get(class_name) instanceof ClassTypeSuper);
    }

    private boolean checkFirstParameterValid(String class_name, FuncType function_type) {
        
        if (function_type.parameters.size() == 0)
        {
            return false;
        }

        ValueType funcVal = function_type.getParamType(0);
        String first_parameter = funcVal.className();
        return first_parameter.equals(class_name);
    }

    private boolean checkMethodNameValid(String function_name) {

        SymbolTable<Type> super_symbol = sym.getParent();
        return super_symbol.get(function_name) == null || !(super_symbol.get(function_name) instanceof ValueType);
    }

    private boolean checkMethodSignaturesSame(String function_name, FuncType function_type) {

        SymbolTable<Type> super_symbol = sym.getParent();
        FuncType super_function_type = (FuncType) super_symbol.get(function_name);        

        if (super_function_type == null)
        {
            return true;
        }
        
        if (super_function_type.parameters.size() != function_type.parameters.size())
        {
            return false;
        }

        if (function_name.equals("__init__") && function_type.parameters.size() == 1)
        {
            return true;
        }

        for (int i = 1; i < function_type.parameters.size(); i++)
        {
            ValueType super_parameter = super_function_type.parameters.get(i);
            ValueType parameter = function_type.parameters.get(i);

            if (!super_parameter.equals(parameter))
            {
                return false;
            }
        }

        return super_function_type.returnType.equals(function_type.returnType);
    }
    
    private void checkClassMethodValid(String class_name, Identifier function_ID, FuncType function_type) {
        
        String function_name = function_ID.name;

        if (!checkFirstParameterValid(class_name, function_type))
        {
            errors.semError(function_ID, "First parameter of the following method "  + "must be of the enclosing class: %s", function_name);
            return;
        }

        if (!checkMethodNameValid(function_name)) {
            errors.semError(function_ID, "Cannot re-define attribute: %s", function_name);
            return;
        }

        if (!checkMethodSignaturesSame(function_name, function_type))
        {
            errors.semError(function_ID, "Method overridden with different type signature: %s", function_name);
            return;
        }
    }
}