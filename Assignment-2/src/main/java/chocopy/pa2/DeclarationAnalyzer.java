package chocopy.pa2;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

import chocopy.common.analysis.AbstractNodeAnalyzer;
import chocopy.common.analysis.types.*;
import chocopy.common.astnodes.*;

/**
 * Analyzes declarations to create a top-level symbol table.
 */
public class DeclarationAnalyzer extends AbstractNodeAnalyzer<Type> {

    /** Current symbol table.  Changes with new declarative region. */
    private SymbolTable<Type> sym = new SymbolTable<>();
    /** Global symbol table. */
    private final SymbolTable<Type> globals = sym;
    /** Receiver for semantic error messages. */
    private final Errors errors;

    /** ********************** */
    /** Symbol table stack. To track the parent symbol table. */
    private Stack<SymbolTable<Type>> symbol_table_stack = new Stack<>();

    /** A new declaration analyzer sending errors to ERRORS0. */
    public DeclarationAnalyzer(Errors errors0) {
        errors = errors0;
    }


    public SymbolTable<Type> getGlobals() {
        return globals;
    }

    private ClassTypeSuper initialise_object_class() {
        List<ValueType> parameters = new ArrayList<>();
        parameters.add(ValueType.OBJECT_TYPE);


        FuncType initialiseFunction = new FuncType(parameters, ValueType.NONE_TYPE);

        SymbolTable<Type> symbolTableObject = new SymbolTable<>(globals);
        symbolTableObject.put("__init__", initialiseFunction);

        
        globals.putScope("object", symbolTableObject);
        return new ClassTypeSuper(null, "object");
    }

    @Override
    public Type analyze(Program program) {

        

        List<ValueType> parameters = new ArrayList<>();
        parameters.add(ValueType.OBJECT_TYPE);


        globals.put("len", new FuncType(parameters, ValueType.INT_TYPE));
        globals.put("input", new FuncType(ValueType.STR_TYPE));
        globals.put("print", new FuncType(parameters, ValueType.NONE_TYPE));
        
        globals.put("int", new ClassTypeSuper("object"));
        globals.put("str", new ClassTypeSuper("object"));
        globals.put("bool", new ClassTypeSuper("object"));
    
        globals.put("object", initialise_object_class());

        symbol_table_stack.push(globals);
        sym = symbol_table_stack.peek();

        for (Declaration decl : program.declarations) {
            Identifier id = decl.getIdentifier();
            String name = id.name;
            Type type = decl.dispatch(this);
            if (type == null)
            {
                continue;
            }
            if (sym.declares(name))
            {
                errors.semError(id, "Duplicate declaration of identifier in same " + "scope: %s", name);
            }
            else
            {
                sym.put(name, type);
            }
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
    public Type analyze(ClassType classType) {

        String class_name = classType.className;
        
        if (class_name.equals("<None>")) {
            return Type.NONE_TYPE;
        }
        return ValueType.annotationToValueType(classType);
    }

    @Override
    public Type analyze(ListType listType) {

        return ValueType.annotationToValueType(listType);
    }


    @Override
    public Type analyze(FuncDef funcDef) {

        Identifier function_ID = funcDef.getIdentifier();
        String function_Name = function_ID.name;
        SymbolTable<Type> current_Symbol = new SymbolTable<>(sym);

        symbol_table_stack.push(current_Symbol);
        sym.putScope(function_Name, current_Symbol);
        sym = symbol_table_stack.peek();

        List<ValueType> parameters = new ArrayList<>();

        for (TypedVar variable : funcDef.params)
        {

            Identifier id = variable.identifier;
            String name = id.name;
            Type type = variable.dispatch(this);
            
            if (type == null)
            {
                continue;
            }
            if (sym.declares(name))
            {
                errors.semError(id, "Duplicate declaration of identifier in same " + "scope: %s", name);
            }
            else
            {
                sym.put(name, type);
                parameters.add((ValueType) type);
            }
        }

        for (Declaration decl : funcDef.declarations) {
            
            Identifier id = decl.getIdentifier();
            String name = id.name;
            Type type = decl.dispatch(this);

            if (type == null)
            {
                continue;
            }

            if (sym.declares(name))
            {
                errors.semError(id, "Duplicate declaration of identifier in same " + "scope: %s", name);
            }
            else
            {
                sym.put(name, type);
            }
        }

        ValueType returnType = (ValueType) funcDef.returnType.dispatch(this);
        sym.put("return", returnType);
        
        symbol_table_stack.pop();
        sym = symbol_table_stack.peek();
        return new FuncType(parameters, returnType);
    }

    @Override 
    public Type analyze(ClassDef classDef) {

        Identifier super_Class_ID = classDef.superClass;
        String super_Class_Name = super_Class_ID.name;
        String class_Name = classDef.name.name;

        if (!globals.declares(super_Class_Name))
        {
            errors.semError(super_Class_ID, "Super-class not defined: %s", super_Class_Name);
            return null;
        };

        if (!(globals.get(super_Class_Name) instanceof ClassTypeSuper)) {
            errors.semError(super_Class_ID, "Super-class must be a class: %s", super_Class_Name);
            return null;
        }

        if (((ClassTypeSuper) globals.get(super_Class_Name)).isSpecialClass())
        {
            errors.semError(super_Class_ID, "Cannot extend special class: %s", super_Class_Name);
            return null;
        }

        SymbolTable<Type> super_Symbol = globals.getScope(super_Class_Name);
        SymbolTable<Type> current_Symbol = new SymbolTable<>(super_Symbol);
        globals.putScope(class_Name, current_Symbol);

        symbol_table_stack.push(current_Symbol);
        sym = symbol_table_stack.peek();
        sym.put(class_Name, new ClassTypeSuper(super_Class_Name, class_Name));

        //check attributes and methods
        for (Declaration decl : classDef.declarations)
        {
            Identifier id = decl.getIdentifier();
            String name = id.name;
            Type type = decl.dispatch(this);

            if (type == null)
            {
                continue;
            }

            if (sym.declares(name))
            {
                errors.semError(id, "Duplicate declaration of identifier in same " + "scope: %s", name);
                continue;
            }

            if (globals.declares(name) && globals.get(name) instanceof ClassTypeSuper)
            {
                // name is the defined type
                errors.semError(id, "Cannot shadow class name: %s", name);
                continue;
            }

            if (type instanceof ValueType && super_Symbol.declares(name))
            {
                errors.semError(id, "Cannot re-define attribute: %s", name);
                continue;
            }

            sym.put(name, type);
        }
        symbol_table_stack.pop();
        sym = symbol_table_stack.peek();
        return new ClassTypeSuper(super_Class_Name, class_Name);
    }

}