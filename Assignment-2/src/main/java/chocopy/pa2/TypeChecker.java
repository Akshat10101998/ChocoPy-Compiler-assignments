package chocopy.pa2;

import chocopy.common.astnodes.*;
import chocopy.common.analysis.AbstractNodeAnalyzer;
import chocopy.common.analysis.types.*;
import static chocopy.common.analysis.types.Type.*;

import java.net.http.WebSocket.Listener;
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;


/** Analyzer that performs ChocoPy type checks on all nodes.  Applied after
 *  collecting declarations. */
public class TypeChecker extends AbstractNodeAnalyzer<Type> {

    /** The current symbol table (changes depending on the function
     *  being analyzed). */
    private SymbolTable<Type> sym;
    /** Global symbol table. */
    private SymbolTable<Type> global_symbol_table;
    /** Collector for errors. */
    private Errors errors;

    /************************ */
    // Symbol table stack. To track the parent symbol table.
    private Stack<SymbolTable<Type>> symbol_table_stack = new Stack<>();

    /** Creates a type checker using GLOBALSYMBOLS for the initial global
     *  symbol table and ERRORS0 to receive semantic errors. */
    public TypeChecker(SymbolTable<Type> globalSymbols, Errors errors0) {
        global_symbol_table = globalSymbols;
        errors = errors0;
    }

    /** Inserts an error message in NODE if there isn't one already.
     *  The message is constructed with MESSAGE and ARGS as for
     *  String.format. */
    private void err(Node node, String message, Object... args) {
        errors.semError(node, message, args);
    }

    @Override
    public Type analyze(Program program) {

        symbol_table_stack.push(global_symbol_table);
        sym = symbol_table_stack.peek();
        
        for (Declaration decl : program.declarations)
        {
            decl.dispatch(this);
        }

        for (Stmt stmt : program.statements)
        {
            stmt.dispatch(this);
        }

        symbol_table_stack.pop();
        return null;
    }

    @Override
    public Type analyze(ExprStmt s) {
        
        s.expr.dispatch(this);
        
        return null;
    }

    @Override
    public Type analyze(IntegerLiteral i) {
        
        return i.setInferredType(INT_TYPE);
    }

    @Override
    public Type analyze(BinaryExpr e) {

        Type t1 = e.left.dispatch(this);
        Type t2 = e.right.dispatch(this);

        switch (e.operator)
        {

        case "-":
        case "*":
        case "//":
        case "%":
            if (INT_TYPE.equals(t1) && INT_TYPE.equals(t2))
            {
                return e.setInferredType(INT_TYPE);
            }
            else
            {
                err(e, "Cannot apply operator `%s` on types `%s` and `%s`",
                    e.operator, t1, t2);
                return e.setInferredType(INT_TYPE);
            }

        case "+":
            if (INT_TYPE.equals(t1) && INT_TYPE.equals(t2))
            {
                return e.setInferredType(INT_TYPE);
            }
            else if (INT_TYPE.equals(t1) || INT_TYPE.equals(t2))
            {
                err(e, "Cannot apply operator `%s` on types `%s` and `%s`",
                    e.operator, t1, t2);
                return e.setInferredType(INT_TYPE);
            }
            else if (STR_TYPE.equals(t1) && STR_TYPE.equals(t2))
            {
                return e.setInferredType(STR_TYPE);
            }
            else if (t1.isListType() && t2.isListType())
            {
                Type element1 = t1.elementType();
                Type element2 = t2.elementType();
                Type resultantType = gtComAcs(element1, element2);
                return e.setInferredType(new ListValueType(resultantType));
            }
            else
            {
                err(e, "Cannot apply operator `%s` on types `%s` and `%s`",
                    e.operator, t1, t2);
                return e.setInferredType(OBJECT_TYPE);
            }

        case "==":
        case "!=":
            if (BOOL_TYPE.equals(t1) && BOOL_TYPE.equals(t2))
            {
                return e.setInferredType(BOOL_TYPE);
            }
            else if (INT_TYPE.equals(t1) && INT_TYPE.equals(t2))
            {
                return e.setInferredType(BOOL_TYPE);
            }
            else if (STR_TYPE.equals(t1) && STR_TYPE.equals(t2))
            {
                return e.setInferredType(BOOL_TYPE);
            }
            else
            {
                err(e, "Cannot apply operator `%s` on types `%s` and `%s`",
                    e.operator, t1, t2);
                return e.setInferredType(BOOL_TYPE);
            }
        
        case "<":
        case ">":
        case "<=":
        case ">=":
            if (INT_TYPE.equals(t1) && INT_TYPE.equals(t2))
            {
                return e.setInferredType(BOOL_TYPE);
            }
            else
            {
                err(e, "Cannot apply operator `%s` on types `%s` and `%s`",
                    e.operator, t1, t2);
                return e.setInferredType(BOOL_TYPE);
            }

        case "or":
        case "and":
            if (BOOL_TYPE.equals(t1) && BOOL_TYPE.equals(t2))
            {
                return e.setInferredType(BOOL_TYPE);
            }
            else
            {
                err(e, "Cannot apply operator `%s` on types `%s` and `%s`",
                    e.operator, t1, t2);
                return e.setInferredType(BOOL_TYPE);
            }
        
        case "is":
            if (!t1.isSpecialType() && !t2.isSpecialType())
            {
                return e.setInferredType(BOOL_TYPE);
            }
            else
            {
                err(e, "Cannot apply operator `%s` on types `%s` and `%s`",
                    e.operator, t1, t2);
                return e.setInferredType(BOOL_TYPE);
            }

        default:
            return e.setInferredType(OBJECT_TYPE);
        }

    }

    @Override
    public Type analyze(Identifier id) {

        String varName = id.name;
        Type varType = sym.get(varName);

        if (varType != null)
        {
            if (varType instanceof ClassTypeSuper)
            {
                varType = Type.OBJECT_TYPE;
            }
            return id.setInferredType(varType);
        }

        err(id, "Not a variable: %s", varName);
        return id.setInferredType(ValueType.OBJECT_TYPE);
    }    

    @Override
    public Type analyze(AssignStmt assignment_statements) {

        Type type1 = null;
        Type type2 = assignment_statements.value.dispatch(this);

        for (Expr trgt : assignment_statements.targets)
        {

            type1 = trgt.dispatch(this);

            if (!chkTypCompatibility(type1, type2))
            {
                err(assignment_statements,  "Expected type `%s`; got type `%s`",  type1, type2);
            }

            if (trgt instanceof IndexExpr && type1.equals(STR_TYPE))
            {
                err(trgt, "`%s` is not a list type", type1);
            }
        }
        
        return null;
    }

    @Override
    public Type analyze(BooleanLiteral i) {
        
        return i.setInferredType(BOOL_TYPE);
    }

    @Override
    public Type analyze(CallExpr call_Expression) {

        Identifier call_expression_function = call_Expression.function;
        String call_expression_function_name = call_expression_function.name;
        Type call_expression_function_type = sym.get(call_expression_function_name);
        Type call_expression_function_return_type;
        List<ValueType> parameters;
        List<Type> arguments = new ArrayList<>();

        for (Expr exp : call_Expression.args) {
            arguments.add(exp.dispatch(this));
        }

        if (call_expression_function_type instanceof ClassTypeSuper)
        {
            parameters = ((FuncType) sym.getScope(call_expression_function_name).get("__init__")).parameters;

            parameters = parameters.subList(1, parameters.size());
            call_expression_function_return_type = new ClassValueType(call_expression_function_type.className());
        }

        else if (call_expression_function_type instanceof FuncType)
        {
            call_expression_function.dispatch(this);

            parameters = ((FuncType) call_expression_function_type).parameters;
            call_expression_function_return_type = ((FuncType) call_expression_function_type).returnType;
        }
        
        else
        {
            err(call_Expression, "Not a function or class: %s", call_expression_function_name);
            return call_Expression.setInferredType(OBJECT_TYPE);
        }

        chkParamsTyp(call_Expression, parameters, arguments);

        return call_Expression.setInferredType(call_expression_function_return_type);
    }

    @Override
    public Type analyze(ClassDef class_Def) {

        Identifier classDef_identifier = class_Def.getIdentifier();
        sym = global_symbol_table.getScope(classDef_identifier.name);
        symbol_table_stack.push(sym);

        for (Declaration declaration : class_Def.declarations)
        {
            declaration.dispatch(this);
        }

        symbol_table_stack.pop();
        sym = symbol_table_stack.peek();
        return null;
    }

    @Override
    public Type analyze(ForStmt for_Statement) {

        for_Statement.identifier.dispatch(this);
        for_Statement.iterable.dispatch(this);

        for (Stmt statement : for_Statement.body)
        {
            statement.dispatch(this);
        }

        return null;
    }

    @Override
    public Type analyze(FuncDef func_Def) {

        Identifier function_identifier = func_Def.getIdentifier();
        sym = sym.getScope(function_identifier.name);
        symbol_table_stack.push(sym);
        Type returnType = null;

        for (Declaration declaration : func_Def.declarations)
        {
            declaration.dispatch(this);
        }

        for (Stmt statement : func_Def.statements)
        {
            statement.dispatch(this);

            if (statement instanceof ReturnStmt && ((ReturnStmt) statement).value != null)
            {
                returnType = ((ReturnStmt) statement).value.getInferredType();
            }
        }

        if (returnType == null && !sym.get("return").equals(NONE_TYPE) && !sym.get("return").equals(OBJECT_TYPE))
        {
            err(function_identifier, "All paths in this function/method must have a return statement: %s", function_identifier.name);
        }

        symbol_table_stack.pop();
        sym = symbol_table_stack.peek();
        return null;
    }

    @Override
    public Type analyze(IfExpr if_expression) {

        if_expression.condition.dispatch(this);

        Type type1 = if_expression.thenExpr.dispatch(this);
        Type type2 = if_expression.elseExpr.dispatch(this);

        Type commonAncestorType = gtComAcs(type1, type2);

        return if_expression.setInferredType(commonAncestorType);
    }

    @Override
    public Type analyze(IfStmt if_statement) {

        if_statement.condition.dispatch(this);

        for (Stmt statement : if_statement.elseBody)
        {
            statement.dispatch(this);
        }

        for (Stmt statement : if_statement.thenBody)
        {
            statement.dispatch(this);
        }

        return null;
    }

    @Override
    public Type analyze(IndexExpr index_expression) {

        Type type1 = index_expression.list.dispatch(this);
        Type type2 = index_expression.index.dispatch(this);

        if (!type2.equals(INT_TYPE))
        {
            err(index_expression, "Index is of non-integer type `%s`", type2);
        }

        if (type1.isListType())
        {
            type1 = ((ListValueType) type1).elementType;
        }
        else if (!type1.equals(STR_TYPE))
        {
            err(index_expression, "Cannot index into type `%s`", type1);
            return index_expression.setInferredType(OBJECT_TYPE);
        }

        return index_expression.setInferredType(type1);
    }

    @Override
    public Type analyze(ListExpr list_expression){
        
        if (list_expression.elements == null || list_expression.elements.size() == 0)
        {
            return list_expression.setInferredType(EMPTY_TYPE);
        }

        Type commonAncestorType = null;

        for (Expr exp : list_expression.elements)
        {
            Type element_Type = exp.dispatch(this);
            commonAncestorType = gtComAcs(commonAncestorType, element_Type);
        }

        return list_expression.setInferredType(new ListValueType(commonAncestorType));
    }

    @Override
    public Type analyze(MemberExpr member_expression) {

        Expr member_expression_object = member_expression.object;
        Type member_expression_object_type = member_expression_object.dispatch(this);
        String member_expression_object_class_name = member_expression_object_type.className();
        SymbolTable<Type> member_expression_object_class_symbol = global_symbol_table.getScope(member_expression_object_class_name);
        String attribute_name = member_expression.member.name;

        if (member_expression_object_class_symbol == null || member_expression_object_class_symbol.get(attribute_name) == null)
        {
            err(member_expression, "There is no attribute named `%s` in class `%s`", attribute_name, member_expression_object_class_name);
            return OBJECT_TYPE;
        }

        Type attribute_Type = member_expression_object_class_symbol.get(attribute_name);
        return member_expression.setInferredType(attribute_Type);
    }

    @Override
    public Type analyze(MethodCallExpr methodCallExpression) {

        MemberExpr call_expression_method = methodCallExpression.method;
        Type call_expression_method_type = call_expression_method.dispatch(this);
        Expr call_expression_method_object = call_expression_method.object;
        Type call_expression_method_object_type = call_expression_method_object.getInferredType();

        List<ValueType> parameters = null;
        List<Type> arguments = new ArrayList<>();
        arguments.add(call_expression_method_object_type);

        if (!call_expression_method_type.isFuncType())
        {
            String call_expression_method_object_class_name = call_expression_method_object_type.className();
            Identifier call_expression_method_member = call_expression_method.member;
            String call_expression_method_member_name = call_expression_method_member.name;

            err(methodCallExpression, "There is no method named `%s` in class `%s`", call_expression_method_member_name, call_expression_method_object_class_name);
            return call_expression_method_type;
        }

        for (Expr exp : methodCallExpression.args)
        {
            arguments.add(exp.dispatch(this));
        }

        parameters = ((FuncType) call_expression_method_type).parameters;
        chkParamsTyp(methodCallExpression, parameters, arguments);
        return methodCallExpression.setInferredType(((FuncType) call_expression_method_type).returnType);
    }

    @Override
    public Type analyze(NoneLiteral i) {

        return i.setInferredType(NONE_TYPE);
    }

    @Override
    public Type analyze(ReturnStmt stmt) {

        Type type_t = null;
        Type exp_type = sym.get("return");

        if (stmt.value != null)
        {
            type_t = stmt.value.dispatch(this);
        }

        if (!iAcs(exp_type, type_t))
        {
            err(stmt, "Expected type `%s`; got %s", exp_type, type_t == null ? "`None`" : "type `" + type_t + "`");
        }

        return null;
    }

    @Override
    public Type analyze(StringLiteral i) {

        return i.setInferredType(STR_TYPE);
    }

    @Override
    public Type analyze(UnaryExpr unary_expression) {

        Expr unary_expression_operand = unary_expression.operand;
        Type unary_type = unary_expression_operand.dispatch(this);
        String unary_expression_operator = unary_expression.operator;

        switch (unary_expression_operator) {

        case "not":
            if (!BOOL_TYPE.equals(unary_type))
            {
                err(unary_expression, "Cannot apply operator `%s` on type `%s`", unary_expression_operator, unary_type);
            }
            return unary_expression.setInferredType(BOOL_TYPE);
            
        case "-":
            if (!INT_TYPE.equals(unary_type))
            {
                err(unary_expression, "Cannot apply operator `%s` on type `%s`", unary_expression_operator, unary_type);
            }
            return unary_expression.setInferredType(INT_TYPE);
        
        default:
            return unary_expression.setInferredType(OBJECT_TYPE);
        }
    }

    @Override
    public Type analyze(VarDef varDef) {

        TypedVar varDef_variable = varDef.var;
        Identifier varDef_variable_identifier = varDef_variable.identifier;
        String varDef_variable_name = varDef_variable_identifier.name;
        Type varDef_variable_type = sym.get(varDef_variable_name);
        Type varDef_variable_value_type = varDef.value.dispatch(this);

        if (!chkTypCompatibility(varDef_variable_type, varDef_variable_value_type))
        {
            err(varDef, "Expected type `%s`; got type `%s`", varDef_variable_type, varDef_variable_value_type);
        }

        return null;
    }

    @Override
    public Type analyze(WhileStmt while_statement) {

        Expr while_statement_condition = while_statement.condition;
        while_statement_condition.dispatch(this);
        List<Stmt> while_statement_body = while_statement.body;

        for (Stmt statement : while_statement.body)
        {
            statement.dispatch(this);
        }

        return null;
    }



    private boolean iAcs(Type parent, Type child) {
        
        if (parent == null || child == null)
        {
            return false;
        }        

        if (parent.isListType() && child.isListType())
        {
            ValueType parent_element_type = parent.elementType();
            ValueType child_element_type = child.elementType();
            return parent_element_type.equals(child_element_type);
        } 

        if (parent.equals(OBJECT_TYPE) && (child.equals(EMPTY_TYPE) || child.equals(NONE_TYPE) || child.isListType()))
        {
            return true;
        }

        if (!(parent instanceof ClassValueType) || !(child instanceof ClassValueType))
        {
            return false;
        }

        String parent_class_name = parent.className();
        String child_class_name = child.className();

        while (!parent_class_name.equals(child_class_name))
        {
            if (parent_class_name == null || child_class_name == null || global_symbol_table.get(child_class_name) == null)
            {
                return false;
            }
            child_class_name = ((ClassTypeSuper) global_symbol_table.get(child_class_name)).getSuperClassName();
        }
        return true;
    }

    private Stack<String> gtAllAcss(Type typ) {

        Stack<String> symbol_table_stack = new Stack<>();
        String typ_class_name = typ.className();

        while (typ_class_name != null)
        {
            symbol_table_stack.push(typ_class_name);
            typ_class_name = ((ClassTypeSuper) global_symbol_table.get(typ_class_name)).getSuperClassName();
        }

        return symbol_table_stack;
    }
    
    private Type gtComAcs(Type typ1, Type typ2) {
        
        if (typ1 == null)
        {
            return typ2;
        }

        Stack<String> symbol_table_stack1 = gtAllAcss(typ1);
        Stack<String> symbol_table_stack2 = gtAllAcss(typ2);
        String ancestor_class_name = null;

        while (!symbol_table_stack1.empty() && !symbol_table_stack2.empty())
        {
            String str1 = symbol_table_stack1.pop();
            String str2 = symbol_table_stack2.pop();
            
            if (str1.equals(str2))
            {
                ancestor_class_name = str1;
            }
        }

        return new ClassValueType(ancestor_class_name);
    }
    
    private void chkParamsTyp(Node node, List<ValueType> parameters, List<Type> arguments) {
        
        if (parameters.size() != arguments.size())
        {
            int offset_value = node instanceof MethodCallExpr ? -1 : 0;
            err(node, "Expected %d arguments; got %d", parameters.size() + offset_value, arguments.size() + offset_value);
            return;
        }

        for (int i = 0; i < parameters.size(); i++)
        {
            Type parameter = parameters.get(i);
            Type argument = arguments.get(i);

            if (!iAcs(parameter, argument))
            {
                err(node, "Expected type `%s`; got type `%s` in parameter %d", parameter, argument, i);
            }
        }
    }

    private boolean chkTypCompatibility(Type trgt, Type val) {
        
        if (iAcs(trgt, val))
        {
            return true;
        }

        if (trgt.isListType() && val.isListType())
        {
            Type typ1 = trgt.elementType();
            Type typ2 = val.elementType();
            return !typ1.isSpecialType() && typ2.equals(NONE_TYPE);
        }
        
        if (!trgt.isSpecialType() && val.equals(NONE_TYPE))
        {
            return true;
        }
        
        if (trgt.isListType() && !trgt.elementType().isListType() && val.equals(EMPTY_TYPE))
        {
            return true;
        }

        if (trgt == null || val == null)
        {
            return false;
        }

        return false;
    }
}