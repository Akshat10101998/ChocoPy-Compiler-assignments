package chocopy.pa3;

import java.util.List;
import java.util.Collections;
import java.util.ArrayList;

import chocopy.common.analysis.SymbolTable;
import chocopy.common.analysis.AbstractNodeAnalyzer;
import chocopy.common.analysis.*;
import chocopy.common.analysis.types.*;
import chocopy.common.astnodes.Stmt;
import chocopy.common.astnodes.ReturnStmt;
import chocopy.common.astnodes.*;
import chocopy.common.codegen.CodeGenBase;
import chocopy.common.codegen.FuncInfo;
import chocopy.common.codegen.Label;
import chocopy.common.codegen.*;
import chocopy.common.codegen.RiscVBackend;
import chocopy.common.codegen.RiscVBackend.Register;
import chocopy.common.codegen.SymbolInfo;

import static chocopy.common.codegen.RiscVBackend.Register.*;

/**
 * This is where the main implementation of PA3 will live.
 *
 * A large part of the functionality has already been implemented
 * in the base class, CodeGenBase. Make sure to read through that
 * class, since you will want to use many of its fields
 * and utility methods in this class when emitting code.
 *
 * Also read the PDF spec for details on what the base class does and
 * what APIs it exposes for its sub-class (this one). Of particular
 * importance is knowing what all the SymbolInfo classes contain.
 */

public class CodeGenImpl extends CodeGenBase {

    /** A code generator emitting instructions to BACKEND. */

    public CodeGenImpl(RiscVBackend backend) {
        super(backend);
    }

    /** Operation on None. */
    private final Label errorNone = new Label("error.None");
    /** Division by zero. */
    private final Label errorDiv = new Label("error.Div");
    /** Index out of bounds. */
    private final Label errorOob = new Label("error.OOB");

    private final Label strcat = new Label("str__cat");
    private final Label boolean_type = new Label("boolean_type");
    private final Label Integer_type = new Label("box_int");
    private final Label streql = new Label("str__eql");
    private final Label strneql = new Label("str__neql");
    private final Label concat_list_label = new Label("list__Concat");

    private final Label allChars = new Label("allChars");
    private final Label initChars = new Label("initChars");
    private final Label strPrototype = new Label("$str$prototype");
    private final String SYM_NAME_OFFSET_LEN = "@.__len__";
    private final String SYM_NAME_OFFSET_STR = "@.__str__";

    private boolean emit_concat_flag = false;

    /**
     * Emits the top level of the program.
     *
     * This method is invoked exactly once, and is surrounded
     * by some boilerplate code that: (1) initializes the heap
     * before the top-level begins and (2) exits after the top-level
     * ends.
     *
     * You only need to generate code for statements.
     *
     * @param statements top level statements
     */

    protected void emitTopLevel(List<Stmt> statements) {

        SlotCounter counter_slots = new SlotCounter();
        int slt_offst = counter_slots.allocAndClaimSlotFromBottom();
        String main_frame_size_name = "@main.size";
        StmtAnalyzer statement_analyser = new StmtAnalyzer(null, counter_slots);

        backend.emitADDI(SP, SP, "-" + main_frame_size_name, "");
        backend.emitADDI(FP, SP, main_frame_size_name, "");
        backend.emitSW(ZERO, SP, slt_offst, "");
        backend.emitSW(ZERO, SP, slt_offst, "");
        backend.emitJAL(initChars, "");

        slt_offst = counter_slots.allocAndClaimSlotFromBottom();

        for (Stmt statement : statements) {
            statement.dispatch(statement_analyser);
        }

        int main_frame_size = counter_slots.stack_frame_size();

        backend.defineSym(main_frame_size_name, main_frame_size);
        backend.emitLI(A0, EXIT_ECALL, "");
        backend.emitEcall(null);
    }

    /**
     * Emits the code for a function described by FUNCINFO.
     *
     * This method is invoked once per function and method definition.
     * At the code generation stage, nested functions are emitted as
     * separate functions of their own. So if function `bar` is nested within
     * function `foo`, you only emit `foo`'s code for `foo` and only emit
     * `bar`'s code for `bar`.
     */

    protected void emitUserDefinedFunction(FuncInfo funcInfo) {
        backend.emitGlobalLabel(funcInfo.getCodeLabel());

        String function_name = funcInfo.getFuncName();
        String main_frame_size_name = "@" + function_name + "";
        SlotCounter counter_slots = new SlotCounter();
        StmtAnalyzer stmtAnalyzer = new StmtAnalyzer(funcInfo, counter_slots);

        backend.emitADDI(SP, SP, "-" + main_frame_size_name, "");
        counter_slots.allocAndClaimSlotFromBottom();
        backend.emitSW(RA, SP, main_frame_size_name + "-4", "");
        counter_slots.allocAndClaimSlotFromBottom();
        backend.emitSW(FP, SP, main_frame_size_name + "-8", "");
        backend.emitADDI(FP, SP, main_frame_size_name, "");

        List<StackVarInfo> local_variable_list = funcInfo.getLocals();

        int i = 0;
        while (i < local_variable_list.size()) {
            local_variable_list.get(i).getInitialValue().dispatch(stmtAnalyzer);
            backend.emitSW(A0, FP, -(i + 2 + 1) * wordSize,
                    String.format("", local_variable_list.get(i).getVarName()));
            counter_slots.allocAndClaimSlotFromBottom();
            i++;
        }

        for (Stmt statement : funcInfo.getStatements()) {
            statement.dispatch(stmtAnalyzer);
        }

        backend.emitMV(A0, ZERO, "");
        backend.emitLocalLabel(stmtAnalyzer.epilogue, "");

        // FIXME: {... reset fp etc. ...}

        int main_frame_size = counter_slots.stack_frame_size();
        backend.defineSym(main_frame_size_name, main_frame_size);
        backend.emitLW(RA, FP, -wordSize, "");
        backend.emitLW(FP, FP, -2 * wordSize, "");
        backend.emitADDI(SP, SP, main_frame_size_name, "");
        backend.emitJR(RA, "");
    }

    /**
     * An analyzer that encapsulates code generation for statements.
     */
    private class StmtAnalyzer extends AbstractNodeAnalyzer<Void> {
        /*
         * The symbol table has all the info you need to determine
         * what a given identifier 'x' in the current scope is. You can
         * use it as follows:
         * SymbolInfo x = sym.get("x");
         *
         * A SymbolInfo can be one the following:
         * - ClassInfo: a descriptor for classes
         * - FuncInfo: a descriptor for functions/methods
         * - AttrInfo: a descriptor for attributes
         * - GlobalVarInfo: a descriptor for global variables
         * - StackVarInfo: a descriptor for variables allocated on the stack,
         * such as locals and parameters
         *
         * Since the input program is assumed to be semantically
         * valid and well-typed at this stage, you can always assume that
         * the symbol table contains valid information. For example, in
         * an expression `foo()` you KNOW that sym.get("foo") will either be
         * a FuncInfo or ClassInfo, but not any of the other infos
         * and never null.
         *
         * The symbol table in funcInfo has already been populated in
         * the base class: CodeGenBase. You do not need to add anything to
         * the symbol table. Simply query it with an identifier name to
         * get a descriptor for a function, class, variable, etc.
         *
         * The symbol table also maps nonlocal and global vars, so you
         * only need to lookup one symbol table and it will fetch the
         * appropriate info for the var that is currently in scope.
         * 
         * References:
         * https://yangdanny97.github.io/blog/2021/08/26/chocopy-jvm-backend
         * https://www.youtube.com/watch?v=-ti07Z0xKKg&ab_channel=DouglasThain
         * https://github.com/Leo-Enrique-Wu?tab=repositories
         * https://www.tutorialspoint.com/compiler_design/
         * compiler_design_code_generation.htm
         * 
         */

        /** Symbol table for my statements. */
        private final SymbolTable<SymbolInfo> sym;

        /** Label of code that exits from procedure. */
        protected final Label epilogue;

        /** The descriptor for the current function, or null at the top level. */
        private final FuncInfo funcInfo;

        private SlotCounter counter_slot = null;

        /**
         * An analyzer for the function described by FUNCINFO0, which is null for the *
         * top level.
         */

        StmtAnalyzer(FuncInfo funcInfo0, SlotCounter counter_slot) {

            funcInfo = funcInfo0;
            if (funcInfo == null) {
                sym = globalSymbols;
            }

            else {
                sym = funcInfo.getSymbolTable();
            }

            epilogue = generateLocalLabel();
            this.counter_slot = counter_slot;
        }

        // FIXME: Example of statement

        @Override
        public Void analyze(ReturnStmt node) {
            // FIXME: Here, we emit an instruction that does nothing. Clearly,
            // this is wrong, and you'll have to fix it.
            // This is here just to demonstrate how to emit a
            // RISC-V instruction.
            if (node.value != null) {
                node.value.dispatch(this);
            }

            backend.emitJ(epilogue, "");
            return null;

            // FIXME: More, of course.

        }

        @Override
        public Void analyze(ExprStmt node) {
            node.expr.dispatch(this);
            return null;
        }

        @Override
        public Void analyze(BinaryExpr node) {

            switch (node.operator) {
                case ">":
                case ">=":
                case "<":
                case "<=":
                case "!=":
                case "==":
                case "is":

                    node.left.dispatch(this);
                    int left_operand_offset = counter_slot.allocAndClaimSlotFromBottom();
                    Register left_operand_reg = Register.T0;
                    Register right_operand_reg = A0;
                    Label next_label = generateLocalLabel();
                    Label true_outcome_label = generateLocalLabel();
                    Label false_outcome_label = generateLocalLabel();

                    backend.emitSW(A0, FP, left_operand_offset, "");

                    node.right.dispatch(this);

                    backend.emitLW(left_operand_reg, FP, left_operand_offset, "");
                    counter_slot.freeSlot(1);

                    switch (node.operator) {

                        case ">":
                            backend.emitBLT(right_operand_reg, left_operand_reg, true_outcome_label, "");
                            backend.emitJ(false_outcome_label, "");
                            break;

                        case ">=":
                            backend.emitBGE(left_operand_reg, right_operand_reg, true_outcome_label, "");
                            backend.emitJ(false_outcome_label, "");
                            break;

                        case "<":
                            backend.emitBLT(left_operand_reg, right_operand_reg, true_outcome_label, "");
                            backend.emitJ(false_outcome_label, "");
                            break;

                        case "<=":
                            backend.emitBGE(right_operand_reg, left_operand_reg, true_outcome_label, "");
                            backend.emitJ(false_outcome_label, "");
                            break;

                        case "!=":
                            if (Type.STR_TYPE.equals(node.left.getInferredType())) {
                                Label function_label = strneql;
                                counter_slot.allocateSlot();

                                int offset_from_frame_top = wordSize;
                                backend.emitSW(left_operand_reg, SP, offset_from_frame_top,
                                        String.format("", function_label.labelName, 0));

                                counter_slot.allocateSlot();
                                backend.emitSW(right_operand_reg, SP, 0,
                                        String.format("", function_label.labelName, 1));
                                backend.emitJAL(function_label, String.format("", function_label.labelName));

                                counter_slot.freeSlotFromFrameTop(2);
                                backend.emitJ(next_label, "");

                            } else {
                                backend.emitBNE(left_operand_reg, right_operand_reg, true_outcome_label, "");
                                backend.emitJ(false_outcome_label, "");
                            }
                            break;

                        case "==":
                            if (Type.STR_TYPE.equals(node.left.getInferredType())) {
                                Label function_label = streql;
                                counter_slot.allocateSlot();
                                int offset_from_frame_top = wordSize;
                                backend.emitSW(left_operand_reg, SP, offset_from_frame_top,
                                        String.format("", function_label.labelName, 0));
                                counter_slot.allocateSlot();
                                backend.emitSW(right_operand_reg, SP, 0,
                                        String.format("", function_label.labelName, 1));
                                backend.emitJAL(function_label, String.format("", function_label.labelName));
                                counter_slot.freeSlotFromFrameTop(2);
                                backend.emitJ(next_label, "");
                            } else {
                                backend.emitBEQ(left_operand_reg, right_operand_reg, true_outcome_label, "");
                                backend.emitJ(false_outcome_label, "");
                            }
                            break;

                        case "is":
                            backend.emitBEQ(left_operand_reg, right_operand_reg, true_outcome_label, "");
                            backend.emitJ(false_outcome_label, "");
                            break;
                    }

                    backend.emitLocalLabel(true_outcome_label, "");
                    backend.emitADDI(A0, ZERO, 1, "");
                    backend.emitJ(next_label, "");

                    backend.emitLocalLabel(false_outcome_label, "");
                    backend.emitADDI(A0, ZERO, 0, "");
                    backend.emitJ(next_label, "");

                    backend.emitLocalLabel(next_label, "");
                    break;

                case "or":
                    Label True_left_label = generateLocalLabel();
                    node.left.dispatch(this);
                    backend.emitBNEZ(A0, True_left_label, "");
                    node.right.dispatch(this);
                    backend.emitLocalLabel(True_left_label, "");
                    break;

                case "and":
                    Label False_left_label = generateLocalLabel();
                    node.left.dispatch(this);
                    backend.emitBEQZ(A0, False_left_label, String.format("", node.operator));
                    node.right.dispatch(this);
                    backend.emitLocalLabel(False_left_label, String.format("", node.operator));
                    break;

                case "-":

                case "+":
                    if (node.left.getInferredType().isListType() && node.right.getInferredType().isListType()) {
                        generate_list_concat(node);
                    }

                case "*":

                case "//":

                case "%":
                    if (node.left.getInferredType().equals(Type.INT_TYPE)
                            && node.right.getInferredType().equals(Type.INT_TYPE)) {
                        node.left.dispatch(this);
                        int left_offset_result = counter_slot.allocAndClaimSlotFromBottom();
                        backend.emitSW(A0, FP, left_offset_result,
                                String.format("", counter_slot.getFreeSlotFromBottom()));
                        node.right.dispatch(this);
                        backend.emitLW(T0, FP, left_offset_result,
                                String.format("", counter_slot.getFreeSlotFromBottom()));
                        counter_slot.freeSlot(1);
                        switch (node.operator) {
                            case "-":
                                backend.emitSUB(A0, T0, A0, "");
                                break;
                            case "+":
                                backend.emitADD(A0, T0, A0, "");
                                break;
                            case "*":
                                backend.emitMUL(A0, T0, A0, "");
                                break;
                            case "//":
                            case "%":
                                Label non_zero_number = generateLocalLabel();
                                backend.emitBNEZ(A0, non_zero_number, "");
                                backend.emitJ(errorDiv, "");
                                backend.emitLocalLabel(non_zero_number, "");
                                switch (node.operator) {
                                    case "//":
                                        Label sign_different = generateLocalLabel();
                                        Label end_division = generateLocalLabel();
                                        backend.emitXOR(T2, T0, A0, "");
                                        backend.emitBLTZ(T2, sign_different, "");
                                        backend.emitDIV(A0, T0, A0, "");
                                        backend.emitJ(end_division, "");
                                        backend.emitLocalLabel(sign_different, "");
                                        backend.emitSLT(T2, ZERO, A0, "");
                                        backend.emitADD(T2, T2, T2, "");
                                        backend.emitADDI(T2, T2, -1, "");
                                        backend.emitADD(T2, T0, T2, "");
                                        backend.emitDIV(T2, T2, A0, "");
                                        backend.emitADDI(A0, T2, -1, "");
                                        backend.emitLocalLabel(end_division, "");
                                        break;
                                    case "%":
                                        Label result_modulo = generateLocalLabel();
                                        backend.emitREM(T2, T0, A0, "");
                                        backend.emitBEQZ(T2, result_modulo, "");
                                        backend.emitXOR(T3, T2, A0, "");
                                        backend.emitBGEZ(T3, result_modulo, "");
                                        backend.emitADD(T2, T2, A0, "");
                                        backend.emitLocalLabel(result_modulo, "");
                                        backend.emitMV(A0, T2, "");
                                        break;
                                }
                                break;
                        }
                    } else if (node.left.getInferredType().equals(Type.STR_TYPE)
                            && node.right.getInferredType().equals(Type.STR_TYPE)) {
                        if (!("+".equals(node.operator))) {
                            break;
                        }
                        node.left.dispatch(this);

                        int offset_from_fp = counter_slot.allocAndClaimSlotFromBottom();
                        int offset_from_frame_top = wordSize;

                        backend.emitSW(A0, FP, offset_from_fp, "");

                        node.right.dispatch(this);

                        backend.emitLW(T0, FP, offset_from_fp, "");
                        counter_slot.freeSlot(1);

                        counter_slot.allocateSlot();
                        backend.emitSW(T0, SP, offset_from_frame_top, String.format("", strcat.labelName, 0));

                        counter_slot.allocateSlot();
                        backend.emitSW(A0, SP, 0, String.format("", strcat.labelName, 1));
                        backend.emitJAL(strcat, String.format("", strcat.labelName));
                        counter_slot.freeSlotFromFrameTop(2);

                    }
                    break;
            }
            return null;
        }

        @Override
        public Void analyze(BooleanLiteral node) {
            backend.emitLI(A0, node.value ? 1 : 0, "Load " + (node.value ? "True" : "False") + " to A0");
            return null;
        }

        @Override
        public Void analyze(CallExpr node) {
            SymbolInfo class_sym_info = this.sym.get(node.function.name);

            if (class_sym_info instanceof ClassInfo) {
                ClassInfo class_info = (ClassInfo) class_sym_info;

                if (class_info.getClassName().equals("int") || class_info.getClassName().equals("bool")) {
                    backend.emitMV(A0, ZERO, "");
                    return null;
                }

                counter_slot.allocateSlot();

                backend.emitLA(A0, class_info.getPrototypeLabel(), String.format("", class_info.getClassName()));
                backend.emitJAL(objectAllocLabel, String.format("", class_info.getClassName()));
                int store_new_object_offset = counter_slot.allocAndClaimSlotFromBottom();
                backend.emitSW(A0, FP, store_new_object_offset,
                        String.format("", class_info.getClassName(), counter_slot.getFreeSlotFromBottom()));
                backend.emitSW(A0, SP, 0, "");
                backend.emitLW(A1, A0, 2 * wordSize, "");
                backend.emitLW(A1, A1, 0, String.format("", class_info.getMethods().get(0).getFuncName()));
                backend.emitJALR(A1, String.format("", class_info.getMethods().get(0).getFuncName()));
                backend.emitLW(A0, FP, store_new_object_offset,
                        String.format("", class_info.getClassName(), counter_slot.getFreeSlotFromBottom()));

            }

            int arg_index = 0;
            List<Integer> temporary_offset = new ArrayList<>();
            Identifier func_ID = node.function;
            FuncType func_Type = null;
            String func_Name = func_ID.name;
            SymbolInfo func_Sym_Info = this.sym.get(func_Name);
            FuncInfo funcInfo = null;
            List<Expr> argument_expressions = node.args;
            int arg_number = argument_expressions.size();

            if (!(func_ID.getInferredType() instanceof FuncType)) {
                return null;
            } else {
                func_Type = (FuncType) func_ID.getInferredType();
            }

            List<ValueType> formal_parameters = func_Type.parameters;

            if (!(func_Sym_Info instanceof FuncInfo)) {
                return null;
            } else {
                funcInfo = (FuncInfo) func_Sym_Info;
            }

            backend.emitInsn("", String.format("", funcInfo.getFuncName()));

            for (Expr argExpr : argument_expressions) {

                ValueType formalParamType = formal_parameters.get(arg_index);

                argExpr.dispatch(this);

                Type argType = argExpr.getInferredType();
                if (Type.OBJECT_TYPE.equals(formalParamType)
                        && (Type.INT_TYPE.equals(argType) || Type.BOOL_TYPE.equals(argType))) {
                    A0_autobox(argType);
                }

                int offsetFromFp = counter_slot.allocAndClaimSlotFromBottom();
                temporary_offset.add(offsetFromFp);
                backend.emitSW(A0, FP, offsetFromFp, String.format("", arg_index));
                arg_index++;

            }

            Collections.reverse(temporary_offset);
            int offsetFromSp = 0;
            arg_index = arg_number - 1;
            for (Integer tempVarOffsetFromFp : temporary_offset) {
                backend.emitLW(T0, FP, tempVarOffsetFromFp, String.format("", arg_index));
                counter_slot.freeSlot(1);
                counter_slot.allocateSlot();
                backend.emitSW(T0, SP, offsetFromSp, String.format("", arg_index));
                offsetFromSp += wordSize;
            }

            boolean isNestedFunc = (funcInfo.getDepth() > 0) ? true : false;
            if (isNestedFunc) {
                int offset_from_frame_top = arg_number * wordSize;
                arg_number++;
                counter_slot.allocateSlot();
                FuncInfo currentFuncInfo = this.funcInfo;
                backend.emitMV(T0, FP, "Static link to " + currentFuncInfo.getFuncName());
                while (funcInfo.getParentFuncInfo().getFuncName() != currentFuncInfo.getFuncName()) {
                    backend.emitLW(T0, T0, currentFuncInfo.getParams().size() * wordSize,
                            "" + currentFuncInfo.getFuncName() + " to "
                                    + currentFuncInfo.getParentFuncInfo().getFuncName());
                    currentFuncInfo = currentFuncInfo.getParentFuncInfo();
                }
                backend.emitSW(T0, SP, offset_from_frame_top, "");
            }

            Label func_Label = funcInfo.getCodeLabel();
            backend.emitJAL(func_Label, String.format("", func_Label.labelName));
            counter_slot.freeSlotFromFrameTop(arg_number);

            return null;

        }

        @Override
        public Void analyze(AssignStmt node) {

            node.value.dispatch(this);

            for (Expr expression : node.targets) {
                if (expression instanceof Identifier) {
                    variableAccessGenerator((Identifier) expression, false);
                }

                else if (expression instanceof IndexExpr) {
                    IndexExpr indexExpression = (IndexExpr) expression;

                    final int OffsetValueToBeAssigned = counter_slot.allocAndClaimSlotFromBottom();

                    backend.emitSW(A0, FP, OffsetValueToBeAssigned, "");

                    generate_code_list(indexExpression);

                    backend.emitLW(A0, FP, OffsetValueToBeAssigned, "");
                    backend.emitSW(A0, T0, 0, "");

                    counter_slot.freeSlot(1);
                }

                else {
                    MemberExpr memberExpression = (MemberExpr) expression;

                    final int OffsetValueToBeAssigned = counter_slot.allocAndClaimSlotFromBottom();

                    backend.emitSW(A0, FP, OffsetValueToBeAssigned, "");

                    memberExpression.object.dispatch(this);

                    Label notNoneLabel = generateLocalLabel();

                    backend.emitBNEZ(A0, notNoneLabel, "");
                    backend.emitJ(errorNone, "");
                    backend.emitLocalLabel(notNoneLabel, "");

                    ClassInfo classInformation = (ClassInfo) sym
                            .get(memberExpression.object.getInferredType().className());
                    final int attributeIndex = classInformation.getAttributeIndex(memberExpression.member.name);

                    backend.emitLW(A1, FP, OffsetValueToBeAssigned, "");
                    backend.emitSW(A1, A0, (attributeIndex + HEADER_SIZE) * wordSize, "");
                    backend.emitMV(A0, A1, "");

                    counter_slot.freeSlot(1);
                }
            }
            return null;
        }

        @Override
        public Void analyze(StringLiteral node) {
            String strValue = node.value;
            Label strLabel = constants.getStrConstant(strValue);
            backend.emitLA(A0, strLabel, "");
            return null;

        }

        @Override
        public Void analyze(Identifier node) {
            variableAccessGenerator(node, true);
            return null;

        }

        @Override
        public Void analyze(IntegerLiteral node) {
            backend.emitLI(A0, node.value, String.format("", node.value));
            return null;

        }

        @Override
        public Void analyze(NoneLiteral node) {
            backend.emitMV(A0, ZERO, "");
            return null;
        }

        @Override
        public Void analyze(WhileStmt node) {
            Label test_label_iter = generateLocalLabel();
            Label iter_label = generateLocalLabel();

            backend.emitJ(test_label_iter, "");
            backend.emitLocalLabel(iter_label, "");

            for (Stmt statement : node.body) {
                statement.dispatch(this);
            }
            backend.emitLocalLabel(test_label_iter, "");

            node.condition.dispatch(this);

            backend.emitBNEZ(A0, iter_label, "");

            return null;
        }

        @Override
        public Void analyze(MethodCallExpr node) {
            int i = 0;
            String method_object_name = node.method.object.getInferredType().className();
            String method_name = node.method.member.name;
            int address_method_offset = counter_slot.allocAndClaimSlotFromBottom();
            int offset_object = counter_slot.allocAndClaimSlotFromBottom();
            FuncType function_type = (FuncType) node.method.getInferredType();
            List<Expr> arguments = node.args;
            List<ValueType> parameters = function_type.parameters;
            List<Integer> temp_var_from_fp = new ArrayList<>();
            int num_var = arguments.size();
            int offset_from_sp = 0;

            node.method.dispatch(this);
            backend.emitSW(A1, FP, address_method_offset,
                    String.format("", method_object_name, method_name, counter_slot.getFreeSlotFromBottom()));
            backend.emitSW(A0, FP, offset_object,
                    String.format("", method_object_name, counter_slot.getFreeSlotFromBottom()));

            assert function_type != null : "";
            assert parameters.size() == arguments.size() + 1 : "";
            backend.emitInsn("", String.format("", method_object_name, method_name));

            for (Expr argument_expression : arguments) {
                ValueType parameter_type = parameters.get(i + 1);
                argument_expression.dispatch(this);
                Type type_argument = argument_expression.getInferredType();
                if (Type.OBJECT_TYPE.equals(parameter_type)
                        && (Type.INT_TYPE.equals(type_argument) || Type.BOOL_TYPE.equals(type_argument))) {
                    A0_autobox(type_argument);
                }
                int offset_from_fp = counter_slot.allocAndClaimSlotFromBottom();
                temp_var_from_fp.add(offset_from_fp);
                backend.emitSW(A0, FP, offset_from_fp, String.format("", i, counter_slot.getFreeSlotFromBottom()));
                i++;

            }

            Collections.reverse(temp_var_from_fp);

            i = num_var - 1;
            for (Integer offset_var_from_fp : temp_var_from_fp) {
                backend.emitLW(T0, FP, offset_var_from_fp,
                        String.format("", i));
                counter_slot.freeSlot(1);
                counter_slot.allocateSlot();
                backend.emitSW(T0, SP, offset_from_sp,
                        String.format("", i));
                offset_from_sp += wordSize;

            }

            backend.emitLW(T0, FP, offset_object,
                    String.format("", method_object_name, counter_slot.getFreeSlotFromBottom()));
            counter_slot.allocateSlot();
            backend.emitSW(T0, SP, num_var * wordSize, String.format(""));
            backend.emitLW(A1, FP, address_method_offset,
                    String.format("", method_object_name, method_name, counter_slot.getFreeSlotFromBottom()));
            backend.emitJALR(A1, String.format("", method_object_name, method_name));
            counter_slot.freeSlotFromFrameTop(num_var + 1);
            counter_slot.freeSlot(2);
            return null;
        }

        @Override
        public Void analyze(IfStmt node) {
            Expr node_condition = node.condition;
            node_condition.dispatch(this);

            boolean exitElseBody = !node.elseBody.isEmpty();
            Label elseBranch = exitElseBody ? generateLocalLabel() : null;
            Label endIf = generateLocalLabel();

            if (exitElseBody) {
                backend.emitBEQZ(A0, elseBranch, "");
            }

            else {
                backend.emitBEQZ(A0, endIf, "");
            }

            for (Stmt statement : node.thenBody) {
                statement.dispatch(this);
            }

            backend.emitJ(endIf, "");

            if (exitElseBody) {
                backend.emitLocalLabel(elseBranch, "");
                for (Stmt statement : node.elseBody) {
                    statement.dispatch(this);
                }
            }

            backend.emitLocalLabel(endIf, "");
            return null;
        }

        @Override
        public Void analyze(UnaryExpr node) {
            Expr operand_Expression = node.operand;
            assert (node.operator.equals("-") || node.operator.equals("not")) : "";

            switch (node.operator) {
                case "not":
                    operand_Expression.dispatch(this);
                    backend.emitSEQZ(A0, A0, "");
                    break;

                case "-":
                    operand_Expression.dispatch(this);
                    backend.emitSUB(A0, ZERO, A0, "");
                    break;

                default:
            }
            return null;
        }

        @Override
        public Void analyze(MemberExpr node) {
            Label check_None = generateLocalLabel();
            Identifier node_member = node.member;
            String object_name = node.object.getInferredType().className();
            SymbolInfo class_info = this.sym.get(object_name);

            node.object.dispatch(this);
            backend.emitBNEZ(A0, check_None, "");
            backend.emitJ(errorNone, "");
            backend.emitLocalLabel(check_None, "");

            assert class_info instanceof ClassInfo : "";
            ClassInfo object_class_info = (ClassInfo) class_info;

            if (node.getInferredType().isFuncType()) {
                int index_method = object_class_info.getMethodIndex(node_member.name);
                backend.emitLW(A1, A0, 2 * wordSize, String.format("", object_name));
                backend.emitLW(A1, A1, index_method * wordSize,
                        String.format("", object_name, node_member.name));

            } else {
                int index_attribute = object_class_info.getAttributeIndex(node_member.name);
                backend.emitLW(A0, A0, (index_attribute + 3) * wordSize,
                        String.format("", object_name, node_member.name));
            }

            return null;
        }

        @Override
        public Void analyze(IndexExpr node) {
            Type list_type = node.list.getInferredType();

            if (list_type.isListType()) {
                generate_code_list(node);
                backend.emitLW(A0, T0, 0, "");
            } else {
                generateStringAccessCode(node);
            }
            return null;
        }

        @Override
        public Void analyze(IfExpr node) {
            Label next_condition = generateLocalLabel();
            Label false_condition = generateLocalLabel();

            Expr node_condition = node.condition;
            node_condition.dispatch(this);

            backend.emitBEQZ(A0, false_condition, "");
            backend.emitInsn("", "");

            Expr block1 = node.thenExpr;
            block1.dispatch(this);

            backend.emitJ(next_condition, "");
            backend.emitLocalLabel(false_condition, "");

            Expr block2 = node.elseExpr;
            block2.dispatch(this);

            backend.emitLocalLabel(next_condition, "");
            return null;
        }

        @Override
        public Void analyze(ListExpr node) {
            if (node.elements.isEmpty()) {
                backend.emitLA(A0, listClass.getPrototypeLabel(), "");
                return null;
            }
            final int num_element = node.elements.size();

            List<Integer> offset_list = new ArrayList<>(num_element);

            for (Expr expression : node.elements) {
                final int check_offset = counter_slot.allocAndClaimSlotFromBottom();
                expression.dispatch(this);

                backend.emitSW(A0, FP, check_offset, " " + check_offset + "");

                offset_list.add(check_offset);
            }

            final int objectSize = HEADER_SIZE + 1 + num_element;
            backend.emitLI(A1, objectSize, "" + objectSize + " ");

            backend.emitLA(A0, listClass.getPrototypeLabel(), "");
            backend.emitJAL(objectAllocResizeLabel, "");
            backend.emitLI(T0, num_element, "" + num_element + "");
            backend.emitSW(T0, A0, "@.__len__", "");

            for (int j = 0, offsetInList = HEADER_SIZE + 1; j < num_element; j++, offsetInList++) {
                backend.emitLW(T0, FP, offset_list.get(j), "" + (j + 1) + "");
                backend.emitSW(T0, A0, offsetInList * wordSize, "" + (j + 1) + "");
            }
            counter_slot.freeSlot(num_element);
            return null;
        }

        @Override
        public Void analyze(ForStmt node) {
            Label test_label_index = generateLocalLabel();
            Label end_label_index = generateLocalLabel();
            Label body_label_index = generateLocalLabel();

            backend.emitLI(T0, 0, "");
            int offset_index = counter_slot.allocAndClaimSlotFromBottom();
            int slot_index = counter_slot.getFreeSlotFromBottom();
            int offset_loop = counter_slot.allocAndClaimSlotFromBottom();
            int slot_loop = counter_slot.getFreeSlotFromBottom();

            backend.emitSW(T0, FP, offset_index, String.format("", slot_index));
            node.iterable.dispatch(this);

            backend.emitSW(A0, FP, offset_loop, String.format("", slot_loop));
            backend.emitJ(test_label_index, "");
            backend.emitLocalLabel(body_label_index, "");

            for (Stmt statement : node.body) {
                statement.dispatch(this);
            }

            backend.emitLocalLabel(test_label_index, "");

            Type type_list = node.iterable.getInferredType();

            if (type_list.isListType()) {
                generate_code_list(null, null, slot_loop, slot_index, end_label_index, "Go to for loop end label");
                backend.emitLW(A0, T0, 0, "");
            } else {
                generateStringAccessCode(null, null, slot_loop, slot_index, end_label_index,
                        "Go to for loop end label");
            }

            variableAccessGenerator(node.identifier, false);

            backend.emitLW(T0, FP, offset_index, String.format("", slot_index));
            backend.emitADDI(T0, T0, 1, "");
            backend.emitSW(T0, FP, offset_index, String.format("", slot_index));
            backend.emitJ(body_label_index, "");
            backend.emitLocalLabel(end_label_index, "");
            return null;
        }

        private void generateStringAccessCode(Expr arr_list, Expr index_expression, int slot_string, int slot_index,
                Label special_label,
                String comment_string) {
            Label valid_index_label = generateLocalLabel();
            if (arr_list != null) {
                slot_string = counter_slot.getFreeSlotFromBottom();
                counter_slot.allocAndClaimSlotFromBottom();
                arr_list.dispatch(this);
                backend.emitSW(A0, FP, -slot_string * wordSize, String.format("", slot_string));
            }

            if (index_expression != null) {
                index_expression.dispatch(this);
            }

            else {
                backend.emitLW(A0, FP, -slot_index * wordSize, String.format("", slot_index));
            }

            counter_slot.freeSlot(1);

            backend.emitLW(A1, FP, -slot_string * wordSize, String.format("", slot_string));
            backend.emitLW(T0, A1, "@.__len__", String.format(""));
            backend.emitBLTU(A0, T0, valid_index_label, "");
            backend.emitJ(special_label, comment_string);
            backend.emitLocalLabel(valid_index_label, "");
            backend.emitLW(A1, FP, -slot_string * wordSize, String.format("", slot_string));

            backend.emitADDI(A0, A0, "@.__str__", "");
            backend.emitLI(T1, 20, "");
            backend.emitADD(A0, A1, A0, "");
            backend.emitLBU(A0, A0, 0, "");

            backend.emitMUL(A0, A0, T1, "");
            backend.emitLA(T0, allChars, "");
            backend.emitADD(A0, T0, A0, "");

        }

        private void generate_code_list(IndexExpr expression) {
            generate_code_list(expression.list, expression.index, -1, -1, errorOob,
                    "Go to out-of-bounds error and abort");
        }

        private void A0_autobox(Type autobox_type) {
            if (Type.INT_TYPE.equals(autobox_type)) {
                counter_slot.allocateSlot();
                counter_slot.freeSlotFromFrameTop(1);

                backend.emitSW(A0, SP, 0, String.format("", 0));
                backend.emitInsn("", "");
                backend.emitJAL(Integer_type, String.format("", Integer_type.labelName));
            }

            else if (Type.BOOL_TYPE.equals(autobox_type)) {
                counter_slot.freeSlotFromFrameTop(1);
                counter_slot.allocateSlot();

                backend.emitSW(A0, SP, 0, String.format("", 0));
                backend.emitInsn("", "");
                backend.emitJAL(boolean_type, String.format("", boolean_type.labelName));
            }
        }

        private void variableAccessGenerator(Identifier node, boolean load_op) {
            SymbolInfo info_symbol = sym.get(node.name);
            if (info_symbol instanceof StackVarInfo) {
                StackVarInfo stack_variable_info = (StackVarInfo) info_symbol;
                FuncInfo var_info = stack_variable_info.getFuncInfo();
                if (funcInfo != var_info) {
                    final int check_nested = funcInfo.getDepth() - var_info.getDepth();
                    FuncInfo parent_function = funcInfo.getParentFuncInfo();

                    backend.emitLW(T0, FP, funcInfo.getParams().size() * wordSize,
                            "" + funcInfo.getFuncName() + " to " + parent_function.getFuncName());

                    for (int j = 0; j < check_nested - 1; j++) {
                        backend.emitLW(T0, T0, parent_function.getParams().size() * wordSize,
                                " " + parent_function.getFuncName() + " to "
                                        + parent_function.getParentFuncInfo().getFuncName());
                        parent_function = parent_function.getParentFuncInfo();
                    }

                    final int check_final_int = parent_function.getVarIndex(stack_variable_info.getVarName())
                            - parent_function.getParams().size() + 1;

                    if (load_op) {
                        backend.emitLW(A0, T0, -check_final_int * wordSize,
                                String.format("", parent_function.getFuncName(), stack_variable_info.getVarName()));
                    } else {
                        backend.emitSW(A0, T0, -check_final_int * wordSize, String.format("",
                                stack_variable_info.getFuncInfo().getFuncName(), stack_variable_info.getVarName()));
                    }
                }

                else {
                    final int check_final_int = funcInfo.getVarIndex(stack_variable_info.getVarName())
                            - funcInfo.getParams().size() + 1;
                    if (load_op) {
                        backend.emitLW(A0, FP, -check_final_int * wordSize,
                                " " + stack_variable_info.getVarName() + " ");
                    } else {
                        backend.emitSW(A0, FP, -check_final_int * wordSize, " " + stack_variable_info.getVarName());
                    }
                }

            } else {
                GlobalVarInfo globalVarInfo = (GlobalVarInfo) info_symbol;
                if (load_op) {
                    backend.emitLW(A0, globalVarInfo.getLabel(), " " + globalVarInfo.getVarName());
                } else {
                    backend.emitSW(A0, globalVarInfo.getLabel(), T0, "" + globalVarInfo.getLabel().labelName + "");
                }
            }
        }

        private void generate_code_list(Expr expression_list, Expr expression_index, int slot_list, int slot_index,
                Label label, String comment) {
            if (expression_list != null) {
                expression_list.dispatch(this);
            } else {
                backend.emitLW(A0, FP, -slot_list * wordSize, String.format("", slot_list));
            }

            Label check_label_none = generateLocalLabel();
            backend.emitBNEZ(A0, check_label_none, "");
            backend.emitJ(errorNone, "");

            backend.emitLocalLabel(check_label_none, "");

            final int list_offset = counter_slot.allocAndClaimSlotFromBottom();
            backend.emitSW(A0, FP, list_offset, "");
            if (expression_index != null) {
                expression_index.dispatch(this);
            } else {
                backend.emitLW(A0, FP, -slot_index * wordSize, String.format("", slot_index));
            }

            Label valid_index_label = generateLocalLabel();

            backend.emitLW(T0, FP, list_offset, "");
            backend.emitLW(T1, T0, "@.__len__", "");
            backend.emitBLTU(A0, T1, valid_index_label, "");
            backend.emitJ(label, comment);

            backend.emitLocalLabel(valid_index_label, "");
            backend.emitADDI(A0, A0, HEADER_SIZE + 1, "");
            backend.emitLI(T1, wordSize, "");
            backend.emitMUL(A0, A0, T1, "");
            backend.emitADD(T0, T0, A0, "");

            counter_slot.freeSlot(1);
        }

        private void generateStringAccessCode(IndexExpr expression) {
            generateStringAccessCode(expression.list, expression.index, -1, -1, errorOob,
                    "Go to out-of-bounds error and abort");
        }

        private void generate_list_concat(BinaryExpr expression) {
            emit_concat_flag = true;
            expression.left.dispatch(this);
            counter_slot.allocateSlot();
            final int array_list_offset_left = counter_slot.allocAndClaimSlotFromBottom();
            backend.emitSW(A0, FP, array_list_offset_left, "");
            expression.right.dispatch(this);
            backend.emitLW(A1, FP, array_list_offset_left, "");

            backend.emitSW(A1, SP, wordSize, "");
            counter_slot.allocateSlot();
            backend.emitSW(A0, SP, 0, "");
            counter_slot.allocateSlot();

            backend.emitJAL(concat_list_label, "");

            counter_slot.freeSlotFromFrameTop(2);

        }

    }

    /**
     * Emits custom code in the CODE segment.
     *
     * This method is called after emitting the top level and the
     * function bodies for each function.
     *
     * You can use this method to emit anything you want outside of the
     * top level or functions, e.g. custom routines that you may want to
     * call from within your code to do common tasks. This is not strictly
     * needed. You might not modify this at all and still complete
     * the assignment.
     *
     * To start you off, here is an implementation of three routines that
     * will be commonly needed from within the code you will generate
     * for statements.
     *
     * The routines are error handlers for operations on None, index out
     * of bounds, and division by zero. They never return to their caller.
     * Just jump to one of these routines to throw an error and
     * exit the program. For example, to throw an OOB error:
     * backend.emitJ(errorOob, "Go to out-of-bounds error and abort");
     *
     */

    private void emitfunction_string_notequal() {
        string_compare_function(strneql, false);
    }

    private void emitFuncInteger_type() {
        backend.emitGlobalLabel(Integer_type);

        int offset_from_frame_top = 0;
        int frame_length = 2 * wordSize;

        backend.emitADDI(SP, SP, -1 * frame_length, "");
        backend.emitSW(FP, SP, offset_from_frame_top, "");

        offset_from_frame_top += wordSize;
        backend.emitSW(RA, SP, offset_from_frame_top, "");

        offset_from_frame_top += wordSize;
        backend.emitADDI(FP, SP, frame_length, "");

        backend.emitADDI(SP, FP, Integer.valueOf(0), "");

        backend.emitLA(A0, intClass.getPrototypeLabel(), "");
        backend.emitJAL(objectAllocLabel, "");

        backend.emitLW(Register.T0, FP, Integer.valueOf(0), "");
        backend.emitSW(Register.T0, A0, getAttrOffset(intClass, "__int__"), "");

        backend.emitLW(RA, FP, -1 * wordSize, "");
        backend.emitLW(FP, FP, -2 * wordSize, "");
        backend.emitJR(RA, "");

    }

    protected void emitCustomCode() {
        emitStdFunc("initChars");
        emitStdFunc("allChars");
        emitFuncboolean_type();

        emitFuncInteger_type();
        emitfunction_string_equal();

        emit_function_string_cat();
        emitfunction_string_notequal();

        if (emit_concat_flag) {
            emitfunction_list_concat();
        }

        emitErrorFunc(errorNone, ERROR_NONE, "Operation on None");
        emitErrorFunc(errorDiv, ERROR_DIV_ZERO, "Division by zero");
        emitErrorFunc(errorOob, ERROR_OOB, "Index out of bounds");

    }

    private class SlotCounter {

        private int bottom_slot_free = 0;

        private int curr_slot_num = 0;

        private int max_slot_num = 0;

        public void allocateSlot() {
            curr_slot_num++;
            max_slot_num = Math.max(max_slot_num, curr_slot_num);
        }

        public int getFreeSlotFromBottom() {
            return bottom_slot_free;
        }

        public int allocAndClaimSlotFromBottom() {
            allocateSlot();
            bottom_slot_free++;
            return (-1 * bottom_slot_free * wordSize);
        }

        public void freeSlotFromFrameTop(int popoff_slot_num) {
            decreaseSlotNumber(popoff_slot_num);
        }

        public void freeSlot(int popoff_slot_num) {
            bottom_slot_free -= popoff_slot_num;
            decreaseSlotNumber(popoff_slot_num);
        }

        public int stack_frame_size() {
            int frame_length = ((max_slot_num * wordSize) % 16 == 0) ? max_slot_num * wordSize
                    : (((max_slot_num * wordSize) / 16) + 1) * 16;
            return frame_length;
        }

        private void decreaseSlotNumber(int popoff_slot_num) {
            curr_slot_num -= popoff_slot_num;
        }

    }

    private void emitFuncboolean_type() {
        backend.emitGlobalLabel(boolean_type);

        int offset_from_frame_top = 0;
        int frame_length = 2 * wordSize;

        backend.emitADDI(SP, SP, -1 * frame_length, "");
        backend.emitSW(FP, SP, offset_from_frame_top, "");

        offset_from_frame_top += wordSize;
        backend.emitSW(RA, SP, offset_from_frame_top, "");

        offset_from_frame_top += wordSize;
        backend.emitADDI(FP, SP, frame_length, "");

        Label false_return = new Label("label");
        Label epilogue = new Label("epilogue");

        backend.emitBEQZ(A0, false_return, String.format("", false_return.labelName));

        Label True_boolean = this.constants.getBoolConstant(true);

        backend.emitLA(A0, True_boolean, String.format("", True_boolean.labelName));
        backend.emitJ(epilogue, String.format("", epilogue.labelName));
        backend.emitLocalLabel(false_return, "");

        Label constantFalseTrueLabel = this.constants.getBoolConstant(false);

        backend.emitLA(A0, constantFalseTrueLabel, String.format("", constantFalseTrueLabel.labelName));
        backend.emitJ(epilogue, String.format("", epilogue.labelName));
        backend.emitLocalLabel(epilogue, "");
        backend.emitADDI(SP, FP, Integer.valueOf(0), "");
        backend.emitLW(RA, FP, -1 * wordSize, "");
        backend.emitLW(FP, FP, -2 * wordSize, "");
        backend.emitJR(RA, "");

    }

    private void emit_function_string_cat() {

        backend.emitGlobalLabel(strcat);

        backend.emitADDI(SP, SP, -16, "");
        backend.emitSW(RA, SP, 12, "");
        backend.emitSW(FP, SP, 8, "");
        backend.emitADDI(FP, SP, 16, "");

        Label arg_empty = new Label("arg_empty");
        Label empty_string_argument = new Label("empty_string_argument");

        backend.emitLW(Register.T0, FP, 4, "");
        backend.emitLW(Register.T1, Register.T0, SYM_NAME_OFFSET_LEN, "");
        backend.emitLW(Register.T2, FP, 0, "");
        backend.emitBEQZ(T1, arg_empty, "");
        backend.emitLW(Register.T3, Register.T2, SYM_NAME_OFFSET_LEN, "");
        backend.emitBEQZ(T3, empty_string_argument, "");
        backend.emitADD(Register.T0, Register.T1, Register.T3, "");
        backend.emitADDI(Register.T0, Register.T0, 1, "");
        backend.emitSRLI(A1, Register.T0, 2, "");
        backend.emitSLLI(Register.T1, A1, 2, "");
        backend.emitSUB(Register.T0, Register.T0, Register.T1, "");

        Label multiplying_with_four = new Label("multiplying_with_four");

        backend.emitBEQ(Register.T0, ZERO, multiplying_with_four, "");
        backend.emitADDI(A1, A1, 5, "");

        Label next_object_size_calc = new Label("next_object_size_calc");

        backend.emitJ(next_object_size_calc, "");
        backend.emitLocalLabel(multiplying_with_four, "");
        backend.emitADDI(A1, A1, 4, "");
        backend.emitLocalLabel(next_object_size_calc, "");
        backend.emitLA(A0, strPrototype, "");
        backend.emitJAL(objectAllocResizeLabel, "");
        backend.emitLW(Register.T0, FP, 4, "");
        backend.emitLW(Register.T1, Register.T0, SYM_NAME_OFFSET_LEN, "");
        backend.emitLW(Register.T2, FP, 0, "");
        backend.emitLW(Register.T3, Register.T2, SYM_NAME_OFFSET_LEN, "");
        backend.emitADD(Register.T4, Register.T1, Register.T3, "");
        backend.emitSW(Register.T4, A0, SYM_NAME_OFFSET_LEN, "");
        backend.emitMV(A1, Register.T1, "");
        backend.emitADDI(T0, T0, SYM_NAME_OFFSET_STR, "");
        backend.emitADDI(T4, A0, SYM_NAME_OFFSET_STR, "");

        Label load_string = new Label("load_string");

        backend.emitJAL(load_string, "");
        backend.emitADDI(T0, T2, SYM_NAME_OFFSET_STR, "");
        backend.emitMV(T1, T3, "");
        backend.emitADDI(T2, A1, SYM_NAME_OFFSET_STR, "");
        backend.emitADD(T4, A0, T2, "");
        backend.emitJAL(load_string, "");
        backend.emitSB(ZERO, T4, 0, "");

        Label epilogue = new Label("second_epilogue");

        backend.emitJ(epilogue, "");
        backend.emitLocalLabel(arg_empty, "");
        backend.emitMV(A0, T2, "");
        backend.emitJ(epilogue, "");

        backend.emitLocalLabel(empty_string_argument, "");
        backend.emitMV(A0, T0, "");
        backend.emitJ(epilogue, "");

        Label keep_end = new Label("keep_end");

        backend.emitLocalLabel(epilogue, null);
        backend.emitMV(SP, FP, null);
        backend.emitLW(RA, FP, -4, null);
        backend.emitLW(FP, FP, -8, null);
        backend.emitJR(RA, null);

        backend.emitLocalLabel(load_string, null);
        backend.emitBEQ(T1, ZERO, keep_end, null);
        backend.emitLB(T5, T0, 0, null);
        backend.emitSB(T5, T4, 0, null);
        backend.emitADDI(T0, T0, 1, null);
        backend.emitADDI(T4, T4, 1, null);
        backend.emitADDI(T1, T1, -1, null);
        backend.emitJ(load_string, null);

        backend.emitLocalLabel(keep_end, null);
        backend.emitJR(RA, null);
    }

    private void emitfunction_string_equal() {
        string_compare_function(streql, true);
    }

    private void emitfunction_list_concat() {
        backend.emitGlobalLabel(concat_list_label);

        backend.emitADDI(SP, SP, -4 * wordSize, "");
        backend.emitSW(RA, SP, 3 * wordSize, "");
        backend.emitSW(FP, SP, 2 * wordSize, "");
        backend.emitADDI(FP, SP, 4 * wordSize, "");

        Label label_none = new Label(concat_list_label.labelName + "_none");
        backend.emitLW(A0, FP, wordSize, "");
        backend.emitBEQZ(A0, label_none, "");

        backend.emitLW(A1, FP, 0, "");
        backend.emitBEQZ(A1, label_none, "");

        backend.emitLW(T0, A0, "@.__len__", "");
        backend.emitLW(T1, A1, "@.__len__", "");

        backend.emitADD(T2, T0, T1, "");

        Label label_done = new Label(concat_list_label.labelName + "label_done");
        Label check_label_empty_left = new Label(concat_list_label.labelName + "check_label_leftEmpty");

        backend.emitBEQ(T2, T1, check_label_empty_left, "");
        backend.emitBEQ(T2, T0, label_done, "");

        backend.emitSW(A0, FP, -3 * wordSize, "");
        backend.emitSW(A1, FP, -4 * wordSize, "");

        backend.emitLA(A0, listClass.getPrototypeLabel(), "");
        backend.emitADDI(A1, T2, HEADER_SIZE + 1, "");
        backend.emitJAL(objectAllocResizeLabel, "");

        backend.emitLW(A1, FP, -3 * wordSize, "");
        backend.emitLW(A2, FP, -4 * wordSize, "");

        backend.emitLW(T1, A1, "@.__len__", "");
        backend.emitLW(T2, A2, "@.__len__", "");
        backend.emitADD(T0, T1, T2, "");
        backend.emitSW(T0, A0, "@.__len__", "");

        backend.emitADDI(T3, A0, "@.__elts__", "");
        backend.emitADDI(A3, A1, "@.__elts__", "");
        backend.emitMV(T0, ZERO, "");

        Label label_copy = new Label(concat_list_label.labelName + "label_copy");
        backend.emitLocalLabel(label_copy, "");
        backend.emitLW(T4, A3, 0, "");
        backend.emitSW(T4, T3, 0, "");
        backend.emitADDI(T0, T0, 1, "");
        backend.emitADDI(A3, A3, wordSize, "");
        backend.emitADDI(T3, T3, wordSize, "");
        backend.emitBNE(T0, T1, label_copy, "");

        backend.emitMV(T0, ZERO, "");
        backend.emitADDI(A3, A2, "@.__elts__", "");

        Label label_right_copy = new Label(concat_list_label.labelName + "label_copyRight");
        backend.emitLocalLabel(label_right_copy, "");
        backend.emitLW(T4, A3, 0, "");
        backend.emitSW(T4, T3, 0, "");
        backend.emitADDI(T0, T0, 1, "");
        backend.emitADDI(A3, A3, wordSize, "");
        backend.emitADDI(T3, T3, wordSize, "");
        backend.emitBNE(T0, T2, label_right_copy, "");

        backend.emitJ(label_done, "");

        backend.emitLocalLabel(label_none, "");
        backend.emitJ(errorNone, "");

        backend.emitLocalLabel(check_label_empty_left, "");
        backend.emitMV(A0, A1, "");

        backend.emitLocalLabel(label_done, "");

        backend.emitLW(RA, FP, -1 * wordSize, "");
        backend.emitLW(FP, FP, -2 * wordSize, "");
        backend.emitADDI(SP, SP, 4 * wordSize, "");
        backend.emitJR(RA, "");

    }

    private void emitErrorFunc(Label label_error, int code_error, String message) {
        backend.emitGlobalLabel(label_error);
        backend.emitLI(A0, code_error, "" + message);
        backend.emitLA(A1, constants.getStrConstant(message), "");
        backend.emitADDI(A1, A1, getAttrOffset(strClass, "__str__"), "");
        backend.emitJ(abortLabel, "");
    }

    private void string_compare_function(Label function_label, boolean check_equal) {
        String func_name = function_label.labelName;
        backend.emitGlobalLabel(function_label);
        Label epilogue = new Label(String.format("%s.epilogue", func_name));
        Label check_result_False = new Label(String.format("%s.check_result_False", func_name));
        Label check_result_true = new Label(String.format("%s.check_result_true", func_name));
        Label equal = null;
        Label check_not_equal = null;
        if (check_equal) {
            equal = check_result_true;
            check_not_equal = check_result_False;
        } else {
            equal = check_result_False;
            check_not_equal = check_result_true;
        }

        Label Byte_compare = new Label(String.format("%s.Byte_compare", func_name));

        backend.emitADDI(SP, SP, -16, String.format("", func_name));
        backend.emitSW(RA, SP, 12, "");
        backend.emitSW(FP, SP, 8, "");
        backend.emitADDI(FP, SP, 16, "");
        backend.emitLW(T0, FP, 4, "");
        backend.emitLW(T1, FP, 0, "");
        backend.emitLW(T2, T0, SYM_NAME_OFFSET_LEN, "");
        backend.emitLW(T3, T1, SYM_NAME_OFFSET_LEN, "");
        backend.emitBNE(T2, T3, check_not_equal, "");
        backend.emitADDI(T0, T0, SYM_NAME_OFFSET_STR, "");
        backend.emitADDI(T1, T1, SYM_NAME_OFFSET_STR, "");
        backend.emitLocalLabel(Byte_compare, null);
        backend.emitBEQZ(T2, equal, "");
        backend.emitLB(T3, T0, 0, "");
        backend.emitLB(T4, T1, 0, "");
        backend.emitBNE(T3, T4, check_not_equal, "");
        backend.emitADDI(T0, T0, 1, "");
        backend.emitADDI(T1, T1, 1, "");
        backend.emitADDI(T2, T2, -1, "");
        backend.emitJ(Byte_compare, "");
        backend.emitLocalLabel(check_result_true, null);
        backend.emitADDI(A0, ZERO, 1, "");
        backend.emitJ(epilogue, "");
        backend.emitLocalLabel(check_result_False, null);
        backend.emitADDI(A0, ZERO, 0, "");
        backend.emitLocalLabel(epilogue, null);
        backend.emitMV(SP, FP, null);
        backend.emitLW(RA, FP, -4, null);
        backend.emitLW(FP, FP, -8, null);
        backend.emitJR(RA, null);

    }
}