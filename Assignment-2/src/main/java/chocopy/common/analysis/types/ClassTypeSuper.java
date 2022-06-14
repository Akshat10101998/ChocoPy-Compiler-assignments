package chocopy.common.analysis.types;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonCreator;

/** Semantic information for a function or method. */
public class ClassTypeSuper extends Type {

    private final String superClass;
    private final String className;
    //can not be extended
    private final Boolean isSpecial;

    /** Create a FuncType returning RETURNTYPE0, intiallly parapeterless. */
    public ClassTypeSuper(String className) {
        this.superClass = "object";
        this.className = className;
        this.isSpecial = true;
    }

    /** Create a FuncType for NAME0 with formal parameter types
    referred https://github.com/taoliq/cs164-sp19/tree/90b9d4a79c09b8427c30ecfcf9b14b5ba2d0fe1b
     *  PARAMETERS0, returning type RETURNTYPE0. */
    @JsonCreator
    public ClassTypeSuper(String superClass, String className) {
        this.superClass = superClass;
        this.className = className;
        this.isSpecial = false;
    }

    public boolean isSpecialClass() {
        return isSpecial;
    }

    @Override
    public String className() {
        return className;
    }

    public String getSuperClassName() {
        return superClass;
    }

    @Override
    public String toString() {
        return "<type>";
    }

}