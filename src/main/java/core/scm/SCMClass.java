package core.scm;

import core.exceptions.WrongTypeException;
import core.procedures.IFn;

public enum SCMClass implements ISCMClass {
  INTEGER("Integer"),
  REAL("Real"),
  RATIONAL("Rational"),
  STRING("String"),
  MUTABLE_STRING("MutableString"),
  IMMUTABLE_STRING("ImmutableString"),
  CHARACTER("Character"),
  BOOLEAN("Boolean"),
  ENVIRONMENT("Environment"),
  SPECIALFORM("SpecialForm"),
  NIL("Nil"),
  LIST("List"),
  PAIR("Pair"),
  SYMBOL("Symbol"),
  VECTOR("Vector"),
  MUTABLE_VECTOR("MutableVector"),
  IMMUTABLE_VECTOR("ImmutableVector"),
  PROMISE("Promise"),
  PROCEDURE("Procedure"),
  ERROR("Error"),
  CLASS("Class"),
  PORT("Port"),
  INPUT_PORT("InputPort"),
  OUTPUT_PORT("OutputPort"),
  EOF("EOF"),
  UNSPECIFIED("Unspecified");

  private final String name;

  SCMClass(String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }

  @Override
  public SCMClass getSCMClass() {
    return CLASS;
  }

  @Override
  public String toString() {
    return "<class " + getName() + ">";
  }

  public static SCMClass valueOf(Class clazz) {
    if (clazz.equals(SCMMutableString.class)) {
      return MUTABLE_STRING;
    } else if (clazz.equals(String.class)) {
      // Java's Strings are immutable
      return IMMUTABLE_STRING;
    } else if (clazz.equals(SCMImmutableString.class)) {
      return IMMUTABLE_STRING;
    } else if (clazz.equals(SCMBoolean.class)) {
      return BOOLEAN;
    } else if (clazz.equals(SCMSymbol.class)) {
      return SYMBOL;
    } else if (clazz.equals(SCMCons.SCMPair.class)) {
      return PAIR;
    } else if (clazz.equals(SCMCons.SCMProperList.class)) {
      return LIST;
    } else if (clazz.equals(SCMVector.class)) {
      return VECTOR;
    } else if (clazz.equals(SCMMutableVector.class)) {
      return MUTABLE_VECTOR;
    } else if (clazz.equals(SCMImmutableVector.class)) {
      return IMMUTABLE_VECTOR;
    } else if (IFn.class.isAssignableFrom(clazz)) {
      return PROCEDURE;
    } else if (clazz.equals(SCMBigRational.class)) {
      return RATIONAL;
    } else if (clazz.equals(SCMMutableVector.class)) {
      return MUTABLE_VECTOR;
    } else if (clazz.equals(SCMPromise.class)) {
      return PROMISE;
    } else if (clazz.equals(ISCMPort.class)) {
      return PORT;
    } else if (clazz.equals(SCMOutputPort.class)) {
      return OUTPUT_PORT;
    } else if (clazz.equals(SCMInputPort.class)) {
      return INPUT_PORT;
    }
    throw new IllegalArgumentException("Unknown SCMClass type: " + clazz);
  }

  public static boolean assertClass(Object o, Class<?> c) {
    if (c.isAssignableFrom(o.getClass())) {
      return true;
    }
    throw new WrongTypeException(c.getSimpleName(), o);
  }

  public static boolean checkClass(Object object, Class<?> expected) {
    /* FIXME Workaround for SCM Lists and Pairs: check and replace with marker class at Runtime */
    Class<?> actual = object.getClass();
    if ((expected.equals(SCMCons.SCMProperList.class)) && (SCMCons.isList(object))) {
      actual = SCMCons.SCMProperList.class;
    } else if ((expected.equals(SCMCons.SCMPair.class)) && (SCMCons.isPair(object))) {
      actual = SCMCons.SCMPair.class;
    }
    if (expected == actual) {
      return true;
    }
    if (expected.isAssignableFrom(actual)) {
      return true;
    }
    if (ISCMClass.class.isAssignableFrom(actual)) {
      if (String.class.equals(expected) && (SCMImmutableString.class.equals(actual) || SCMMutableString.class.equals(actual)) ) {
        return true;
      } else if (SCMImmutableString.class.equals(expected) && (String.class.equals(actual) || SCMImmutableString.class.equals(actual)) ) {
        return true;
      } else if (SCMMutableString.class.equals(expected) && (StringBuilder.class.equals(actual) || SCMMutableString.class.equals(actual)) ) {
        return true;
      } else if (Boolean.class.equals(expected) && SCMBoolean.class.equals(actual)) {
        return true;
      }
    }
    return false;
  }
}
