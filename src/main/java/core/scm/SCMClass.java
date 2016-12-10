package core.scm;

import core.exceptions.WrongTypeException;
import core.procedures.IFn;

import java.util.HashMap;
import java.util.Map;

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

  private static final Map<Class, SCMClass> JAVA_TO_SCM_CLASSES = new HashMap<>();
  static {
    JAVA_TO_SCM_CLASSES.put(Integer.class,    SCMClass.INTEGER);
    JAVA_TO_SCM_CLASSES.put(Long.class,       SCMClass.INTEGER);
    JAVA_TO_SCM_CLASSES.put(Double.class,     SCMClass.REAL);
    JAVA_TO_SCM_CLASSES.put(Float.class,      SCMClass.REAL);
    JAVA_TO_SCM_CLASSES.put(Character.class,  SCMClass.CHARACTER);
    JAVA_TO_SCM_CLASSES.put(String.class, IMMUTABLE_STRING);
    JAVA_TO_SCM_CLASSES.put(SCMMutableString.class, MUTABLE_STRING);
    JAVA_TO_SCM_CLASSES.put(SCMImmutableString.class, IMMUTABLE_STRING);
    JAVA_TO_SCM_CLASSES.put(Boolean.class,    SCMClass.BOOLEAN);
    JAVA_TO_SCM_CLASSES.put(IFn.class, PROCEDURE);
    JAVA_TO_SCM_CLASSES.put(SCMBoolean.class, BOOLEAN);
    JAVA_TO_SCM_CLASSES.put(SCMSymbol.class, SYMBOL);
    JAVA_TO_SCM_CLASSES.put(SCMCons.SCMPair.class, PAIR);
    JAVA_TO_SCM_CLASSES.put(SCMCons.SCMProperList.class, LIST);
    JAVA_TO_SCM_CLASSES.put(SCMVector.class, VECTOR);
    JAVA_TO_SCM_CLASSES.put(SCMMutableVector.class, MUTABLE_VECTOR);
    JAVA_TO_SCM_CLASSES.put(SCMImmutableVector.class, IMMUTABLE_VECTOR);
    JAVA_TO_SCM_CLASSES.put(SCMBigRational.class, RATIONAL);
    JAVA_TO_SCM_CLASSES.put(SCMMutableVector.class, MUTABLE_VECTOR);
    JAVA_TO_SCM_CLASSES.put(SCMPromise.class, PROMISE);
    JAVA_TO_SCM_CLASSES.put(ISCMPort.class, PORT);
    JAVA_TO_SCM_CLASSES.put(SCMOutputPort.class, OUTPUT_PORT);
    JAVA_TO_SCM_CLASSES.put(SCMInputPort.class, INPUT_PORT);
  }

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
    SCMClass scmClass = JAVA_TO_SCM_CLASSES.get(clazz);
    if (scmClass == null) {
      throw new IllegalArgumentException("Unknown SCMClass type: " + clazz);
    }
    return scmClass;
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
