package core.scm;

import core.exceptions.WrongTypeException;
import core.procedures.IFn;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.HashMap;
import java.util.List;
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
  SPECIAL_FORM("SpecialForm"),
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

  private static final Map<Class, SCMClass> SCM_CLASSES = new HashMap<>();
  static {
    SCM_CLASSES.put(Integer.class,               INTEGER);
    SCM_CLASSES.put(Long.class,                  INTEGER);
    SCM_CLASSES.put(BigInteger.class,            INTEGER);
    SCM_CLASSES.put(Double.class,                REAL);
    SCM_CLASSES.put(Float.class,                 REAL);
    SCM_CLASSES.put(Character.class,             CHARACTER);
    SCM_CLASSES.put(String.class,                IMMUTABLE_STRING);
    SCM_CLASSES.put(SCMImmutableString.class,    IMMUTABLE_STRING);
    SCM_CLASSES.put(StringBuilder.class,         MUTABLE_STRING);
    SCM_CLASSES.put(SCMMutableString.class,      MUTABLE_STRING);
    SCM_CLASSES.put(Boolean.class,               BOOLEAN);
    SCM_CLASSES.put(SCMBoolean.class,            BOOLEAN);
    SCM_CLASSES.put(IFn.class,                   PROCEDURE);
    SCM_CLASSES.put(SCMSymbol.class,             SYMBOL);
    SCM_CLASSES.put(SCMCons.SCMPair.class,       PAIR);
    SCM_CLASSES.put(SCMCons.SCMProperList.class, LIST);
    SCM_CLASSES.put(SCMVector.class,             VECTOR);
    SCM_CLASSES.put(SCMImmutableVector.class,    IMMUTABLE_VECTOR);
    SCM_CLASSES.put(SCMMutableVector.class,      MUTABLE_VECTOR);
    SCM_CLASSES.put(SCMBigRational.class,        RATIONAL);
    SCM_CLASSES.put(SCMPromise.class,            PROMISE);
    SCM_CLASSES.put(ISCMPort.class,              PORT);
    SCM_CLASSES.put(SCMOutputPort.class,         OUTPUT_PORT);
    SCM_CLASSES.put(SCMInputPort.class,          INPUT_PORT);
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
    return SCM_CLASSES.get(clazz);
  }

  public static SCMClass classOf(Object object) {
    /* Special Checks go first */
    if (object == null) {
      return SCMClass.NIL;
    }
    /* All custom SCM Classes should implement ISCMClass interface */
    if (object instanceof ISCMClass) {
      return ((ISCMClass)object).getSCMClass();
    }
    /* Must be a Java object */
    if (object instanceof BigDecimal) {
      /* Check if it is integral */
      if (((BigDecimal)object).remainder(BigDecimal.ONE).equals(BigDecimal.ZERO)) {
        return SCMClass.INTEGER;
      }
      return SCMClass.REAL;
    }
    /* Check Pair and Nil */
    if (object instanceof List) {
      if (((List) object).isEmpty()) {
        return SCMClass.NIL;
      }
      return SCMClass.PAIR;
    }
    /* Not a special case, just map Java class to SCMClass */
    return valueOf(object.getClass());
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
