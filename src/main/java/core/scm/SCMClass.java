package core.scm;

import core.exceptions.WrongTypeException;
import core.procedures.IFn;
import core.utils.NumberUtils;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;

public enum SCMClass implements ISCMClass {
  INTEGER("Integer"),
  REAL("Real"),
  RATIONAL("Rational"),
  COMPLEX("Complex"),
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
    SCM_CLASSES.put(BigDecimal.class,            REAL);
    SCM_CLASSES.put(SCMBigRational.class,        RATIONAL);
    SCM_CLASSES.put(SCMBigComplex.class,         COMPLEX);

    SCM_CLASSES.put(Character.class,             CHARACTER);
    SCM_CLASSES.put(String.class,                IMMUTABLE_STRING);
    SCM_CLASSES.put(SCMImmutableString.class,    IMMUTABLE_STRING);
    SCM_CLASSES.put(StringBuilder.class,         MUTABLE_STRING);
    SCM_CLASSES.put(SCMMutableString.class,      MUTABLE_STRING);
    SCM_CLASSES.put(Boolean.class,               BOOLEAN);
    SCM_CLASSES.put(SCMBoolean.class,            BOOLEAN);
    SCM_CLASSES.put(IFn.class,                   PROCEDURE);
    SCM_CLASSES.put(SCMSymbol.class,             SYMBOL);
    SCM_CLASSES.put(SCMPair.class,               PAIR);
    SCM_CLASSES.put(SCMProperList.class,         LIST);
    SCM_CLASSES.put(SCMVector.class,             VECTOR);
    SCM_CLASSES.put(SCMImmutableVector.class,    IMMUTABLE_VECTOR);
    SCM_CLASSES.put(SCMMutableVector.class,      MUTABLE_VECTOR);
    SCM_CLASSES.put(SCMPromise.class,            PROMISE);
    SCM_CLASSES.put(ISCMPort.class,              PORT);
    SCM_CLASSES.put(SCMOutputPort.class,         OUTPUT_PORT);
    SCM_CLASSES.put(SCMInputPort.class,          INPUT_PORT);
    SCM_CLASSES.put(SCMEof.class,                EOF);
    SCM_CLASSES.put(SCMError.class,              ERROR);
    SCM_CLASSES.put(SCMUnspecified.class,        UNSPECIFIED);
  }

  /* Marker classes for FnArgs annotation
   *
   * FnArgs can't cover all numerical types because of the following limitation:
   *
   * The return type of a method declared in an annotation type must be one of the following,
   * or a compile-time error occurs:
   * - Primitive type
   * - String
   * - Class or an invocation of Class (§4.5)
   * - Enum type
   * - Annotation type
   * - Array type whose component type is one of the preceding types
   *
   * (see https://docs.oracle.com/javase/specs/jls/se8/html/jls-9.html#jls-9.6.1)
   *
   * Below is the mapping from predicates to the actual/marker classes:
   *
   *   pair?                      -> SCMPair + SCMCons.isPair()
   *   list?                      -> SCMProperList + SCMCons.isList()
   *   number?                    -> Number.class
   *   complex?                   -> SCMBigComplex.class
   *   real?                      -> Real.class
   *   rational?                  -> SCMBigRational + NumberUtils.IsRational()
   *   integer?                   -> Integer.class/Long.class
   *   exact-integer?             -> ExactInteger.class *
   *   exact-nonnegative-integer? -> ExactNonNegativeInteger.class *
   *   exact-positive-integer?    -> ExactPositiveInteger.class *
   *   inexact-real?              -> InexactReal.class *
   *   positive?                  -> Positive.class *
   *   negative?                  -> Negative.class *
   *   nonnegative?               -> NonNegative.class *
   *   exact?                     -> Exact.class *
   *   inexact?                   -> Inexact.class *
   */
  /* Marker classes for Proper and Improper lists */
  public abstract class SCMProperList implements ISCMClass {
    @Override public SCMClass getSCMClass() { return SCMClass.LIST; }
  }
  public abstract class SCMPair implements ISCMClass {
    @Override public SCMClass getSCMClass() { return SCMClass.PAIR; }
  }
  /* Marker classes for numbers */
  public abstract class ExactNonNegativeInteger {} // the only one that is actually used?
  public abstract class ExactInteger {}
  public abstract class ExactPositiveInteger {}
  public abstract class InexactReal {}
  public abstract class Positive {}
  public abstract class Negative {}
  public abstract class NonNegative {}
  public abstract class Exact {}
  public abstract class Inexact {}
  public abstract class Real {}

  private static final Map<Class, Predicate<Object>> TYPE_PREDICATES = new HashMap<>();
  static {
    TYPE_PREDICATES.put(Boolean.class, o -> SCMBoolean.class.equals(o.getClass()));
    TYPE_PREDICATES.put(String.class, o -> SCMImmutableString.class.equals(o.getClass()) || SCMMutableString.class.equals(o.getClass()));
    TYPE_PREDICATES.put(SCMImmutableString.class, o -> String.class.equals(o.getClass()) || SCMImmutableString.class.equals(o.getClass()));
    TYPE_PREDICATES.put(SCMMutableString.class, o -> StringBuilder.class.equals(o.getClass()) || SCMMutableString.class.equals(o.getClass()));
    TYPE_PREDICATES.put(SCMProperList.class, SCMCons::isList);
    TYPE_PREDICATES.put(SCMPair.class, SCMCons::isPair);
    TYPE_PREDICATES.put(SCMBigRational.class, NumberUtils::isRational);
    TYPE_PREDICATES.put(Long.class, NumberUtils::isInteger);
    TYPE_PREDICATES.put(Integer.class, NumberUtils::isInteger);
    TYPE_PREDICATES.put(Exact.class, NumberUtils::isExact);
    TYPE_PREDICATES.put(ExactInteger.class, NumberUtils::isExactInteger);
    TYPE_PREDICATES.put(ExactPositiveInteger.class, NumberUtils::isExactPositiveInteger);
    TYPE_PREDICATES.put(ExactNonNegativeInteger.class, NumberUtils::isExactNonNegativeInteger);
    TYPE_PREDICATES.put(Inexact.class, NumberUtils::isInexact);
    TYPE_PREDICATES.put(InexactReal.class, NumberUtils::isInexact);
    TYPE_PREDICATES.put(Positive.class, NumberUtils::isPositive);
    TYPE_PREDICATES.put(Negative.class, NumberUtils::isNegative);
    TYPE_PREDICATES.put(NonNegative.class, NumberUtils::isNonNegative);
    TYPE_PREDICATES.put(Real.class, NumberUtils::isReal);
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
    return "#<class:" + getName() + ">";
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
    if (object instanceof Number) {
      return classOfNumber((Number)object);
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

  public static SCMClass classOfNumber(Number number) {
    /* Must be a Java object */
    if (number instanceof BigDecimal) {
      /* Check if it is integral */
      BigDecimal bd = (BigDecimal)number;
      if (bd.signum() == 0 || bd.scale() <= 0) {
        return SCMClass.INTEGER;
      }
      return SCMClass.REAL;
    }
    /* Not a special case, just map Java class to SCMClass */
    return valueOf(number.getClass());
  }

  public static boolean assertClass(Object o, Class<?> c) {
    if (c.isAssignableFrom(o.getClass())) {
      return true;
    }
    throw new WrongTypeException(c.getSimpleName(), o);
  }

  public static boolean checkType(Object o, Class<?> expected) {
    Class<?> actual = o.getClass();
    if (expected == actual) {
      return true;
    } else if (expected.isAssignableFrom(actual)) {
      return true;
    } else {
      if (Long.class.equals(expected)) {
        return NumberUtils.isInteger(o);
      }
      Predicate<Object> check = TYPE_PREDICATES.get(expected);
      return check != null && check.test(o);
    }
  }
}
