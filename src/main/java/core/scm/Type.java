package core.scm;

import core.procedures.AFn;
import core.procedures.IFn;
import core.utils.Utils;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Predicate;

public enum Type {
  INTEGER("Integer"),
  REAL("Real"),
  RATIONAL("Rational"),
  COMPLEX("Complex"),
  STRING("String"),
  MUTABLE_STRING("MutableString"),
  CHARACTER("Character"),
  BOOLEAN("Boolean"),
  VOID("Void"),
  LIST("List"),
  PAIR("Pair"),
  SYMBOL("Symbol"),
  VECTOR("Vector"),
  MUTABLE_VECTOR("MutableVector"),
  IMMUTABLE_VECTOR("ImmutableVector"),
  DELAY("Delay"),
  PROMISE("Promise"),
  FUTURE("Future"),
  PROCEDURE("Procedure"),
  ERROR("Error"),
  PORT("Port"),
  INPUT_PORT("InputPort"),
  OUTPUT_PORT("OutputPort"),
  KEYWORD("Keyword"),
  MAP_ENTRY("MapEntry"),
  ;

  private static final Map<Class, Type> TYPE_MAPPINGS = new HashMap<>();
  static {
    TYPE_MAPPINGS.put(Integer.class,               INTEGER);
    TYPE_MAPPINGS.put(Long.class,                  INTEGER);
    TYPE_MAPPINGS.put(BigInteger.class,            INTEGER);
    TYPE_MAPPINGS.put(Double.class,                REAL);
    TYPE_MAPPINGS.put(Float.class,                 REAL);
    TYPE_MAPPINGS.put(BigDecimal.class,            REAL);
    TYPE_MAPPINGS.put(BigRatio.class,              RATIONAL);
    TYPE_MAPPINGS.put(BigComplex.class,            COMPLEX);
    TYPE_MAPPINGS.put(Character.class,             CHARACTER);
    TYPE_MAPPINGS.put(String.class,                STRING);
    TYPE_MAPPINGS.put(CharSequence.class,          STRING);
    TYPE_MAPPINGS.put(StringBuilder.class,         MUTABLE_STRING);
    TYPE_MAPPINGS.put(MutableString.class,         MUTABLE_STRING);
    TYPE_MAPPINGS.put(Boolean.class,               BOOLEAN);
    TYPE_MAPPINGS.put(IFn.class,                   PROCEDURE);
    TYPE_MAPPINGS.put(AFn.class,                   PROCEDURE);
    TYPE_MAPPINGS.put(Symbol.class,                SYMBOL);
    TYPE_MAPPINGS.put(Pair.class,                  PAIR);
    TYPE_MAPPINGS.put(ProperList.class,            LIST);
    TYPE_MAPPINGS.put(Vector.class,                VECTOR);
    TYPE_MAPPINGS.put(ImmutableVector.class,       IMMUTABLE_VECTOR);
    TYPE_MAPPINGS.put(MutableVector.class,         MUTABLE_VECTOR);
    TYPE_MAPPINGS.put(Delay.class,                 DELAY);
    TYPE_MAPPINGS.put(Promise.class,               PROMISE);
    TYPE_MAPPINGS.put(Future.class,                FUTURE);
    TYPE_MAPPINGS.put(IPort.class,                 PORT);
    TYPE_MAPPINGS.put(OutputPort.class,            OUTPUT_PORT);
    TYPE_MAPPINGS.put(InputPort.class,             INPUT_PORT);
    TYPE_MAPPINGS.put(Error.class,                 ERROR);
    TYPE_MAPPINGS.put(Keyword.class,               KEYWORD);
    TYPE_MAPPINGS.put(Void.class,                  VOID);
    TYPE_MAPPINGS.put(IMapEntry.class,             MAP_ENTRY);
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
   *   pair?                      -> Pair + Cons.isPair()
   *   list?                      -> ProperList + Cons.isList()
   *   number?                    -> Number.class
   *   complex?                   -> BigComplex.class
   *   real?                      -> Real.class
   *   rational?                  -> BigRatio + Utils.IsRational()
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
  public abstract class ProperList {}
  public abstract class Pair {}
  /* Marker classes for numbers */
  public static final class ExactNonNegativeInteger {}
  public static final class ExactInteger {}
  public static final class ExactPositiveInteger {}
  public static final class InexactReal {}
  public static final class Positive {}
  public static final class Negative {}
  public static final class NonNegative {}
  public static final class Exact {}
  public static final class Inexact {}
  public static final class Real {}
  public static final class BitOp {}

  private static final Map<Class, Predicate<Object>> TYPE_PREDICATES = new HashMap<>();
  static {
    TYPE_PREDICATES.put(CharSequence.class, o -> o instanceof CharSequence);
    TYPE_PREDICATES.put(String.class, o -> o instanceof CharSequence);
    TYPE_PREDICATES.put(MutableString.class, o -> StringBuilder.class.equals(o.getClass()) || MutableString.class.equals(o.getClass()));
    TYPE_PREDICATES.put(ProperList.class, Cons::isList);
    TYPE_PREDICATES.put(Pair.class, Cons::isPair);
    TYPE_PREDICATES.put(BigRatio.class, Utils::isRational);
    TYPE_PREDICATES.put(Long.class, Utils::isInteger);
    TYPE_PREDICATES.put(Integer.class, Utils::isInteger);
    TYPE_PREDICATES.put(Exact.class, Utils::isExact);
    TYPE_PREDICATES.put(ExactInteger.class, Utils::isExactInteger);
    TYPE_PREDICATES.put(ExactPositiveInteger.class, Utils::isExactPositiveInteger);
    TYPE_PREDICATES.put(ExactNonNegativeInteger.class, Utils::isExactNonNegativeInteger);
    TYPE_PREDICATES.put(Inexact.class, Utils::isInexact);
    TYPE_PREDICATES.put(InexactReal.class, Utils::isInexact);
    TYPE_PREDICATES.put(Positive.class, Utils::isPositive);
    TYPE_PREDICATES.put(Negative.class, Utils::isNegative);
    TYPE_PREDICATES.put(NonNegative.class, Utils::isNonNegative);
    TYPE_PREDICATES.put(Real.class, Utils::isReal);
    TYPE_PREDICATES.put(BitOp.class, Utils::isBitOpSupported);
  }

  private final String name;

  Type(String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }

  @Override
  public String toString() {
    return "#<class:" + getName() + ">";
  }

  public static Type valueOf(Class clazz) {
    return TYPE_MAPPINGS.get(clazz);
  }

  public static boolean checkType(Object o, Class<?> expected) {
    /* Nil is possible value of any data type */
    if (o == null) {
      return true;
    }
    Class<?> actual = o.getClass();
    if (expected == actual || expected.isAssignableFrom(actual)) {
      return true;
    } else {
      if (Long.class.equals(expected)) {
        return Utils.isInteger(o);
      }
      Predicate<Object> check = TYPE_PREDICATES.get(expected);
      return check != null && check.test(o);
    }
  }
}
