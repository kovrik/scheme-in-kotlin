package core.scm;

import core.procedures.AFn;
import core.procedures.IFn;
import core.utils.Utils;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Predicate;

public final class Type {

  /* Override type names for some classes */
  private static final Map<Class, String> TYPE_NAME_MAPPINGS = new HashMap<>();
  static {
    TYPE_NAME_MAPPINGS.put(Long.class, "Integer");
    TYPE_NAME_MAPPINGS.put(BigRatio.class, "Rational");
    TYPE_NAME_MAPPINGS.put(BigComplex.class, "Complex");
    TYPE_NAME_MAPPINGS.put(CharSequence.class, "String");
    TYPE_NAME_MAPPINGS.put(StringBuilder.class, "MutableString");
    TYPE_NAME_MAPPINGS.put(MutableString.class, "MutableString");
    TYPE_NAME_MAPPINGS.put(IFn.class, "Procedure");
    TYPE_NAME_MAPPINGS.put(AFn.class, "Procedure");
    TYPE_NAME_MAPPINGS.put(ProperList.class, "List");
    TYPE_NAME_MAPPINGS.put(IPort.class, "Port");
    TYPE_NAME_MAPPINGS.put(Map.Entry.class, "MapEntry");
  }

  public static String nameOf(Class clazz) {
    return TYPE_NAME_MAPPINGS.getOrDefault(clazz, clazz.getSimpleName());
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
   * (see https://docs.oracle.com/javase/specs/jls/se8/html/jls-9.html#jls-9.6.1)
   */
  // TODO Get rid of them
  /* Marker classes for Proper and Improper lists */
  public static final class ProperList { private ProperList(){} }
  public static final class Pair { private Pair(){} }
  /* Marker classes for numbers */
  public static final class ExactNonNegativeInteger { private ExactNonNegativeInteger(){} }
  public static final class ExactPositiveInteger { private ExactPositiveInteger(){} }
  public static final class Real { private Real(){} }
  public static final class BitOp { private BitOp(){} }

  private static final Map<Class, Predicate<Object>> TYPE_PREDICATES = new HashMap<>();
  static {
    TYPE_PREDICATES.put(String.class, o -> o instanceof CharSequence);
    TYPE_PREDICATES.put(MutableString.class, o -> StringBuilder.class.equals(o.getClass()) || MutableString.class.equals(o.getClass()));
    TYPE_PREDICATES.put(ProperList.class, Cons::isList);
    TYPE_PREDICATES.put(Pair.class, Cons::isPair);
    TYPE_PREDICATES.put(BigRatio.class, Utils::isRational);
    TYPE_PREDICATES.put(Long.class, Utils::isInteger);
    TYPE_PREDICATES.put(Integer.class, Utils::isInteger);
    TYPE_PREDICATES.put(ExactPositiveInteger.class, Utils::isExactPositiveInteger);
    TYPE_PREDICATES.put(ExactNonNegativeInteger.class, Utils::isExactNonNegativeInteger);
    TYPE_PREDICATES.put(Real.class, Utils::isReal);
    TYPE_PREDICATES.put(BitOp.class, Utils::isBitOpSupported);
  }

  public static boolean checkType(Object o, Class<?> expected) {
    /* Nil is possible value for any data type */
    if (o == null) {
      return true;
    }
    Class<?> actual = o.getClass();
    if (expected == actual || expected.isAssignableFrom(actual)) {
      return true;
    }
    Predicate<Object> check = TYPE_PREDICATES.get(expected);
    return check != null && check.test(o);
  }
}
