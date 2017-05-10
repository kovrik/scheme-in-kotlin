package core.procedures.predicates;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.procedures.IFn;
import core.procedures.math.Remainder;
import core.scm.*;
import core.utils.Utils;

import java.math.BigDecimal;
import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.CompletableFuture;

public final class Predicate extends AFn {

  public static final Predicate IS_NULL = new Predicate("null?", Objects::isNull);
  public static final Predicate IS_NIL = new Predicate("nil?", Objects::isNull);
  public static final Predicate IS_EOF = new Predicate("eof-object?", Objects::isNull);
  public static final Predicate IS_SOME = new Predicate("some?", Objects::nonNull);
  public static final Predicate IS_EMPTY = new Predicate("empty?", o -> (o == null || (isEmpty(o))));
  public static final Predicate IS_PAIR = new Predicate("pair?", Cons::isPair);
  public static final Predicate IS_LIST = new Predicate("list?", Cons::isList);
  public static final Predicate IS_PROMISE = new Predicate("promise?", o -> (o instanceof CompletableFuture) || (Delay.class.equals(o.getClass())));
  public static final Predicate IS_FUTURE = new Predicate("future?", o -> (Future.class.equals(o.getClass())));
  public static final Predicate IS_FUTURE_DONE = new Predicate("future-done?", o -> assertClass("future-done?", o, java.util.concurrent.Future.class) && ((java.util.concurrent.Future)o).isDone());
  public static final Predicate IS_FUTURE_CANCELLED = new Predicate("future-cancelled?", o -> assertClass("future-cancelled?", o, java.util.concurrent.Future.class) && ((java.util.concurrent.Future)o).isCancelled());
  public static final Predicate IS_DELAY = new Predicate("delay?", o -> (o instanceof Delay));
  public static final Predicate IS_REALIZED = new Predicate("realized?", Predicate::isRealized);
  public static final Predicate IS_CHAR = new Predicate("char?", o -> (o instanceof Character));
  public static final Predicate IS_STRING = new Predicate("string?", o -> (o instanceof CharSequence));
  public static final Predicate IS_VECTOR = new Predicate("vector?", o -> (o instanceof Vector));
  public static final Predicate IS_SET = new Predicate("set?", o -> (o instanceof Set));
  public static final Predicate IS_MAP = new Predicate("map?", o -> o instanceof Map);
  public static final Predicate IS_MAP_ENTRY = new Predicate("map-entry?", o -> o instanceof Map.Entry);
  public static final Predicate IS_COLL = new Predicate("coll?", o -> o instanceof Collection || o instanceof Map || o instanceof Vector);
  public static final Predicate IS_SYMBOL = new Predicate("symbol?", o -> (o instanceof Symbol));
  public static final Predicate IS_BOOLEAN = new Predicate("boolean?", o -> (o instanceof Boolean));
  public static final Predicate IS_TRUE = new Predicate("true?", o -> (o instanceof Boolean) && (Boolean)o);
  public static final Predicate IS_FALSE = new Predicate("false?", o -> (o instanceof Boolean) && !(Boolean)o);
  public static final Predicate IS_PROC = new Predicate("procedure?", Predicate::isProcedure);
  public static final Predicate IS_PORT = new Predicate("port?", o -> (o instanceof IPort));
  public static final Predicate IS_INPUT_PORT = new Predicate("input-port?", o -> (o instanceof InputPort));
  public static final Predicate IS_OUTPUT_PORT = new Predicate("output-port?", o -> (o instanceof OutputPort));
  public static final Predicate IS_NUMBER = new Predicate("number?", o -> (o instanceof Number));
  public static final Predicate IS_INTEGER = new Predicate("integer?", Utils::isInteger);
  public static final Predicate IS_RATIONAL = new Predicate("rational?", Utils::isRational);
  public static final Predicate IS_RATIO = new Predicate("ratio?", o -> o instanceof BigRatio);
  public static final Predicate IS_REAL = new Predicate("real?", Utils::isReal);
  public static final Predicate IS_COMPLEX = new Predicate("complex?", o -> (o instanceof Number));
  public static final Predicate IS_ZERO = new Predicate("zero?", o -> (assertClass("zero?", o, Number.class) && Utils.isZero(o)));
  public static final Predicate IS_POSITIVE = new Predicate("positive?", o -> (assertClass("positive?", o, Type.Real.class) && Utils.isPositive(o)));
  public static final Predicate IS_POS = new Predicate("pos?", o -> (assertClass("pos?", o, Type.Real.class) && Utils.isPositive(o)));
  public static final Predicate IS_NEGATIVE = new Predicate("negative?", o -> (assertClass("negative?", o, Type.Real.class) && Utils.isNegative(o)));
  public static final Predicate IS_NEG = new Predicate("neg?", o -> (assertClass("neg?", o, Type.Real.class) && Utils.isNegative(o)));
  public static final Predicate IS_EXACT = new Predicate("exact?", o -> (assertClass("exact?", o, Number.class) && Utils.isExact(o)));
  public static final Predicate IS_INEXACT = new Predicate("inexact?", o -> (assertClass("inexact?", o, Number.class) && Utils.isInexact(o)));
  public static final Predicate IS_IMMUTABLE = new Predicate("immutable?", Predicate::isImmutable);
  public static final Predicate IS_MUTABLE = new Predicate("mutable?", Predicate::isMutable);
  public static final Predicate IS_EVEN = new Predicate("even?", o -> (assertClass("even?", o, Integer.class) && (Utils.isZero(Remainder.apply((Number)o, 2L)))));
  public static final Predicate IS_ODD = new Predicate("odd?", o -> (assertClass("odd?", o, Integer.class) && !(Utils.isZero(Remainder.apply((Number)o, 2L)))));
  public static final Predicate IS_KEYWORD = new Predicate("keyword?", o -> (o instanceof Keyword));
  public static final Predicate IS_ANY = new Predicate("any?", o -> true);
  public static final Predicate IS_BLANK = new Predicate("blank?", o -> assertClass("blank?", o, String.class) && o == null || o.toString().isEmpty() || o.toString().trim().isEmpty());
  public static final Predicate IS_CLASS = new Predicate("class?", o -> o instanceof Class);
  public static final Predicate IS_DECIMAL = new Predicate("decimal?", o -> o instanceof BigDecimal);
  public static final Predicate IS_FLOAT = new Predicate("float?", o -> o instanceof Float || o instanceof Double);
  public static final Predicate IS_FN = new Predicate("fn?", Predicate::isProcedure);

  private final String name;
  private final java.util.function.Predicate predicate;

  private Predicate(String name, java.util.function.Predicate predicate) {
    super(new FnArgsBuilder().min(1).max(1).build());
    this.name = name;
    this.predicate = predicate;
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public Boolean apply1(Object arg) {
    return predicate.test(arg);
  }

  @Override
  public Boolean apply(Object... args) {
    return predicate.test(args[0]);
  }

  private static boolean isMutable(Object o) {
    return !isImmutable(o);
  }

  private static boolean isImmutable(Object o) {
    return !((o instanceof MutableString) || (o instanceof MutableVector));
  }

  private static boolean assertClass(String name, Object o, Class<?> c) {
    if (Type.checkType(o, c)) {
      return true;
    }
    throw new WrongTypeException(name, c, o);
  }

  private static boolean isEmpty(Object o) {
    if (o instanceof Collection) {
      return ((Collection)o).isEmpty();
    } else if (o instanceof Vector) {
      return ((Vector)o).size() == 0;
    } else if (o instanceof CharSequence) {
      return ((CharSequence)o).length() == 0;
    } else if (o instanceof Map) {
      return ((Map)o).size() == 0;
    }
    return false;
  }

  private static boolean isRealized(Object o) {
    if (o instanceof java.util.concurrent.Future) {
      return ((java.util.concurrent.Future) o).isDone();
    }
    throw new WrongTypeException("realized?", "Delay or Promise or Future", o);
  }

  private static boolean isProcedure(Object o) {
    return (o instanceof IFn) && !(o instanceof Symbol) && !(o instanceof Keyword) && !(o instanceof Map) &&
          !(o instanceof Vector) && !(o instanceof Map.Entry);
  }
}
