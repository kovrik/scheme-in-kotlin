package core.procedures.predicates;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.procedures.IFn;
import core.procedures.math.Remainder;
import core.scm.*;
import core.utils.NumberUtils;

import java.math.BigDecimal;
import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Future;
import java.util.function.Predicate;

public final class SCMPredicate extends AFn {

  public static final SCMPredicate IS_NULL = new SCMPredicate("null?", Objects::isNull);
  public static final SCMPredicate IS_NIL = new SCMPredicate("nil?", Objects::isNull);
  public static final SCMPredicate IS_EOF = new SCMPredicate("eof-object?", Objects::isNull);
  public static final SCMPredicate IS_SOME = new SCMPredicate("some?", Objects::nonNull);
  public static final SCMPredicate IS_EMPTY = new SCMPredicate("empty?", o -> (o == null || (isEmpty(o))));
  public static final SCMPredicate IS_PAIR = new SCMPredicate("pair?", SCMCons::isPair);
  public static final SCMPredicate IS_LIST = new SCMPredicate("list?", SCMCons::isList);
  public static final SCMPredicate IS_PROMISE = new SCMPredicate("promise?", o -> (o instanceof CompletableFuture) || (SCMDelay.class.equals(o.getClass())));
  public static final SCMPredicate IS_FUTURE = new SCMPredicate("future?", o -> (SCMFuture.class.equals(o.getClass())));
  public static final SCMPredicate IS_FUTURE_DONE = new SCMPredicate("future-done?", o -> assertClass("future-done?", o, Future.class) && ((Future)o).isDone());
  public static final SCMPredicate IS_FUTURE_CANCELLED = new SCMPredicate("future-cancelled?", o -> assertClass("future-cancelled?", o, Future.class) && ((Future)o).isCancelled());
  public static final SCMPredicate IS_DELAY = new SCMPredicate("delay?", o -> (o instanceof SCMDelay));
  public static final SCMPredicate IS_REALIZED = new SCMPredicate("realized?", SCMPredicate::isRealized);
  public static final SCMPredicate IS_CHAR = new SCMPredicate("char?", o -> (o instanceof Character));
  public static final SCMPredicate IS_STRING = new SCMPredicate("string?", o -> (o instanceof CharSequence));
  public static final SCMPredicate IS_VECTOR = new SCMPredicate("vector?", o -> (o instanceof SCMVector));
  public static final SCMPredicate IS_SET = new SCMPredicate("set?", o -> (o instanceof Set));
  public static final SCMPredicate IS_MAP = new SCMPredicate("map?", o -> o instanceof Map);
  public static final SCMPredicate IS_MAP_ENTRY = new SCMPredicate("map-entry?", o -> o instanceof Map.Entry);
  public static final SCMPredicate IS_COLL = new SCMPredicate("coll?", o -> o instanceof Collection || o instanceof Map || o instanceof SCMVector);
  public static final SCMPredicate IS_SYMBOL = new SCMPredicate("symbol?", o -> (o instanceof SCMSymbol));
  public static final SCMPredicate IS_BOOLEAN = new SCMPredicate("boolean?", o -> (o instanceof Boolean));
  public static final SCMPredicate IS_TRUE = new SCMPredicate("true?", o -> (o instanceof Boolean) && (Boolean)o);
  public static final SCMPredicate IS_FALSE = new SCMPredicate("false?", o -> (o instanceof Boolean) && !(Boolean)o);
  public static final SCMPredicate IS_PROC = new SCMPredicate("procedure?", SCMPredicate::isProcedure);
  public static final SCMPredicate IS_PORT = new SCMPredicate("port?", o -> (o instanceof ISCMPort));
  public static final SCMPredicate IS_INPUT_PORT = new SCMPredicate("input-port?", o -> (o instanceof SCMInputPort));
  public static final SCMPredicate IS_OUTPUT_PORT = new SCMPredicate("output-port?", o -> (o instanceof SCMOutputPort));
  public static final SCMPredicate IS_NUMBER = new SCMPredicate("number?", o -> (o instanceof Number));
  public static final SCMPredicate IS_INTEGER = new SCMPredicate("integer?", NumberUtils::isInteger);
  public static final SCMPredicate IS_RATIONAL = new SCMPredicate("rational?", NumberUtils::isRational);
  public static final SCMPredicate IS_RATIO = new SCMPredicate("ratio?", o -> o instanceof SCMBigRational);
  public static final SCMPredicate IS_REAL = new SCMPredicate("real?", NumberUtils::isReal);
  public static final SCMPredicate IS_COMPLEX = new SCMPredicate("complex?", o -> (o instanceof Number));
  public static final SCMPredicate IS_ZERO = new SCMPredicate("zero?", o -> (assertClass("zero?", o, Number.class) && NumberUtils.isZero(o)));
  public static final SCMPredicate IS_POSITIVE = new SCMPredicate("positive?", o -> (assertClass("positive?", o, SCMClass.Real.class) && NumberUtils.isPositive(o)));
  public static final SCMPredicate IS_POS = new SCMPredicate("pos?", o -> (assertClass("pos?", o, SCMClass.Real.class) && NumberUtils.isPositive(o)));
  public static final SCMPredicate IS_NEGATIVE = new SCMPredicate("negative?", o -> (assertClass("negative?", o, SCMClass.Real.class) && NumberUtils.isNegative(o)));
  public static final SCMPredicate IS_NEG = new SCMPredicate("neg?", o -> (assertClass("neg?", o, SCMClass.Real.class) && NumberUtils.isNegative(o)));
  public static final SCMPredicate IS_EXACT = new SCMPredicate("exact?", o -> (assertClass("exact?", o, Number.class) && NumberUtils.isExact(o)));
  public static final SCMPredicate IS_INEXACT = new SCMPredicate("inexact?", o -> (assertClass("inexact?", o, Number.class) && NumberUtils.isInexact(o)));
  public static final SCMPredicate IS_IMMUTABLE = new SCMPredicate("immutable?", SCMPredicate::isImmutable);
  public static final SCMPredicate IS_MUTABLE = new SCMPredicate("mutable?", SCMPredicate::isMutable);
  public static final SCMPredicate IS_EVEN = new SCMPredicate("even?", o -> (assertClass("even?", o, Integer.class) && (NumberUtils.isZero(Remainder.apply((Number)o, 2L)))));
  public static final SCMPredicate IS_ODD = new SCMPredicate("odd?", o -> (assertClass("odd?", o, Integer.class) && !(NumberUtils.isZero(Remainder.apply((Number)o, 2L)))));
  public static final SCMPredicate IS_KEYWORD = new SCMPredicate("keyword?", o -> (o instanceof SCMKeyword));
  public static final SCMPredicate IS_ANY = new SCMPredicate("any?", o -> true);
  public static final SCMPredicate IS_BLANK = new SCMPredicate("blank?", o -> assertClass("blank?", o, String.class) && o == null || o.toString().isEmpty() || o.toString().trim().isEmpty());
  public static final SCMPredicate IS_CLASS = new SCMPredicate("class?", o -> o instanceof Class);
  public static final SCMPredicate IS_DECIMAL = new SCMPredicate("decimal?", o -> o instanceof BigDecimal);
  public static final SCMPredicate IS_FLOAT = new SCMPredicate("float?", o -> o instanceof Float || o instanceof Double);
  public static final SCMPredicate IS_FN = new SCMPredicate("fn?", SCMPredicate::isProcedure);

  private final String name;
  private final Predicate<Object> predicate;

  private SCMPredicate(String name, Predicate<Object> predicate) {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1));
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
    return !((o instanceof SCMMutableString) || (o instanceof SCMMutableVector));
  }

  private static boolean assertClass(String name, Object o, Class<?> c) {
    if (SCMClass.checkType(o, c)) {
      return true;
    }
    throw new WrongTypeException(name, c.getSimpleName(), o);
  }

  private static boolean isEmpty(Object o) {
    if (o instanceof Collection) {
      return ((Collection)o).isEmpty();
    } else if (o instanceof SCMVector) {
      return ((SCMVector)o).length() == 0;
    } else if (o instanceof CharSequence) {
      return ((CharSequence)o).length() == 0;
    } else if (o instanceof Map) {
      return ((Map)o).size() == 0;
    }
    return false;
  }

  private static boolean isRealized(Object o) {
    if (o instanceof Future) {
      return ((Future) o).isDone();
    }
    throw new WrongTypeException("realized?", "Delay or Promise or Future", o);
  }

  private static boolean isProcedure(Object o) {
    return (o instanceof IFn) && !(o instanceof SCMSymbol) && !(o instanceof SCMKeyword) && !(o instanceof Map) &&
          !(o instanceof SCMVector) && !(o instanceof Map.Entry);
  }
}
