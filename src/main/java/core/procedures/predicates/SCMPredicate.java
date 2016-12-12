package core.procedures.predicates;

import core.procedures.AFn;
import core.procedures.IFn;
import core.procedures.math.NumericalComparison;
import core.scm.*;
import core.utils.NumberUtils;

import java.util.List;
import java.util.function.Predicate;

@FnArgs(args = {Object.class})
public class SCMPredicate extends AFn {

  public static final SCMPredicate IS_NULL = new SCMPredicate("null?", SCMCons::isNull);
  public static final SCMPredicate IS_EMPTY = new SCMPredicate("empty?", o -> (o == null || ((o instanceof List) && (((List)o).isEmpty()))));
  public static final SCMPredicate IS_PAIR = new SCMPredicate("pair?", SCMCons::isPair);
  public static final SCMPredicate IS_LIST = new SCMPredicate("list?", SCMCons::isList);
  public static final SCMPredicate IS_PROMISE = new SCMPredicate("promise?", o -> (o instanceof SCMPromise));
  public static final SCMPredicate IS_CHAR = new SCMPredicate("char?", o -> (o instanceof Character));
  public static final SCMPredicate IS_STRING = new SCMPredicate("string?", o -> (o instanceof SCMMutableString || o instanceof String));
  public static final SCMPredicate IS_VECTOR = new SCMPredicate("vector?", o -> (o instanceof SCMVector));
  public static final SCMPredicate IS_SYMBOL = new SCMPredicate("symbol?", o -> (o instanceof SCMSymbol));
  public static final SCMPredicate IS_BOOLEAN = new SCMPredicate("boolean?", o -> (o instanceof SCMBoolean));
  public static final SCMPredicate IS_PROC = new SCMPredicate("procedure?", o -> (o instanceof IFn));
  public static final SCMPredicate IS_PORT = new SCMPredicate("port?", o -> (o instanceof ISCMPort));
  public static final SCMPredicate IS_INPUT_PORT = new SCMPredicate("input-port?", o -> (o instanceof SCMInputPort));
  public static final SCMPredicate IS_OUTPUT_PORT = new SCMPredicate("output-port?", o -> (o instanceof SCMOutputPort));
  public static final SCMPredicate IS_NUMBER = new SCMPredicate("number?", o -> (o instanceof Number));
  public static final SCMPredicate IS_COMPLEX = new SCMPredicate("complex?", o -> (o instanceof Number));
  public static final SCMPredicate IS_RATIONAL = new SCMPredicate("rational?", NumberUtils::isRational);
  public static final SCMPredicate IS_REAL = new SCMPredicate("real?", o -> (o instanceof Number));
  public static final SCMPredicate IS_EOF = new SCMPredicate("eof-object?", o -> (o instanceof SCMEof));
  public static final SCMPredicate IS_EXACT = new SCMPredicate("exact?", o -> (SCMClass.assertClass(o, Number.class) && NumberUtils.isExact(o)));
  public static final SCMPredicate IS_INEXACT = new SCMPredicate("inexact?", o -> (SCMClass.assertClass(o, Number.class) && NumberUtils.isInexact(o)));
  public static final SCMPredicate IS_ZERO = new SCMPredicate("zero?", o -> (SCMClass.assertClass(o, Number.class) &&
                                                                             NumericalComparison.invoke(0L, o, NumericalComparison.Type.EQUAL)));
  public static final SCMPredicate IS_POSITIVE = new SCMPredicate("positive?", o -> (SCMClass.assertClass(o, Number.class) &&
                                                                               NumericalComparison.invoke(o, 0L, NumericalComparison.Type.GREATER)));
  public static final SCMPredicate IS_NEGATIVE = new SCMPredicate("negative?", o -> (SCMClass.assertClass(o, Number.class) &&
                                                                               NumericalComparison.invoke(o, 0L, NumericalComparison.Type.LESS)));
  public static final SCMPredicate IS_INTEGER = new SCMPredicate("integer?", NumberUtils::isInteger);
  public static final SCMPredicate IS_IMMUTABLE = new SCMPredicate("immutable?", SCMPredicate::isImmutable);
  public static final SCMPredicate IS_MUTABLE   = new SCMPredicate("mutable?", SCMPredicate::isMutable);

  private final String name;
  private final Predicate<Object> predicate;

  private SCMPredicate(String name, Predicate<Object> predicate) {
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
  public SCMBoolean invoke(Object... args) {
    return SCMBoolean.toSCMBoolean(predicate.test(args[0]));
  }

  private static boolean isMutable(Object o) {
    return !isImmutable(o);
  }

  private static boolean isImmutable(Object o) {
    return !((o instanceof SCMMutableString) || (o instanceof SCMMutableVector));
  }
}
