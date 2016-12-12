package core.procedures.predicates;

import core.procedures.AFn;
import core.procedures.IFn;
import core.procedures.math.NumericalComparison;
import core.procedures.math.Round;
import core.scm.*;
import core.utils.NumberUtils;

import java.util.List;
import java.util.function.Function;

// TODO Rename to SCMPredicate + implement java.util.function.Predicate
@FnArgs(args = {Object.class})
public class Predicate extends AFn {

  public static final Predicate IS_NULL = new Predicate("null?",       o -> (o == null || ((o instanceof List) && (((List)o).isEmpty()))));
  public static final Predicate IS_EMPTY = new Predicate("empty?",     o -> (o == null || ((o instanceof List) && (((List)o).isEmpty()))));
  public static final Predicate IS_PAIR = new Predicate("pair?",       SCMCons::isPair);
  public static final Predicate IS_LIST = new Predicate("list?",       SCMCons::isList);
  public static final Predicate IS_PROMISE = new Predicate("promise?", o -> (o instanceof SCMPromise));
  public static final Predicate IS_CHAR = new Predicate("char?",       o -> (o instanceof Character));
  public static final Predicate IS_STRING = new Predicate("string?",   o -> (o instanceof SCMMutableString || o instanceof String));
  public static final Predicate IS_VECTOR = new Predicate("vector?",   o -> (o instanceof SCMVector));
  public static final Predicate IS_SYMBOL = new Predicate("symbol?",   o -> (o instanceof SCMSymbol));
  public static final Predicate IS_BOOLEAN = new Predicate("boolean?", o -> (o instanceof SCMBoolean));
  public static final Predicate IS_PROC = new Predicate("procedure?",  o -> (o instanceof IFn));
  public static final Predicate IS_PORT = new Predicate("port?",       o -> (o instanceof ISCMPort));
  public static final Predicate IS_INPUT_PORT = new Predicate("input-port?",   o -> (o instanceof SCMInputPort));
  public static final Predicate IS_OUTPUT_PORT = new Predicate("output-port?", o -> (o instanceof SCMOutputPort));
  public static final Predicate IS_NUMBER = new Predicate("number?",    o -> (o instanceof Number));
  public static final Predicate IS_COMPLEX = new Predicate("complex?",  o -> (o instanceof Number));
  public static final Predicate IS_RATIONAL = new Predicate("rational?", NumberUtils::isRational);
  public static final Predicate IS_REAL = new Predicate("real?",       o -> (o instanceof Number));
  public static final Predicate IS_EOF = new Predicate("eof-object?",  o -> (o instanceof SCMEof));
  public static final Predicate IS_EXACT = new Predicate("exact?",     o -> (SCMClass.assertClass(o, Number.class) && NumberUtils.isExact(o)));
  public static final Predicate IS_INEXACT = new Predicate("inexact?", o -> (SCMClass.assertClass(o, Number.class) && NumberUtils.isInexact(o)));
  public static final Predicate IS_ZERO = new Predicate("zero?",       o -> (SCMClass.assertClass(o, Number.class) &&
                                                                             NumericalComparison.invoke(0L, o, NumericalComparison.Type.EQUAL)));
  public static final Predicate IS_POSITIVE = new Predicate("positive?", o -> (SCMClass.assertClass(o, Number.class) &&
                                                                               NumericalComparison.invoke(o, 0L, NumericalComparison.Type.GREATER)));
  public static final Predicate IS_NEGATIVE = new Predicate("negative?", o -> (SCMClass.assertClass(o, Number.class) &&
                                                                               NumericalComparison.invoke(o, 0L, NumericalComparison.Type.LESS)));
  public static final Predicate IS_INTEGER = new Predicate("integer?", o -> (SCMClass.assertClass(o, Number.class) &&
                                                                             NumericalComparison.invoke(Round.round((Number)o), o, NumericalComparison.Type.EQUAL)));
  public static final Predicate IS_IMMUTABLE = new Predicate("immutable?",  Predicate::isImmutable);
  public static final Predicate IS_MUTABLE   = new Predicate("mutable?",    Predicate::isMutable);

  private final String name;
  private final Function<Object, Boolean> function;

  private Predicate(String name, Function<Object, Boolean> function) {
    this.name = name;
    this.function = function;
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
    return SCMBoolean.toSCMBoolean(function.apply(args[0]));
  }

  private static boolean isMutable(Object o) {
    return !isImmutable(o);
  }

  private static boolean isImmutable(Object o) {
    if ((o instanceof SCMMutableString) || (o instanceof SCMMutableVector)) {
      return false;
    }
    return true;
  }
}
