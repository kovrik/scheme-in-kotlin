package core.procedures.strings;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;

import java.math.BigDecimal;
import java.util.List;

public class NumberToString extends SCMProcedure {

  private static final SCMSymbol num = new SCMSymbol("num");
  private static final SCMSymbol rad = new SCMSymbol("rad");
  private static final List<SCMSymbol> params = SCMCons.list(num, rad);

  public NumberToString() {
    super("number->string", params, null, null, true);
  }

  @Override
  public String apply(IEvaluator evaluator, IEnvironment env) {
    Object o = env.get(num);
    if (!(o instanceof Number)) {
      throw new WrongTypeException("Number", o);
    }
    List r = (List)env.get(rad);
    if (r.size() > 1) {
      throw new ArityException(r.size() + 1, "number->string");
    }
    Object o1 = null;
    if (r.size() == 1) {
      o1 = r.get(0);
      if (!(o1 instanceof Long)) {
        throw new WrongTypeException("Integer", o);
      }
      if (!(o1.equals(2L) || o1.equals(8L) || o1.equals(10L) || o1.equals(16L))) {
        throw new IllegalArgumentException("Wrong radix: " + o1);
      }
    }
    int radix = (o1 != null) ? ((Number)o1).intValue() : 10;
    if (o instanceof Long) {
      return Long.toString((Long)o, radix);
    }
    if (o instanceof Double) {
      if (radix != 10) {
        throw new IllegalArgumentException("number->string: inexact numbers can only be printed in base 10");
      }
      return o.toString();
    }
    if (o instanceof BigDecimal) {
      BigDecimal bigDecimal = (BigDecimal) o;
      if (radix == 10) {
        return bigDecimal.toString();
      }
      /* Check if it is integral */
      // TODO Trailing zeros?
      if (bigDecimal.remainder(BigDecimal.ONE).equals(BigDecimal.ZERO)) {
        return bigDecimal.toBigInteger().toString(radix);
      }
      throw new IllegalArgumentException("number->string: inexact numbers can only be printed in base 10");
    }
    return o.toString();
  }
}