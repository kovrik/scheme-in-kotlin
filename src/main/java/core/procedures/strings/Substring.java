package core.procedures.strings;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;

import java.util.List;

public class Substring extends SCMProcedure {

  private static final SCMSymbol string  = new SCMSymbol("string");
  private static final SCMSymbol start = new SCMSymbol("start");
  private static final SCMSymbol end = new SCMSymbol("end");
  private static final List<SCMSymbol> params = SCMCons.list(string, start, end);

  public Substring() {
    super("substring", params, null, null, true);
  }

  @Override
  public String apply(IEvaluator evaluator, IEnvironment env) {
    Object o = env.get(string);
    if (!(o instanceof String)) {
      throw new WrongTypeException("String", o);
    }
    String s = (String)o;

    Object p = env.get(start);
    if (!(p instanceof Long)) {
      throw new WrongTypeException("Integer", p);
    }
    long start = (long)p;
    if ((start < 0) || (start >= s.length())) {
      throw new IllegalArgumentException(String.format("Value out of range: %s", start));
    }
    long end = (long)s.length();

    List e = (List) env.get(Substring.end);
    if (e.size() > 1) {
      throw new ArityException(e.size() + 1, "substring");
    }
    if (!e.isEmpty()) {
      Object oe = e.get(0);
      if (!(oe instanceof Long)) {
        throw new WrongTypeException("Integer", oe);
      }
      end = (long) oe;
      if ((end < 0) || (end >= s.length())) {
        throw new IllegalArgumentException(String.format("Value out of range: %s", end));
      }
    }
    return s.substring((int)start, (int)end);
  }
}
