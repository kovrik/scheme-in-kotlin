package core.procedures.vectors;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.WrongTypeException;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;
import core.scm.SCMVector;

import java.util.List;

public class VectorRef extends SCMProcedure {

  private static final SCMSymbol vector  = new SCMSymbol("vector");
  private static final SCMSymbol pos = new SCMSymbol("pos");
  private static final List<SCMSymbol> params = SCMCons.list(vector, pos);

  public VectorRef() {
    super("vector-ref", params, null, null, false);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {

    Object o = env.get(vector);
    if (!(o instanceof SCMVector)) {
      throw new WrongTypeException("Vector", o);
    }
    SCMVector vec = (SCMVector)o;

    Object p = env.get(pos);
    if (!(p instanceof Long)) {
      throw new WrongTypeException("Integer", p);
    }
    Long pos = (Long)p;
    if ((pos < 0) || (pos >= vec.length())) {
      throw new IllegalArgumentException(String.format("Value out of range: %s", pos));
    }
    return vec.get(pos.intValue());
  }
}
