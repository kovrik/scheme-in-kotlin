package core.procedures.vectors;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.*;
import core.scm.specialforms.SCMSpecialForm;

import java.util.List;

public class VectorSet extends SCMProcedure {

  private static final SCMSymbol vector  = new SCMSymbol("vector");
  private static final SCMSymbol pos = new SCMSymbol("pos");
  private static final SCMSymbol v = new SCMSymbol("v");
  private static final List<SCMSymbol> params = SCMCons.list(vector, pos, v);

  public VectorSet() {
    super("vector-set!", params, null, null, false);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {

    Object o = env.get(vector);
    if (!(o instanceof SCMVector)) {
      throw new IllegalArgumentException(String.format("Wrong argument type. Expected: Vector, actual: %s",
                                                       o));
    }
    SCMVector vec = (SCMVector)o;

    Object p = env.get(pos);
    if (!(p instanceof Long)) {
      throw new IllegalArgumentException(String.format("Wrong argument type. Expected: Integer, actual: %s",
                                                       p));
    }
    Long pos = (Long)p;
    if ((pos < 0) || (pos >= vec.length())) {
      throw new IllegalArgumentException(String.format("Value out of range: %s", pos));
    }
    vec.set(pos.intValue(), env.get(v));
    return SCMSpecialForm.UNSPECIFIED;
  }
}
