package core.procedures.vectors;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.SCMList;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;
import core.scm.SCMVector;
import core.scm.specialforms.SCMSpecialForm;

import java.util.List;

public class MakeVector extends SCMProcedure {

  private static final SCMSymbol size  = new SCMSymbol("size");
  private static final SCMSymbol value = new SCMSymbol("value");
  private static final List<SCMSymbol> params = new SCMList<SCMSymbol>(size, value);

  public MakeVector() {
    super("make-vector", params, null, null, true);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {

    Object o = env.get(size);
    if (!(o instanceof Number)) {
      throw new IllegalArgumentException(String.format("Wrong argument type. Expected: Integer, actual: %s",
                                                       o.getClass().getSimpleName()));
    }
    Long s = (Long)o;
    if (s < 0) {
      throw new IllegalArgumentException("Size value is out of range in `make-vector`");
    }
    SCMList vals = (SCMList)env.get(value);
    if (vals.size() > 1) {
      throw new IllegalArgumentException("Wrong number of arguments to `make-vector'");
    }
    if (vals.isEmpty()) {
      return new SCMVector(s.intValue(), SCMSpecialForm.UNSPECIFIED);
    }
    Object init = vals.getFirst();
    return new SCMVector(s.intValue(), init);
  }
}
