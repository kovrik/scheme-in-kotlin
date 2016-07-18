package core.procedures.vectors;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.ArityException;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;
import core.scm.SCMVector;
import core.scm.specialforms.SCMSpecialForm;
import core.writer.Writer;

import java.util.List;

public class MakeVector extends SCMProcedure {

  private static final SCMSymbol size  = new SCMSymbol("size");
  private static final SCMSymbol value = new SCMSymbol("value");
  private static final List<SCMSymbol> params = SCMCons.list(size, value);

  public MakeVector() {
    super("make-vector", params, null, null, true);
  }

  @Override
  public SCMVector apply(IEvaluator evaluator, IEnvironment env) {

    Object o = env.get(size);
    if (!(o instanceof Long)) {
      throw new IllegalArgumentException(String.format("Wrong argument type. Expected: Integer, actual: %s",
                                                       Writer.write(o)));
    }
    Long s = (Long)o;
    if (s < 0) {
      throw new IllegalArgumentException("Size value is out of range in `make-vector`");
    }
    List vals = (List)env.get(value);
    if (vals.size() > 1) {
      throw new ArityException(vals.size() + 1, "make-vector");
    }
    if (vals.isEmpty()) {
      return new SCMVector(s.intValue(), SCMSpecialForm.UNSPECIFIED);
    }
    Object init = vals.get(0);
    return new SCMVector(s.intValue(), init);
  }
}
