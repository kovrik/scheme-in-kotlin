package core.procedures.vectors;

import core.scm.SCMList;
import core.scm.SCMSymbol;
import core.scm.SCMVector;
import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.SCMProcedure;

import java.util.Arrays;
import java.util.List;

public class MakeVector extends SCMProcedure {

  private static final SCMSymbol SIZEP = new SCMSymbol("size");
  private static final SCMSymbol VP = new SCMSymbol("v");

  public MakeVector() {
    super("make-vector", new SCMList<SCMSymbol>(SIZEP, VP), null);
  }

  public MakeVector(List<SCMSymbol> params, Object body) {
    super("make-vector", new SCMList<SCMSymbol>(SIZEP, VP), null);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    Object s = env.get(SIZEP);
    if (!(s instanceof Number)) {
      throw new IllegalArgumentException("Wrong type of `size` argument to `make-vector`!");
    }
    int size = ((Number)s).intValue();
    Object[] objects = new Object[size];

    Object v = env.get(VP);
    if (v != null) {
      Arrays.fill(objects, v);
    }
    return new SCMVector(objects);
  }
}
