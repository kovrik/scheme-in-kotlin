package core.procedures.delayed;

import core.scm.SCMSymbol;
import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.SCMProcedure;

import java.util.List;

public class SCMPromise extends SCMProcedure {

  private Object result;

  public SCMPromise(List<SCMSymbol> params, Object body) {
    super("promise", params, body);
  }

  public Object getResult() {
    return result;
  }

  public void setResult(Object result) {
    this.result = result;
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    if (this.result != null) {
      return this.result;
    }
    Object result = super.apply(evaluator, env);
    this.result = result;
    return result;
  }

  @Override
  public String getName() {
    return "#<promise " + hashCode() + ">";
  }
}
