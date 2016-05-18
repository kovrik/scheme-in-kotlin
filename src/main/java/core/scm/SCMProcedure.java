package core.scm;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.ArityException;
import core.procedures.AFn;

import java.util.Collections;
import java.util.List;

public class SCMProcedure extends AFn {

  private SCMSymbol name;
  private List<SCMSymbol> params;
  private Object body;

  public SCMProcedure(SCMSymbol name, List<SCMSymbol> params, Object body) {
    this.name = name;
    this.params = (params == null) ? Collections.<SCMSymbol>emptyList() : params;
    this.body = body;
  }

  public SCMProcedure(String name, List<SCMSymbol> params, Object body) {
    this(new SCMSymbol(name), params, body);
  }

  public Object getBody() {
    return body;
  }

  public List<SCMSymbol> getParams() {
    return params;
  }

  public SCMSymbol getName() {
    return name;
  }

  public Object apply(IEvaluator evaluator, IEnvironment env) {
    return evaluator.eval(body, env);
  }

  @Override
  public Object throwArity(int actual) {
    throw new ArityException(actual, params.size(), name.getValue());
  }
}
