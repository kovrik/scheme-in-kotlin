package core.scm;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.ArityException;
import core.procedures.AFn;
import core.scm.specialforms.SCMSpecialForm;

import java.util.Collections;
import java.util.List;

public class SCMProcedure extends AFn {

  private SCMSymbol name;
  private List<SCMSymbol> params;
  private Object body;

  // TODO Implement as a separate class?
  private IEnvironment closure = null;

  private boolean variableArity = false;

  public SCMProcedure(SCMSymbol name, List<SCMSymbol> params, Object body) {
    this.name = name;
    this.params = (params == null) ? Collections.emptyList() : params;
    this.body = body;
    this.closure = null;
  }

  public SCMProcedure(SCMSymbol name, List<SCMSymbol> params, Object body, IEnvironment closure) {
    this.name = name;
    this.params = (params == null) ? Collections.emptyList() : params;
    this.body = body;
    this.closure = closure;
  }

  public SCMProcedure(SCMSymbol name, List<SCMSymbol> params, Object body, boolean variableArity) {
    this.name = name;
    this.params = (params == null) ? Collections.emptyList() : params;
    this.body = body;
    this.closure = null;
    this.variableArity = variableArity;
  }

  public SCMProcedure(SCMSymbol name, List<SCMSymbol> params, Object body, IEnvironment closure, boolean variableArity) {
    this.name = name;
    this.params = (params == null) ? Collections.emptyList() : params;
    this.body = body;
    this.closure = closure;
    this.variableArity = variableArity;
  }

  public SCMProcedure(String name, List<SCMSymbol> params, Object body) {
    this(new SCMSymbol(name), params, body);
  }

  public SCMProcedure(String name, List<SCMSymbol> params, Object body, IEnvironment closure) {
    this(new SCMSymbol(name), params, body, closure);
  }

  public SCMProcedure(String name, List<SCMSymbol> params, Object body, IEnvironment closure, boolean variableArity) {
    this(new SCMSymbol(name), params, body, closure, variableArity);
  }

  public Object getBody() {
    return body;
  }

  public List<SCMSymbol> getParams() {
    return params;
  }

  @Override
  public String getName() {
    if (name == null) {
      return "#<procedure " + hashCode() + ">";
    }
    return name.getValue();
  }

  public Object apply(IEvaluator evaluator, IEnvironment env) {
    /* Here we avoid passing first implicit `begin` (added by `define` Special Form) Symbol to evaluator,
     * but let `begin` SpecialForm evaluate the body.
     * Otherwise, it would be impossible to define and apply procedures
     * if user has redefined `begin` symbol to something else:
     * Evaluator would try to evaluate implicit `begin`,
     * but will get redefined `begin` Symbol from environment.
     *
     * See EvaluatorTest.testRedefineSpecialForms() */
    if ((body instanceof SCMCons) && (SCMSpecialForm.BEGIN.symbol().equals(((SCMCons)body).car()))) {
      return SCMSpecialForm.BEGIN.eval((SCMCons)body, env, evaluator);
    }
    /* No implicit `begin`, evaluate as usual */
    return evaluator.eval(body, env);
  }

  public IEnvironment getClosure() {
    return closure;
  }

  @Override
  public Object throwArity(int actual) {
    throw new ArityException(actual, params.size(), name.getValue());
  }

  public boolean isVariableArity() {
    return variableArity;
  }
}
