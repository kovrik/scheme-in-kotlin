package core.scm;

import core.environment.IEnvironment;
import core.exceptions.ArityException;
import core.procedures.AFn;

import java.util.Collections;
import java.util.List;

/* Lambda */
public class SCMProcedure extends AFn {

  private String name;

  /* List of arguments the procedure expects */
  private List<SCMSymbol> args;

  /* Body form of the procedure */
  private Object body;

  /* Lexical environment */
  private IEnvironment localEnvironment = null;

  /* Is it a variadic function? False by default.
   * If set to `true`, then list of all optional params is bound to the last argument */
  private boolean isVariadic = false;

  public SCMProcedure(String name, List<SCMSymbol> args, Object body, IEnvironment localEnvironment) {
    this.name = name;
    this.args = (args == null) ? Collections.emptyList() : args;
    this.body = body;
    this.localEnvironment = localEnvironment;
  }

  public SCMProcedure(String name, List<SCMSymbol> args, Object body, IEnvironment localEnvironment, boolean isVariadic) {
    this.name = name;
    this.args = (args == null) ? Collections.emptyList() : args;
    this.body = body;
    this.localEnvironment = localEnvironment;
    this.isVariadic = isVariadic;
  }

  public Object getBody() {
    return body;
  }

  public List<SCMSymbol> getArgs() {
    return args;
  }

  @Override
  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public IEnvironment getLocalEnvironment() {
    return localEnvironment;
  }

  @Override
  public Object throwArity(int actual) {
    throw new ArityException(actual, args.size(), name);
  }

  public boolean isVariadic() {
    return isVariadic;
  }
}
