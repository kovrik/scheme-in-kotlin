package core.scm;

import core.environment.IEnvironment;
import core.procedures.AFn;

import java.util.List;

/* Lambda */
public class SCMProcedure extends AFn {

  private String name;

  /* List of arguments the procedure expects */
  private List<SCMSymbol> args;

  /* Body form of the procedure */
  private List<Object> body;

  /* Lexical environment */
  private IEnvironment localEnvironment = null;

  /* Is it a variadic function? False by default.
   * If set to `true`, then list of all optional params is to be bound to the last argument */
  private boolean isVariadic = false;

  public SCMProcedure(String name, List<SCMSymbol> args, List<Object> body, IEnvironment localEnvironment) {
    this(name, args, body, localEnvironment, false);
  }

  public SCMProcedure(String name, List<SCMSymbol> args, List<Object> body, IEnvironment localEnvironment, boolean isVariadic) {
    this.name = name;
    this.args = (args == null) ? SCMCons.NIL : args;
    this.body = body;
    this.localEnvironment = localEnvironment;
    this.isVariadic = isVariadic;
  }

  public List<Object> getBody() {
    return body;
  }

  public List<SCMSymbol> getArgs() {
    return args;
  }

  public void setName(String name) {
    this.name = name;
  }

  public IEnvironment getLocalEnvironment() {
    return localEnvironment;
  }

  public boolean isVariadic() {
    return isVariadic;
  }

  @Override
  public String getName() {
    return name;
  }
}
