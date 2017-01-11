package core.scm;

import core.environment.Environment;
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

  /* Minimum number of arguments */
  private int minArgs = 0;

  /* Maximum number of arguments */
  private int maxArgs = 255;

  public SCMProcedure(String name, List<SCMSymbol> args, List<Object> body, IEnvironment localEnvironment, boolean isVariadic) {
    this.name = name;
    this.args = (args == null) ? SCMCons.NIL : args;
    this.body = body;
    this.localEnvironment = localEnvironment;
    if (isVariadic) {
      /* Do not count rest arg */
      this.minArgs = this.args.size() - 1;
      this.maxArgs = this.args.size();
    } else {
      this.minArgs = this.args.size();
      this.maxArgs = this.args.size();
    }
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

  public int minArgs() {
    return minArgs;
  }

  public int maxArgs() {
    return maxArgs;
  }

  public IEnvironment bindArgs(List<Object> values) {
    /* Evaluate mandatory params and put values into new local environment */
    IEnvironment env = new Environment(this.localEnvironment);
    List<SCMSymbol> args = getArgs();
    for (int i = 0; i < minArgs(); i++) {
      env.put(args.get(i), values.get(i));
    }
    /* If it is a variadic function, then evaluate rest param */
    if (minArgs != maxArgs) {
      /* Optional params: pass them as a list bound to the last param.
       * Everything AFTER mandatory params goes to that list. */
      List<Object> varargs = SCMCons.list(values.subList(minArgs(), values.size()));
      env.put(args.get(minArgs()), varargs);
    }
    return env;
  }

  @Override
  public String getName() {
    return name;
  }
}
