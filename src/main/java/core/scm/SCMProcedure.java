package core.scm;

import core.environment.Environment;
import core.procedures.AFn;

import java.util.Arrays;
import java.util.List;
import java.util.stream.IntStream;

/* Lambda */
public class SCMProcedure extends AFn {

  private String name;

  /* List of arguments the procedure expects */
  private List<SCMSymbol> args;

  /* Body form of the procedure */
  private Object body;

  /* Lexical environment */
  private Environment localEnvironment = null;

  /* Minimum number of arguments */
  private int minArgs = 0;

  /* Maximum number of arguments */
  private int maxArgs = 255;

  @Override
  public int minArgs() {
    return minArgs;
  }

  @Override
  public int maxArgs() {
    return maxArgs;
  }

  public SCMProcedure(String name, List<SCMSymbol> args, Object body, Environment localEnvironment, boolean isVariadic) {
    this.name = name;
    this.args = (args == null) ? SCMCons.NIL : args;
    this.body = body;
    this.localEnvironment = localEnvironment;
    if (isVariadic) {
      /* Do not count rest arg */
      this.minArgs = this.args.size() - 1;
    } else {
      this.minArgs = this.args.size();
      this.maxArgs = this.args.size();
    }
  }

  public List<SCMSymbol> getArgs() {
    return args;
  }

  public void setName(String name) {
    this.name = name;
  }

  // TODO Optimize
  private Environment bindArgs(Object... values) {
    /* Evaluate mandatory params and put values into new local environment */
    Environment env = new Environment(this.localEnvironment);
    List<SCMSymbol> args = getArgs();
    IntStream.range(0, minArgs).forEach(i -> env.put(args.get(i), values[i]));
    /* If it is a variadic function, then evaluate rest param */
    if (minArgs != maxArgs) {
      /* Optional params: pass them as a list bound to the last param.
       * Everything AFTER mandatory params goes to that list. */
      env.put(args.get(minArgs()), Arrays.asList(Arrays.copyOfRange(values, minArgs(), values.length)));
    }
    return env;
  }

  @Override
  public Object apply0() {
    return new SCMThunk(body, new Environment(this.localEnvironment));
  }

  @Override
  public Object apply1(Object arg1) {
    Environment environment = new Environment(this.localEnvironment);
    environment.put(args.get(0), arg1);
    return new SCMThunk(body, environment);
  }

  @Override
  public Object apply2(Object arg1, Object arg2) {
    Environment environment = new Environment(this.localEnvironment);
    environment.put(args.get(0), arg1);
    environment.put(args.get(1), arg2);
    return new SCMThunk(body, environment);
  }

  @Override
  public Object apply3(Object arg1, Object arg2, Object arg3) {
    Environment environment = new Environment(this.localEnvironment);
    environment.put(args.get(0), arg1);
    environment.put(args.get(1), arg2);
    environment.put(args.get(2), arg3);
    return new SCMThunk(body, environment);
  }

  @Override
  public Object apply4(Object arg1, Object arg2, Object arg3, Object arg4) {
    Environment environment = new Environment(this.localEnvironment);
    environment.put(args.get(0), arg1);
    environment.put(args.get(1), arg2);
    environment.put(args.get(2), arg3);
    environment.put(args.get(3), arg4);
    return new SCMThunk(body, environment);
  }

  @Override
  public Object apply(Object... args) {
    return new SCMThunk(body, bindArgs(args));
  }

  @Override
  public String getName() {
    return name;
  }
}
