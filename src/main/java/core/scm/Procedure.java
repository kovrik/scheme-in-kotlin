package core.scm;

import core.environment.Environment;
import core.procedures.AFn;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;

/* Lambda */
public class Procedure extends AFn {

  private String name;

  /* List of arguments the procedure expects */
  private final List<Symbol> args;

  /* Body form of the procedure */
  private final Object body;

  /* Is body a constant? If it is, then no need to evaluate it */
  private final boolean isBodyConst;

  /* Lexical environment */
  private Environment localEnvironment = null;

  public Procedure(String name, List<Symbol> args, Object body, Environment localEnvironment, boolean isVariadic) {
    this.name = name;
    this.args = (args == null) ? Cons.EMPTY : args;
    this.body = body;
    this.isBodyConst = isConst(body);
    this.localEnvironment = localEnvironment;
    if (isVariadic) {
      /* Do not count rest arg */
      this.minArgs = this.args.size() - 1;
    } else {
      this.minArgs = this.args.size();
      this.maxArgs = this.args.size();
    }
  }

  // TODO Check collections?
  private static boolean isConst(Object obj) {
    return !((obj instanceof Symbol) || (obj instanceof List) || (obj instanceof Map) ||
             (obj instanceof Vector) || (obj instanceof Set));
  }

  private List<Symbol> getArgs() {
    return args;
  }

  public void setName(String name) {
    this.name = name;
  }

  // TODO Optimize
  private Environment bindArgs(Object... values) {
    /* Evaluate mandatory params and put values into new local environment */
    Environment env = new Environment(values.length, this.localEnvironment);
    List<Symbol> args = getArgs();
    for (int i = 0; i < minArgs; i++) {
      env.put(args.get(i), values[i]);
    }
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
    if (isBodyConst) {
      return body;
    }
    return new Thunk(body, new Environment(0, this.localEnvironment));
  }

  @Override
  public Object apply1(Object arg1) {
    if (isBodyConst) {
      return body;
    }
    Environment environment = new Environment(1, this.localEnvironment);
    environment.put(args.get(0), arg1);
    return new Thunk(body, environment);
  }

  @Override
  public Object apply2(Object arg1, Object arg2) {
    if (isBodyConst) {
      return body;
    }
    Environment environment = new Environment(2, this.localEnvironment);
    environment.put(args.get(0), arg1);
    environment.put(args.get(1), arg2);
    return new Thunk(body, environment);
  }

  @Override
  public Object apply3(Object arg1, Object arg2, Object arg3) {
    if (isBodyConst) {
      return body;
    }
    Environment environment = new Environment(3, this.localEnvironment);
    environment.put(args.get(0), arg1);
    environment.put(args.get(1), arg2);
    environment.put(args.get(2), arg3);
    return new Thunk(body, environment);
  }

  @Override
  public Object apply4(Object arg1, Object arg2, Object arg3, Object arg4) {
    if (isBodyConst) {
      return body;
    }
    Environment environment = new Environment(4, this.localEnvironment);
    environment.put(args.get(0), arg1);
    environment.put(args.get(1), arg2);
    environment.put(args.get(2), arg3);
    environment.put(args.get(3), arg4);
    return new Thunk(body, environment);
  }

  @Override
  public Object apply(Object... args) {
    if (isBodyConst) {
      return body;
    }
    return new Thunk(body, bindArgs(args));
  }

  @Override
  public String getName() {
    return name;
  }
}