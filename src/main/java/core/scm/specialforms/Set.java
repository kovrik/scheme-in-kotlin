package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.ISCMClass;
import core.scm.SCMClass;
import core.scm.SCMSymbol;
import core.scm.SCMUnspecified;

import java.util.List;

import static core.scm.SCMUnspecified.UNSPECIFIED;

/* Syntax:
 * (set! <variable> <expression>)
 */
public class Set implements ISpecialForm, ISCMClass {

  public static final Set SET = new Set();

  private final String syntax = "set!";
  private final SCMSymbol symbol = new SCMSymbol(this.syntax);

  private Set() {}

  @Override
  public SCMUnspecified eval(List<Object> expression, IEnvironment env, IEvaluator evaluator) {
    env.findAndPut(expression.get(1), evaluator.eval(expression.get(2), env));
    return UNSPECIFIED;
  }

  public SCMSymbol symbol() {
    return symbol;
  }

  @Override
  public String toString() {
    return syntax;
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.SPECIALFORM;
  }
}
