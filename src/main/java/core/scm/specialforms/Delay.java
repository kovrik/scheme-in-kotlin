package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.ISCMClass;
import core.scm.SCMClass;
import core.scm.SCMPromise;
import core.scm.SCMSymbol;

import java.util.List;

/* Syntax:
 * (delay <expression>)
 */
public class Delay implements ISpecialForm, ISCMClass {

  public static final ISpecialForm DELAY = new Delay();

  private final String syntax = "delay";
  private final SCMSymbol symbol = new SCMSymbol(this.syntax);

  private Delay() {}

  @Override
  public SCMPromise eval(List<Object> expression, IEnvironment env, IEvaluator evaluator) {
    if (expression.size() < 2) {
      throw new IllegalSyntaxException("delay: bad `delay` in form: " + expression);
    }
    return new SCMPromise(expression.get(1));
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
