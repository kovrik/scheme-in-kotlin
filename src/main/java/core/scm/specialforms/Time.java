package core.scm.specialforms;

import core.Repl;
import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.exceptions.SCMIOException;
import core.scm.SCMUnspecified;
import core.writer.Writer;

import java.io.IOException;
import java.util.List;

/**
 * Time Special Form:
 *
 * Counts time taken for evaluation.
 *
 * Syntax:
 * (time <expression1> ... <expression n>)
 */
public enum Time implements ISpecialForm {
  TIME;

  private static final String LS = System.getProperty("line.separator");

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.size() < 2) {
      throw IllegalSyntaxException.of(toString(), expression);
    }
    long start = System.nanoTime();
    for (int i = 1; i < expression.size() - 1; i++) {
      evaluator.eval(expression.get(i), env);
    }
    try {
      Repl.getCurrentOutputPort().write(Writer.write(evaluator.eval(expression.get(expression.size() - 1), env)) + LS);
    } catch (IOException e) {
      throw new SCMIOException(e);
    }
    long diff = (System.nanoTime() - start) / 1000000;
    try {
      Repl.getCurrentOutputPort().write(String.format("time: %s ms", diff) + LS);
    } catch (IOException e) {
      throw new SCMIOException(e);
    }
    return SCMUnspecified.UNSPECIFIED;
  }

  @Override
  public String toString() {
    return "time";
  }
}
