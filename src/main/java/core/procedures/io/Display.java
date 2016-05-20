package core.procedures.io;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.SCMList;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;

import java.io.PrintStream;
import java.util.List;

public class Display extends SCMProcedure {

  private static final SCMSymbol value = new SCMSymbol("value");
  private static final List<SCMSymbol> params = new SCMList<SCMSymbol>(value);

  private PrintStream printStream;

  public Display(PrintStream printStream) {
    super("display", params, value);
    this.printStream = printStream;
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    Object result = super.apply(evaluator, env);
    printStream.println(result);
    return null;
  }
}
