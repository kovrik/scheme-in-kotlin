package core.procedures.io;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;
import core.scm.specialforms.SCMSpecialForm;

import java.io.PrintStream;
import java.util.List;

public class Newline extends SCMProcedure {

  private static final List<SCMSymbol> params = SCMCons.list();

  private PrintStream printStream;

  public Newline(PrintStream printStream) {
    super("newline", params, null);
    this.printStream = printStream;
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    printStream.println();
    return SCMSpecialForm.UNSPECIFIED;
  }
}
