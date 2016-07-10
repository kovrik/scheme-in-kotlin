package core.procedures.io;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;
import core.scm.specialforms.SCMSpecialForm;
import core.writer.IWriter;
import core.writer.Writer;

import java.io.PrintStream;
import java.util.List;

public class Display extends SCMProcedure {

  private static final IWriter writer = new Writer();
  private static final SCMSymbol value = new SCMSymbol("value");
  private static final List<SCMSymbol> params = SCMCons.list(value);

  private PrintStream printStream;

  public Display(PrintStream printStream) {
    super("display", params, value);
    this.printStream = printStream;
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    Object result = super.apply(evaluator, env);
    printStream.print(writer.toString(result));
    return SCMSpecialForm.UNSPECIFIED;
  }
}
