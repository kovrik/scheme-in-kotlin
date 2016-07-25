package core.procedures.io;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.scm.specialforms.SCMSpecialForm;
import core.writer.IWriter;
import core.writer.Writer;

import java.io.PrintStream;

public class Display extends AFn {

  private static final IWriter writer = new Writer();

  private PrintStream printStream;

  public Display(PrintStream printStream) {
    this.printStream = printStream;
  }

  @Override
  public Object invoke(Object... args) {
    if (args.length != 1) {
      throw new ArityException(args.length, 1, "display");
    }
    Object arg = args[0];
    if (arg instanceof String) {
      printStream.print(arg);
    } else {
      printStream.print(writer.toString(arg));
    }
    return SCMSpecialForm.UNSPECIFIED;
  }
}
