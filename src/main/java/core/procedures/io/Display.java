package core.procedures.io;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.scm.SCMString;
import core.writer.IWriter;
import core.writer.Writer;

import java.io.PrintStream;

import static core.scm.SCMUnspecified.UNSPECIFIED;

public class Display extends AFn {

  @Override
  public String getName() {
    return "display";
  }

  private static final IWriter writer = new Writer();

  private PrintStream printStream;

  public Display(PrintStream printStream) {
    this.printStream = printStream;
  }

  @Override
  public Object invoke(Object... args) {
    if (args.length != 1) {
      throw new ArityException(args.length, 1, getName());
    }
    Object arg = args[0];
    if ((arg instanceof String) || (arg instanceof SCMString)) {
      printStream.print(arg);
    } else {
      printStream.print(writer.toString(arg));
    }
    return UNSPECIFIED;
  }
}
