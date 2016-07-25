package core.procedures.io;

import core.procedures.AFn;
import core.scm.specialforms.SCMSpecialForm;

import java.io.PrintStream;

public class Newline extends AFn {

  private PrintStream printStream;

  public Newline(PrintStream printStream) {
    this.printStream = printStream;
  }

  @Override
  public Object invoke(Object... args) {
    printStream.println();
    return SCMSpecialForm.UNSPECIFIED;
  }
}
