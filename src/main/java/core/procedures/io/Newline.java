package core.procedures.io;

import core.procedures.AFn;

import java.io.PrintStream;

import static core.scm.SCMUnspecified.UNSPECIFIED;

public class Newline extends AFn {

  @Override
  public String getName() {
    return "newline";
  }

  private PrintStream printStream;

  public Newline(PrintStream printStream) {
    this.printStream = printStream;
  }

  @Override
  public Object invoke(Object... args) {
    printStream.println();
    return UNSPECIFIED;
  }
}
