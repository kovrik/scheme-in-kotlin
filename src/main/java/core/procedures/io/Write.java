package core.procedures.io;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.scm.SCMUnspecified;
import core.writer.Writer;

public class Write extends AFn {

  @Override
  public String getName() {
    return "write";
  }

  @Override
  public Object invoke(Object... args) {
    // TODO Read input-port as second arg
    if (args.length != 1) {
      throw new ArityException(args.length, 1, getName());
    }
    System.out.print(Writer.write(args[0]));
    return SCMUnspecified.UNSPECIFIED;
  }
}
