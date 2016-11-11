package core.procedures.io;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.reader.IReader;
import core.reader.Reader;

public class ReadChar extends AFn {

  private final IReader reader = new Reader();

  @Override
  public String getName() {
    return "read-char";
  }

  @Override
  public Object invoke(Object... args) {
    // TODO Read input-port as first arg
    if (args.length > 0) {
      throw new ArityException(args.length, 0, getName());
    }
    return reader.readChar(System.in);
  }
}
