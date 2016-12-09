package core.procedures.io;

import core.exceptions.ArityException;
import core.exceptions.SCMFileNotFoundException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMInputPort;
import core.scm.SCMMutableString;

import java.io.FileInputStream;
import java.io.FileNotFoundException;

public class OpenInputFile extends AFn {

  @Override
  public String getName() {
    return "open-input-file";
  }

  @Override
  public Object invoke(Object... args) {
    if (args.length != 1) {
      throw new ArityException(args.length, 1, getName());
    }
    if (!(args[0] instanceof String || args[0] instanceof SCMMutableString)) {
      throw new WrongTypeException("String", args[0]);
    }
    String filename = args[0].toString();
    try {
      return new SCMInputPort(new FileInputStream(filename));
    } catch (FileNotFoundException e) {
      throw new SCMFileNotFoundException(filename);
    }
  }
}
