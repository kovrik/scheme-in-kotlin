package core.procedures.io;

import core.exceptions.SCMFileNotFoundException;
import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMInputPort;

import java.io.FileInputStream;
import java.io.FileNotFoundException;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {String.class})
public final class OpenInputFile extends AFn {

  @Override
  public String getName() {
    return "open-input-file";
  }

  @Override
  public Object apply(Object... args) {
    String filename = args[0].toString();
    try {
      return new SCMInputPort(new FileInputStream(filename));
    } catch (FileNotFoundException e) {
      throw new SCMFileNotFoundException(filename);
    }
  }
}
