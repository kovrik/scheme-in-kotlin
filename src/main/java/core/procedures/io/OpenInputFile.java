package core.procedures.io;

import core.exceptions.SCMFileNotFoundException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMInputPort;

import java.io.FileInputStream;
import java.io.FileNotFoundException;

public final class OpenInputFile extends AFn {

  public OpenInputFile() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{String.class}));
  }

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
