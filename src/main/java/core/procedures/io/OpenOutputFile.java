package core.procedures.io;

import core.exceptions.SCMFileNotFoundException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMOutputPort;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;

public final class OpenOutputFile extends AFn {

  public OpenOutputFile() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{CharSequence.class}).build());
  }

  @Override
  public String getName() {
    return "open-output-file";
  }

  @Override
  public Object apply(Object... args) {
    String filename = args[0].toString();
    try {
      return new SCMOutputPort(new FileOutputStream(filename));
    } catch (FileNotFoundException e) {
      throw new SCMFileNotFoundException(filename);
    }
  }
}
