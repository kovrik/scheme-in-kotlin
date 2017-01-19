package core.procedures.io;

import core.procedures.AFn;
import core.reader.FileReader;
import core.scm.FnArgs;
import core.scm.SCMCons;
import core.scm.specialforms.Begin;
import core.scm.SCMThunk;

import java.io.File;
import java.util.List;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {String.class})
public final class Load extends AFn {

  private final FileReader reader = new FileReader();

  @Override
  public String getName() {
    return "load";
  }

  @Override
  public Object apply(Object... args) {
    File file = new File(args[0].toString());
    List<Object> sexps = SCMCons.list(Begin.BEGIN);
    sexps.addAll(reader.read(file));
    return new SCMThunk(sexps, null);
  }
}
