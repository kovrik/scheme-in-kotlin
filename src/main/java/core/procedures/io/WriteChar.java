package core.procedures.io;

import core.Repl;
import core.exceptions.SCMIOException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMOutputPort;

import java.io.IOException;

public final class WriteChar extends AFn {

  public WriteChar() {
    super(new FnArgsBuilder().min(1).max(2).mandatory(new Class[]{Character.class})
                             .rest(SCMOutputPort.class).build());
  }

  @Override
  public String getName() {
    return "write-char";
  }

  @Override
  public Object apply(Object... args) {
    Character ch = (Character)args[0];
    SCMOutputPort outputPort;
    if (args.length == 1) {
      outputPort = Repl.getCurrentOutputPort();
    } else {
      outputPort = ((SCMOutputPort)args[1]);
    }
    try {
      outputPort.write(ch);
    } catch (IOException e) {
      throw new SCMIOException(e);
    }
    return null;
  }
}
