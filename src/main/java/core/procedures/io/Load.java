package core.procedures.io;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.reader.FileReader;
import core.scm.SCMCons;
import core.scm.SCMString;
import core.scm.specialforms.Begin;
import core.scm.specialforms.TailCall;

import java.io.File;
import java.util.List;

public class Load extends AFn {

  private final FileReader reader = new FileReader();

  @Override
  public String getName() {
    return "load";
  }

  @Override
  public Object invoke(Object... args) {
    if (args.length != 1) {
      throw new ArityException(args.length, 1, getName());
    }
    if (args[0] instanceof String || args[0] instanceof SCMString) {
      File file = new File(args[0].toString());
      // TODO Is BEGIN Ok here?
      List<Object> sexps = SCMCons.list(Begin.BEGIN);
      sexps.addAll(reader.read(file));
      return new TailCall(sexps, null);
    }
    throw new WrongTypeException("String", args[0]);
  }
}
