package core.procedures.io;

import core.exceptions.ArityException;
import core.exceptions.SCMIOException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.IFn;
import core.scm.SCMCons;
import core.scm.SCMInputPort;
import core.scm.SCMString;
import core.scm.specialforms.TailCall;

import java.io.FileInputStream;
import java.io.FileNotFoundException;

public class CallWithInputFile extends AFn {

  @Override
  public String getName() {
    return "call-with-input-file";
  }

  @Override
  public Object invoke(Object... args) {
    if (args.length != 2) {
      throw new ArityException(args.length, 2, getName());
    }
    if (!(args[0] instanceof String || args[0] instanceof SCMString)) {
      throw new WrongTypeException("String", args[0]);
    }
    String filename = args[0].toString();
    SCMInputPort inputPort;
    try {
      inputPort = new SCMInputPort(new FileInputStream(filename));
    } catch (FileNotFoundException e) {
      throw new SCMIOException(e);
    }

    if (!(args[1] instanceof IFn)) {
      throw new WrongTypeException("Procedure", args[0]);
    }
    IFn proc = ((IFn)args[1]);
    SCMCons sexp = SCMCons.list(proc, inputPort);
    return new TailCall(sexp, null);
  }
}
