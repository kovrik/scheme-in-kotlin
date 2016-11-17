package core.procedures.io;

import core.exceptions.ArityException;
import core.exceptions.SCMIOException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.IFn;
import core.scm.SCMCons;
import core.scm.SCMOutputPort;
import core.scm.SCMString;
import core.scm.specialforms.TailCall;

import java.io.FileOutputStream;
import java.io.FileNotFoundException;

public class CallWithOutputFile extends AFn {

  @Override
  public String getName() {
    return "call-with-output-file";
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
    SCMOutputPort outputPort;
    try {
      outputPort = new SCMOutputPort(new FileOutputStream(filename));
    } catch (FileNotFoundException e) {
      throw new SCMIOException(e);
    }

    if (!(args[1] instanceof IFn)) {
      throw new WrongTypeException("Procedure", args[0]);
    }
    IFn proc = ((IFn)args[1]);
    SCMCons sexp = SCMCons.list(proc, outputPort);
    return new TailCall(sexp, null);
  }
}
