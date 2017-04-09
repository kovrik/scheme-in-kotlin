package core.procedures.io;

import core.exceptions.SCMFileNotFoundException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.procedures.IFn;
import core.scm.SCMCons;
import core.scm.SCMOutputPort;
import core.scm.SCMThunk;

import java.io.FileOutputStream;
import java.io.FileNotFoundException;

public final class CallWithOutputFile extends AFn {

  public CallWithOutputFile() {
    super(new FnArgsBuilder().minArgs(2).maxArgs(2).mandatoryArgsTypes(new Class[]{CharSequence.class, IFn.class}));
  }

  @Override
  public String getName() {
    return "call-with-output-file";
  }

  @Override
  public Object apply(Object... args) {
    String filename = args[0].toString();
    SCMOutputPort outputPort;
    try {
      outputPort = new SCMOutputPort(new FileOutputStream(filename));
    } catch (FileNotFoundException e) {
      throw new SCMFileNotFoundException(filename);
    }
    IFn proc = ((IFn)args[1]);
    SCMCons sexp = SCMCons.list(proc, outputPort);
    return new SCMThunk(sexp, null);
  }
}
