package core.procedures.io;

import core.exceptions.SCMFileNotFoundException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.procedures.IFn;
import core.scm.Cons;
import core.scm.OutputPort;
import core.scm.Thunk;

import java.io.FileOutputStream;
import java.io.FileNotFoundException;

public final class CallWithOutputFile extends AFn {

  public CallWithOutputFile() {
    super(new FnArgsBuilder().min(2).max(2).mandatory(new Class[]{CharSequence.class, IFn.class}).build());
  }

  @Override
  public String getName() {
    return "call-with-output-file";
  }

  @Override
  public Object apply(Object... args) {
    String filename = args[0].toString();
    OutputPort outputPort;
    try {
      outputPort = new OutputPort(new FileOutputStream(filename));
    } catch (FileNotFoundException e) {
      throw new SCMFileNotFoundException(filename);
    }
    IFn proc = ((IFn)args[1]);
    Cons sexp = Cons.list(proc, outputPort);
    return new Thunk(sexp);
  }
}
