package core.procedures.io;

import core.exceptions.SCMFileNotFoundException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.procedures.IFn;
import core.scm.Cons;
import core.scm.InputPort;
import core.scm.Thunk;

import java.io.FileInputStream;
import java.io.FileNotFoundException;

public final class CallWithInputFile extends AFn {

  public CallWithInputFile() {
    super(new FnArgsBuilder().min(2).max(2).mandatory(new Class[]{CharSequence.class, IFn.class}).build());
  }

  @Override
  public String getName() {
    return "call-with-input-file";
  }

  @Override
  public Object apply(Object... args) {
    String filename = args[0].toString();
    InputPort inputPort;
    try {
      inputPort = new InputPort(new FileInputStream(filename));
    } catch (FileNotFoundException e) {
      throw new SCMFileNotFoundException(filename);
    }
    IFn proc = ((IFn)args[1]);
    Cons sexp = Cons.list(proc, inputPort);
    return new Thunk(sexp);
  }
}
