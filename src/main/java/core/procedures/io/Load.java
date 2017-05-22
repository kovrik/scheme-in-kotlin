package core.procedures.io;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.reader.FileReader;
import core.scm.Cons;
import core.scm.specialforms.Begin;
import core.scm.Thunk;

import java.io.File;
import java.util.List;

public final class Load extends AFn {

  private final FileReader reader = new FileReader();

  public Load() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{CharSequence.class}).build());
  }

  @Override
  public String getName() {
    return "load";
  }

  @Override
  public Object apply1(Object arg) {
    File file = new File(arg.toString());
    List<Object> sexps = Cons.list(Begin.BEGIN);
    sexps.addAll(reader.read(file));
    return new Thunk(sexps);
  }
}
