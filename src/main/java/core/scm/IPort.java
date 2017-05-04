package core.scm;

import java.io.IOException;

public interface IPort extends ITyped {

  void close() throws IOException;
}
