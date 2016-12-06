package core.scm;

import java.io.IOException;

public interface ISCMPort extends ISCMClass {

  void close() throws IOException;
}
