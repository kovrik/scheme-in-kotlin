package core.reader;

import java.util.List;

public interface IReader {

  /* Read input stream and
   * parse it into a list of S-expressions. */
  List<Object> read();
}
