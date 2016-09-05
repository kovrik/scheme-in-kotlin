package core.reader;

import java.io.InputStream;
import java.util.List;

public interface IReader {

  /* Read input stream and
   * parse it into a list of S-expressions. */
  List<Object> read(InputStream inputStream);

  /* Read a String and
   * parse it into a list of S-expressions. */
  List<Object> read(String string);

  /* Reads and returns the first S-expression
   * (used in Tests) */
  Object readFirst(String string);
}
