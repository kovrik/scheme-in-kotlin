package core.reader;

import java.io.InputStream;

public interface IReader {

  Object read(InputStream inputStream);

  Object read(String string);
}
