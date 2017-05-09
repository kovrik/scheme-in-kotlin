package core.reader;

import java.io.IOException;
import java.io.PushbackReader;
import java.util.ArrayList;
import java.util.List;

public class StringReader extends Reader {

  public Object readFirst(String string) {
    reader = new PushbackReader(new java.io.StringReader(string), 1);
    try {
      return nextToken();
    } catch (IOException e) {
      e.printStackTrace();
    } finally {
      try {
        reader.close();
      } catch (IOException ignore) {
      }
    }
    return null;
  }

  public List<Object> read(String string) {
    reader = new PushbackReader(new java.io.StringReader(string), 1);
    try {
      List<Object> tokens = new ArrayList<>();
      Object token;
      while ((token = nextToken()) != null) {
        tokens.add(token);
      }
      return tokens;
    } catch (IOException e) {
      e.printStackTrace();
    } finally {
      try {
        reader.close();
      } catch (IOException ignore) {
      }
    }
    return null;
  }
}
