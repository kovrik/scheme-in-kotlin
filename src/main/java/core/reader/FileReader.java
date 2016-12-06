package core.reader;

import core.exceptions.IllegalSyntaxException;
import core.exceptions.SCMFileNotFoundException;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PushbackReader;
import java.util.ArrayList;
import java.util.List;

public class FileReader extends Reader {

  public List<Object> read(File file) {
    try {
      reader = new PushbackReader(new BufferedReader(new java.io.FileReader(file)), 2);
    } catch (FileNotFoundException e) {
      throw new SCMFileNotFoundException(file.getPath());
    }
    List<Object> tokens = new ArrayList<>();
    try {
      int read;
      while ((read = reader.read()) != -1) {
        reader.unread(read);
        Object token = nextToken();
        if (DOT.equals(token)) {
          throw new IllegalSyntaxException("read: illegal use of '.'");
        }
        if (token != null) {
          tokens.add(token);
        }
      }
    } catch (IOException e) {
      e.printStackTrace();
    } finally {
      try {
        reader.close();
      } catch (IOException ignore) {
      }
    }
    return tokens;
  }
}
