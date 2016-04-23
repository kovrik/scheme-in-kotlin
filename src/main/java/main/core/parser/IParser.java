package main.core.parser;

import java.io.InputStream;

public interface IParser {

  Object parse(InputStream inputStream);
}
