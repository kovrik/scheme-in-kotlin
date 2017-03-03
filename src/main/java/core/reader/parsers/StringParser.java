package core.reader.parsers;

import static core.reader.parsers.Result.Type.FAILURE;
import static core.reader.parsers.Result.Type.SUCCESS;
import static core.reader.parsers.Result.failure;

public class StringParser {

  private final String str;

  private StringParser(String str) {
    this.str = str;
  }

  public Result parse(String input) {
    if (str.equals(input)) {
      return new Result(str, Result.Type.SUCCESS, "");
    }
    if (input.startsWith(str)) {
      return new Result(str, Result.Type.SUCCESS, input.substring(str.length(), input.length()));
    }
    return failure(input);
  }

  /** Parser Combinators **/
  public StringParser andThenMaybe(StringParser then) {
    return new StringParser(str) {
      @Override
      public Result parse(String input) {
        Result first = StringParser.this.parse(input);
        if (first.getType() == FAILURE) {
          return first;
        }
        Result second = then.parse(first.getRest());
        if (second.getType() == FAILURE) {
          return first;
        }
        return first.merge(second);
      }
    };
  }

  public StringParser or(StringParser then) {
    return new StringParser(str) {
      @Override
      public Result parse(String input) {
        Result first = StringParser.this.parse(input);
        if (first.getType() == SUCCESS) {
          return first;
        }
        Result second = then.parse(first.getRest());
        if (second.getType() == SUCCESS) {
          return second;
        }
        return failure(input);
      }
    };
  }

  public StringParser or(String str) {
    return this.or(new StringParser(str));
  }

  public static StringParser choice(String... strs) {
    return new StringParser("") {
      @Override
      public Result parse(String input) {
        for (String str : strs) {
          StringParser parser = new StringParser(str);
          Result result = parser.parse(input);
          if (result.getType() == SUCCESS) {
            return result;
          }
        }
        return failure(input);
      }
    };
  }
}
