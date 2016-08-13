package core.reader.parsers;

import core.reader.IReader;

import java.io.InputStream;

import static core.reader.parsers.Result.Type.FAILURE;
import static core.reader.parsers.Result.Type.SUCCESS;
import static core.reader.parsers.Result.failure;

public class StringParser implements IReader {

  private final String str;

  public StringParser(String str) {
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
  public StringParser andThen(StringParser then) {
    return new StringParser(str) {
      @Override
      public Result parse(String input) {
        Result first = StringParser.this.parse(input);
        if (first.getType() == FAILURE) {
          return first;
        }
        Result second = then.parse(first.getRest());
        if (second.getType() == FAILURE) {
          return failure(input);
        }
        return first.merge(second);
      }
    };
  }

  public StringParser andThen(String str) {
    return this.andThen(new StringParser(str));
  }

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

  public StringParser andThenMaybe(String str) {
    return this.andThenMaybe(new StringParser(str));
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

  private static final StringParser ANY = new StringParser("") {
    @Override
    public Result parse(String input) {
      if (input == null || input.isEmpty()) {
        return failure(input);
      }
      return new Result(input, SUCCESS, "");
    }
  };

  public static StringParser any() {
    return ANY;
  }

  public StringParser notFollowedBy(StringParser then) {
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
        return failure(input);
      }
    };
  }

  public StringParser notFollowedBy(String str) {
    return this.notFollowedBy(new StringParser(str));
  }

  public StringParser many() {
    return new StringParser(str) {
      @Override
      public Result parse(String input) {
        Result parse = StringParser.this.parse(input);
        if (parse.getType() == FAILURE) {
          return parse;
        }
        Result result = new Result((String)null, SUCCESS, parse.getRest());
        while (parse.getType() == SUCCESS) {
          result = result.merge(parse);
          parse = StringParser.this.parse(parse.getRest());
        }
        return result;
      }
    };
  }

  public StringParser eof() {
    return this.notFollowedBy(any());
  }

  @Override
  public Object read(InputStream inputStream) {
    return null;
  }

  @Override
  public Object read(String string) {
    return null;
  }
}
