package core.reader.parsers;

import static core.reader.parsers.Result.Type.FAILURE;
import static core.reader.parsers.Result.Type.SUCCESS;
import static core.reader.parsers.Result.failure;

public class StringParser {

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
        Result result = new Result((String)null, SUCCESS, parse.getRest());
        while (parse.getType() == SUCCESS) {
          result = result.merge(parse);
          parse = StringParser.this.parse(parse.getRest());
        }
        return result;
      }
    };
  }

  public StringParser many1() {
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

  public static StringParser choice(StringParser... parsers) {
    return new StringParser("") {
      @Override
      public Result parse(String input) {
        for (StringParser parser : parsers) {
          Result result = parser.parse(input);
          if (result.getType() == SUCCESS) {
            return result;
          }
        }
        return failure(input);
      }
    };
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

  public static StringParser sequence(StringParser... parsers) {
    return new StringParser("") {
      @Override
      public Result parse(String input) {
        Result parse = parsers[0].parse(input);
        Result result = parse;
        for (int i = 1; i < parsers.length; i++) {
          parse = parsers[i].parse(parse.getRest());
          if (parse.getType() == SUCCESS) {
            result = result.merge(parse);
          } else {
            return failure(input);
          }
        }
        return result;
      }
    };
  }

  public static StringParser sequence(String... strings) {
    return new StringParser("") {
      @Override
      public Result parse(String input) {
        Result parse = new StringParser(strings[0]).parse(input);
        Result result = parse;
        for (int i = 1; i < strings.length; i++) {
          parse = new StringParser(strings[i]).parse(parse.getRest());
          if (parse.getType() == SUCCESS) {
            result = result.merge(parse);
          } else {
            return failure(input);
          }
        }
        return result;
      }
    };
  }

  public StringParser between(StringParser open, StringParser close) {
    return new StringParser(str) {
      @Override
      public Result parse(String input) {
        Result o = open.parse(input);
        if (o.getType() == FAILURE) {
          return failure(input);
        }
        Result parse = StringParser.this.parse(o.getRest());
        if (parse.getType() == FAILURE) {
          return failure(input);
        }
        Result c = close.parse(parse.getRest());
        if (c.getType() == FAILURE) {
          return failure(input);
        }
        return parse;
      }
    };
  }

  public StringParser between(String open, String close) {
    return this.between(new StringParser(open), new StringParser(close));
  }
}
