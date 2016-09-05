package core.reader.parsers;

import static core.reader.parsers.Result.Type.FAILURE;
import static core.reader.parsers.Result.Type.SUCCESS;
import static core.reader.parsers.Result.failure;

public class CharParser {

  private final char c;

  public CharParser(char c) {
    this.c = c;
  }

  public Result parse(String input) {
    if (input == null || input.isEmpty()) {
      return failure(input);
    }
    if (input.charAt(0) == c) {
      return new Result(Character.toString(c), Result.Type.SUCCESS, input.substring(1, input.length()));
    }
    return failure(input);
  }

  /** Parser Combinators **/
  public CharParser andThen(CharParser then) {
    return new CharParser(c) {
      @Override
      public Result parse(String input) {
        Result first = CharParser.this.parse(input);
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

  public CharParser andThen(char c) {
    return this.andThen(new CharParser(c));
  }

  public CharParser andThenMaybe(CharParser then) {
    return new CharParser(c) {
      @Override
      public Result parse(String input) {
        Result first = CharParser.this.parse(input);
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

  public CharParser andThenMaybe(char c) {
    return this.andThenMaybe(new CharParser(c));
  }

  public CharParser or(CharParser then) {
    return new CharParser(c) {
      @Override
      public Result parse(String input) {
        Result first = CharParser.this.parse(input);
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

  public CharParser or(char c) {
    return this.or(new CharParser(c));
  }

  private static final CharParser ANY = new CharParser(Character.MIN_VALUE) {
    @Override
    public Result parse(String input) {
      if (input == null || input.isEmpty()) {
        return failure(input);
      }
      return new Result(Character.toString(input.charAt(0)), SUCCESS, input.substring(1, input.length()));
    }
  };

  public static CharParser any() {
    return ANY;
  }

  public CharParser notFollowedBy(CharParser then) {
    return new CharParser(c) {
      @Override
      public Result parse(String input) {
        Result first = CharParser.this.parse(input);
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

  public CharParser notFollowedBy(String str) {
    return this.notFollowedBy(new CharParser(c));
  }

  public CharParser many() {
    return new CharParser(c) {
      @Override
      public Result parse(String input) {
        Result parse = CharParser.this.parse(input);
        Result result = new Result((String)null, SUCCESS, parse.getRest());
        while (parse.getType() == SUCCESS) {
          result = result.merge(parse);
          parse = CharParser.this.parse(parse.getRest());
        }
        return result;
      }
    };
  }

  public CharParser many1() {
    return new CharParser(c) {
      @Override
      public Result parse(String input) {
        Result parse = CharParser.this.parse(input);
        if (parse.getType() == FAILURE) {
          return parse;
        }
        Result result = new Result((String)null, SUCCESS, parse.getRest());
        while (parse.getType() == SUCCESS) {
          result = result.merge(parse);
          parse = CharParser.this.parse(parse.getRest());
        }
        return result;
      }
    };
  }

  public CharParser eof() {
    return this.notFollowedBy(any());
  }

  public static CharParser choice(CharParser... parsers) {
    return new CharParser(Character.MIN_VALUE) {
      @Override
      public Result parse(String input) {
        for (CharParser parser : parsers) {
          Result result = parser.parse(input);
          if (result.getType() == SUCCESS) {
            return result;
          }
        }
        return failure(input);
      }
    };
  }

  public static CharParser sequence(CharParser... parsers) {
    return new CharParser(Character.MIN_VALUE) {
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

  public static CharParser sequence(char... chars) {
    return new CharParser(Character.MIN_VALUE) {
      @Override
      public Result parse(String input) {
        Result parse = new CharParser(chars[0]).parse(input);
        Result result = parse;
        for (int i = 1; i < chars.length; i++) {
          parse = new CharParser(chars[i]).parse(parse.getRest());
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

  public static CharParser sequence(String chars) {
    return sequence(chars.toCharArray());
  }

  public static CharParser choice(String chars) {
    return choice(chars.toCharArray());
  }

  public static CharParser choice(char... chars) {
    return new CharParser(Character.MIN_VALUE) {
      @Override
      public Result parse(String input) {
        for (char c : chars) {
          CharParser parser = new CharParser(c);
          Result result = parser.parse(input);
          if (result.getType() == SUCCESS) {
            return result;
          }
        }
        return failure(input);
      }
    };
  }

  public CharParser between(CharParser open, CharParser close) {
    return new CharParser(c) {
      @Override
      public Result parse(String input) {
        Result o = open.parse(input);
        if (o.getType() == FAILURE) {
          return failure(input);
        }
        Result parse = CharParser.this.parse(o.getRest());
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

  public CharParser between(char open, char close) {
    return this.between(new CharParser(open), new CharParser(close));
  }
}
