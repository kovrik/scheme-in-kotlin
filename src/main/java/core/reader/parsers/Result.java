package core.reader.parsers;

import java.util.ArrayList;
import java.util.List;

public class Result {

  public enum Type {
    SUCCESS,
    FAILURE
  }

  private final Type type;
  private List<String> match = new ArrayList<>();
  private final String rest;

  public Result(String match, Type type, String rest) {
    if (match != null) {
      addMatch(match);
    }
    this.type = type;
    this.rest = rest;
  }

  public Result(List<String> match, Type type, String rest) {
    this.match = match;
    this.type = type;
    this.rest = rest;
  }

  public List<String> addMatch(String match) {
    this.match.add(match);
    return this.match;
  }

  public Type getType() {
    return type;
  }

  public List<String> getMatch() {
    return match;
  }

  public String getRest() {
    return rest;
  }

  public Result merge(Result other) {
    if (type == Type.FAILURE || other.getType() == Type.FAILURE) {
      throw new UnsupportedOperationException("Can't merge failed parse results!");
    }
    match.addAll(other.getMatch());
    return new Result(match, Type.SUCCESS, other.getRest());
  }

  public static Result failure(String input) {
    return new Result((String)null, Type.FAILURE, input);
  }

  @Override
  public String toString() {
    return "Result{" + "type=" + type + ", match=" + match + ", rest=" + rest + '}';
  }
}
