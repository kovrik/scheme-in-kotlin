package core.scm;

public interface ICons {

  boolean isList();

  boolean isPair();

  boolean isNull();

  Object car();

  Object cdr();
}
