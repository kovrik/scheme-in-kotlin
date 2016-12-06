package core.scm;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface FnArgs {

  // TODO Args range for variadic
  // TODO Arg type for variadic
  // TODO Class for Pair and List (SCMCons)?
  boolean isVariadic() default false;

  Class<?>[] args() default {};
}
