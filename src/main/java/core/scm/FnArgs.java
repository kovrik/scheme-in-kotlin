package core.scm;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface FnArgs {

  // TODO Performance hit? Replace with abstract methods in AFn?

  /* Minimum number of arguments */
  short minArgs() default 0;

  /* Types of mandatory arguments */
  Class<?>[] mandatoryArgsTypes() default {};

  /* Maximum number of arguments.
   * If not equal to minArgs, then procedure is variadic.
   *
   * By default it is 255, which means ANY number of arguments:
   * method can't have more than 255 args anyway.
   *
   * See: https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.11 */
  short maxArgs() default 255;

  Class<?>[] restArgsType() default {};

  /* Special case for the last argument (for example, see Append procedure) */
  Class<?>[] lastArgType() default {};
}
