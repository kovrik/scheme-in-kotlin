package core.scm;

import java.util.concurrent.ExecutionException;
public interface IDeref {

  Object deref() throws ExecutionException, InterruptedException;
}
