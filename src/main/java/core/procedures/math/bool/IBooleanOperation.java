package core.procedures.math.bool;

import core.procedures.math.IOperation;

public interface IBooleanOperation<T extends Boolean> extends IOperation<T> {

  T zero();

  T apply(T first, T second);
}
