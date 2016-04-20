package main.functions.math;

public interface INumericalOperation<T extends Number> extends IOperation<T> {

  T zero();

  T apply(T first, T second);
}
