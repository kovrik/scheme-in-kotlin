package unittests;

import core.scm.SCMBigComplex;
import org.junit.Test;

import java.math.BigDecimal;

import static org.junit.Assert.assertEquals;

public class ComplexTest extends AbstractTest {

  @Test
  public void testAddition() {
    assertEquals(new SCMBigComplex(new BigDecimal("4.75"), new BigDecimal("2.0")), eval("(+ 1 3/4 2.5 -2.5+6i 3-4i)", env));
    assertEquals(new SCMBigComplex(new BigDecimal("0.0"), new BigDecimal("0.0")),  eval("(+ 0 0/4 0.0 -000+0i 0-0i)", env));
    assertEquals(new SCMBigComplex(new BigDecimal("0"), new BigDecimal("0")),  eval("(+ 1+1i -1-1i)", env));
  }

  @Test
  public void testSubtraction() {
    assertEquals(new SCMBigComplex(new BigDecimal("-2.75"), new BigDecimal("-2.0")), eval("(- 1 3/4 2.5 -2.5+6i 3-4i)", env));
    assertEquals(0.0,  eval("(- 0 0/4 0.0 -000+0i 0-0i)", env));
    assertEquals(new SCMBigComplex(new BigDecimal("2"), new BigDecimal("2")),  eval("(- 1+1i -1-1i)", env));
  }

  @Test
  public void testMultiplication() {
    assertEquals(new SCMBigComplex(new BigDecimal("30.9375"), new BigDecimal("52.5")), eval("(* 1 3/4 2.5 -2.5+6i 3-4i)", env));
    assertEquals(0L,  eval("(* 0 0/4 0.0 -000+0i 0-0i)", env));
    assertEquals(new SCMBigComplex(new BigDecimal("0"), new BigDecimal("-2")),  eval("(* 1+1i -1-1i)", env));
  }

  @Test
  public void testDivision() {
    assertEquals(new SCMBigComplex(new BigDecimal("0.0083313609467456"), new BigDecimal("-0.0141380670611440")),
                 eval("(/ 1 3/4 2.5 -2.5+6i 3-4i)", env));

    assertEquals(new SCMBigComplex(new BigDecimal("-1"), new BigDecimal("0")),  eval("(/ 1+1i -1-1i)", env));
  }
}
