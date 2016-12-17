package unittests;

import core.scm.SCMBigComplex;
import org.junit.Test;

import java.math.BigDecimal;

import static core.scm.SCMBoolean.FALSE;
import static core.scm.SCMBoolean.TRUE;
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

  @Test
  public void testExactness() {
    assertEquals(TRUE,  eval("(exact? 1+2i)", env));
    assertEquals(TRUE,  eval("(exact? 0+2i)", env));
    assertEquals(FALSE, eval("(exact? 0+2.0i)", env));
    assertEquals(FALSE, eval("(exact? 2.3+2.4i)", env));
    assertEquals(FALSE, eval("(exact? 0.0+1i)", env));
    assertEquals(FALSE, eval("(inexact? 1+2i)", env));
    assertEquals(FALSE, eval("(inexact? 0+2i)", env));
    assertEquals(TRUE,  eval("(inexact? 0+2.0i)", env));
    assertEquals(TRUE,  eval("(inexact? 0.0+1i)", env));
    assertEquals(FALSE, eval("(exact? (exact->inexact 1+2i))", env));
    // FIXME Rationals
//    assertEquals(TRUE,  eval("(exact? (inexact->exact 1.3+2.4i))", env));
  }

  @Test
  public void testSqrt() {
    assertEquals(new SCMBigComplex(new BigDecimal("1.09868411346781"), new BigDecimal("0.4550898605622274")), eval("(sqrt 1+1i)", env));
    assertEquals(new SCMBigComplex(new BigDecimal("0.7071067811865476"), new BigDecimal("-2.1213203435596424")), eval("(sqrt -4-3i)", env));
  }

  @Test
  public void testLog() {
    assertEquals(new SCMBigComplex(new BigDecimal("0.3465735902799727"), new BigDecimal("0.7853981633974483")),  eval("(log  1+1i)", env));
    assertEquals(new SCMBigComplex(new BigDecimal("1.6094379124341003"), new BigDecimal("-2.498091544796509")),  eval("(log -4-3i)", env));
    assertEquals(new SCMBigComplex(new BigDecimal("1.6094379124341003"), new BigDecimal("-0.6435011087932844")), eval("(log  4-3i)", env));
    assertEquals(new SCMBigComplex(new BigDecimal("1.6094379124341003"), new BigDecimal("0.6435011087932844")),  eval("(log  4+3i)", env));
    assertEquals(new SCMBigComplex(new BigDecimal("1.6094379124341003"), new BigDecimal("2.498091544796509")),   eval("(log -4+3i)", env));
  }

  @Test
  public void testExpt() {
    assertEquals(new SCMBigComplex(new BigDecimal("0.2739572538301211"), new BigDecimal("0.5837007587586146")), eval("(expt 1+1i 1+1i)", env));
    assertEquals(new SCMBigComplex(new BigDecimal("0.0004911725350693"), new BigDecimal("-0.0007294124825311")), eval("(expt 7+2.3i -4-3i)", env));
  }
}
