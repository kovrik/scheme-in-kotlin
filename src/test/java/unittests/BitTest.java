package unittests;

import org.junit.Test;

import static java.lang.Boolean.FALSE;
import static java.lang.Boolean.TRUE;
import static org.junit.Assert.assertEquals;

public class BitTest extends AbstractTest {

  @Test
  public void testBitAnd() {
    assertEquals(8L, eval("(bit-and #b1100 #b1001)", env));
    assertEquals(8L, eval("(bit-and 12 9)", env));
    assertEquals("108", eval("(Integer/toHexString (bit-and #x0108 #xffff))", env));
    assertEquals(195L, eval("(bit-and 235 199)", env));
  }

  @Test
  public void testBitAndNot() {
    assertEquals(4L, eval("(bit-and-not #b1100 #b1001)", env));
    assertEquals("100", eval("(Integer/toBinaryString (bit-and-not #b1100 #b1010))", env));
  }

  @Test
  public void testBitClear() {
    assertEquals(3L, eval("(bit-clear #b1011 3)", env));
    assertEquals(3L, eval("(bit-clear 11 3)", env));
    assertEquals(2147479551L, eval("(bit-clear Integer/MAX_VALUE 12)", env));
  }

  @Test
  public void testBitFlip() {
    assertEquals(15L, eval("(bit-flip #b1011 2)", env));
    assertEquals(11L, eval("(bit-flip #b1111 2)", env));
    assertEquals(2147450879L, eval("(bit-flip Integer/MAX_VALUE 15)", env));
  }

  @Test
  public void testBitNot() {
    assertEquals(-8L, eval("(bit-not #b0111)", env));
    assertEquals(7L, eval("(bit-not #b-1000)", env));
  }

  @Test
  public void testBitOr() {
    assertEquals(13L, eval("(bit-or #b1100 #b1001)", env));
    assertEquals(13L, eval("(bit-or 12 9)", env));
    assertEquals("1110", eval("(Integer/toBinaryString (bit-or #b1100 #b1010))", env));
  }

  @Test
  public void testBitSet() {
    assertEquals(15L, eval("(bit-set #b1011 2)", env));
    assertEquals(15L, eval("(bit-set 11 2)", env));
    assertEquals(-9223372036854775808L, eval("(bit-set 0 63)", env));
  }

  @Test
  public void testBitShiftLeft() {
    assertEquals(1024L, eval("(bit-shift-left 1 10)", env));
    assertEquals(52L, eval("(bit-shift-left #b1101 2)", env));
  }

  @Test
  public void testBitShiftRight() {
    assertEquals(13L, eval("(bit-shift-right #b1101 0)", env));
    assertEquals(6L,  eval("(bit-shift-right #b1101 1)", env));
    assertEquals(3L,  eval("(bit-shift-right #b1101 2)", env));
    assertEquals(1L,  eval("(bit-shift-right #b1101 3)", env));
    assertEquals(0L,  eval("(bit-shift-right #b1101 4)", env));
  }

  @Test
  public void testBitTest() {
    assertEquals(TRUE,  eval("(bit-test #b1001 0)", env));
    assertEquals(FALSE, eval("(bit-test #b1001 1)", env));
    assertEquals(FALSE, eval("(bit-test #b1001 2)", env));
    assertEquals(TRUE,  eval("(bit-test #b1001 3)", env));
    assertEquals(FALSE, eval("(bit-test #b1001 7)", env));
  }

  @Test
  public void testBitXor() {
    assertEquals(5L, eval("(bit-xor #b1100 #b1001)", env));
    assertEquals("110", eval("(Integer/toBinaryString (bit-xor #b1100 #b1010))", env));
  }
}
