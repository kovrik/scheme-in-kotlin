package unittests;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import unittests.s7.S7Tests;

@RunWith(Suite.class)
@Suite.SuiteClasses({
    ReaderTest.class,
    WriterTest.class,
    EvaluatorTest.class,
    TCOTest.class,
    SpecialFormTest.class,
    DelayedTest.class,
    NumberTest.class,
    ComplexTest.class,
    TrigonometryTest.class,
    SCMRationalTest.class,
    CharacterTest.class,
    StringTest.class,
    ListTest.class,
    SCMConsTest.class,
    VectorTest.class,
    IOTest.class,
    RosettaCodeTest.class,
    /* S7 Test Suite: https://ccrma.stanford.edu/software/snd/snd/s7.html */
    S7Tests.class
})
public class AllTests {
}
