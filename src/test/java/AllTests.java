import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import s7.S7Tests;

@RunWith(Suite.class)
@Suite.SuiteClasses({
    EvaluatorTest.class,
    ReaderTest.class,
    WriterTest.class,
    SCMConsTest.class,
    /* S7 Test Suite: https://ccrma.stanford.edu/software/snd/snd/s7.html */
    S7Tests.class,
    ConsTest.class
})
public class AllTests {
}
