import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses({
    EvaluatorTest.class,
    ReaderTest.class,
    SCMConsTest.class,
    ConsTest.class
})
public class AllTests {
}
