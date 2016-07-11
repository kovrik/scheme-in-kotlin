package s7;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import s7.tests.EqTest;
import s7.tests.EqvTest;

@RunWith(Suite.class)
@Suite.SuiteClasses({
    EqTest.class,
    EqvTest.class
})
public class S7Tests {
}
