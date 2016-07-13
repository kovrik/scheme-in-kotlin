package s7;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import s7.tests.EqTest;
import s7.tests.EqualTest;
import s7.tests.EqvTest;
import s7.tests.NotTest;

@RunWith(Suite.class)
@Suite.SuiteClasses({
    EqTest.class,
    EqvTest.class,
    EqualTest.class,
    NotTest.class
})
public class S7Tests {
}
