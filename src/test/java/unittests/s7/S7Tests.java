package unittests.s7;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import unittests.s7.tests.*;

@RunWith(Suite.class)
@Suite.SuiteClasses({
    EqTest.class,
    EqvTest.class,
    EqualTest.class,
    NotTest.class,
    IsSymbolTest.class,
    IsProcedureTest.class,
    IsCharTest.class,
    MemqTest.class,
    MemvTest.class,
    MemberTest.class,
    QuasiquoteTest.class
})
public class S7Tests {
}
