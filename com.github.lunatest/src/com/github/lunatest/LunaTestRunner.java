package com.github.lunatest;

import java.io.IOException;
import java.util.LinkedList;
import java.util.Queue;

import org.eclipse.core.runtime.Path;
import org.junit.runner.Description;
import org.junit.runner.Runner;
import org.junit.runner.notification.Failure;
import org.junit.runner.notification.RunNotifier;

import com.naef.jnlua.JavaFunction;
import com.naef.jnlua.LuaState;

/**
 * Runner used to pass LunaTest tests using JUnit 4. This class must be used with the RunWith annotation and the test
 * class must have a method <code>public static void suite(LunaTestRunner runner)</code> where <code>runner</code> is
 * the runner instance which will run the tests. For example:
 * 
 * <pre>
 * &#064;RunWith(LunaTestRunner.class)
 * public class MyTest {
 *     public static void suite(LunaTestRunner runner) {
 *         runner.addTestSuite(&quot;/some/test.lua&quot;, &quot;some.test&quot;);
 *     }
 * }
 * </pre>
 * 
 * There is some limitations to tests:
 * <ul>
 * <li>All tests are run just after calling <code>suite</code> and not inside run method</li>
 * <li>Tests time are not relevant</li>
 * </ul>
 * 
 * @author jdesgats
 */
@SuppressWarnings("nls")
public class LunaTestRunner extends Runner {
    private static class LuaTestResult {
        public Description desc;
        public String status; // pass, fail, error, skip or start (when suite is started)
        public String message = null;
        @SuppressWarnings("unused")
        // FIXME: find a way to give the test duration to runner
        public long milliseconds = 0;
    }

    private static final String LUNATEST_INDEX = "LuaRunner_lunatest";
    private Class<?> klass;
    private Description description;
    private Description currentSuite;
    private LuaState state;
    private Queue<LuaTestResult> events;

    private JavaFunction suiteStarted = new JavaFunction() {
        @Override
        public int invoke(LuaState luaState) {
            LuaTestResult event = new LuaTestResult();
            event.status = "start";
            luaState.getField(1, "name");
            String name = luaState.toString(-1);
            event.desc = Description.createSuiteDescription(name);
            description.addChild(event.desc);
            currentSuite = event.desc;
            events.add(event);
            return 0;
        }
    };

    private JavaFunction testFinished = new JavaFunction() {
        @Override
        public int invoke(LuaState luaState) {
            String name = luaState.toString(1);
            LuaTestResult event = new LuaTestResult();

            event.desc = Description.createTestDescription(klass, name);
            currentSuite.addChild(event.desc);

            // get test status
            luaState.getField(2, "type");
            luaState.pushValue(2);
            luaState.call(1, 1);
            event.status = luaState.toString(-1);

            // status message
            luaState.getField(2, "msg");
            event.message = luaState.toString(-1);

            // elapsed time
            luaState.getField(2, "elapsed");
            event.milliseconds = (long) luaState.toNumber(-1);
            events.add(event);
            return 0;
        }
    };

    private JavaFunction suiteFinished = new JavaFunction() {
        @Override
        public int invoke(LuaState luaState) {
            LuaTestResult event = new LuaTestResult();
            event.desc = currentSuite;
            // suite status is handled by the test runner
            event.status = "pass";
            currentSuite = null;
            events.add(event);
            return 0;
        }
    };

    public LunaTestRunner(Class<?> klass) throws IOException {
        super();
        this.klass = klass;
        this.description = Description.createSuiteDescription(klass);
        this.events = new LinkedList<LunaTestRunner.LuaTestResult>();

        // create and setup a Lua state for test case
        state = new LuaState();
        state.openLibs();

        // os.exit can be called by lunatest: make it a dummy
        state.getGlobal("os");
        state.pushJavaFunction(new JavaFunction() {
            @Override
            public int invoke(LuaState luaState) {
                return 0;
            }
        });
        state.setField(-2, "exit");
        state.pop(1);

        // add LunaTest in path
        Path path = LunaTest.getPath();
        String code = "package.path = package.path  .. [[;" + path + "?.luac;" + path + "?.lua]]";
        state.load(code, "lunatestPathLoading");
        state.call(0, 0);

        // load LunaTest in registry
        getState().getGlobal("require");
        getState().pushString("lunatest");
        getState().call(1, 1);
        getState().setField(LuaState.REGISTRYINDEX, LUNATEST_INDEX);

        // tests are actually run now
        runLuaTests();
    }

    @Override
    public Description getDescription() {
        return description;
    }

    /**
     * Runs the Lua tests: builds test description and put results on events queue. This must be ran before by
     * constructor because there is no "description" phase on LunaTest so the description is known only at run time.
     */
    private void runLuaTests() {
        try {
            klass.getDeclaredMethod("suite", LunaTestRunner.class).invoke(null, this);
        } catch (Exception e) {
            e.printStackTrace();
            return;
        }

        getState().getField(LuaState.REGISTRYINDEX, LUNATEST_INDEX);
        // this hack is not done to protect call but just to make an additional stack level, see lunatest.lua:724
        getState().getGlobal("pcall");
        getState().getField(-2, "run");

        // setup hooks
        getState().newTable();
        getState().pushJavaFunction(suiteStarted);
        getState().setField(-2, "begin_suite");
        getState().pushJavaFunction(testFinished);
        getState().setField(-2, "post_test");
        getState().pushJavaFunction(suiteFinished);
        getState().setField(-2, "end_suite");

        getState().call(2, 0);
    }

    @Override
    public void run(RunNotifier notifier) {
        LuaTestResult result;
        while ((result = events.poll()) != null) {
            notifier.fireTestStarted(result.desc);
            if (result.status.equals("start")) {
                continue;
            } else if (result.status.equals("fail")) {
                notifier.fireTestFailure(new Failure(result.desc, new AssertionError(result.message)));
            } else if (result.status.equals("error")) {
                notifier.fireTestFailure(new Failure(result.desc, new Exception(result.message)));
            } else if (result.status.equals("skip")) {
                notifier.fireTestIgnored(result.desc);
            }
            notifier.fireTestFinished(result.desc);
        }
    }

    /**
     * @return The LuaState used to run the tests. You can call this method to setup test environment.
     */
    public LuaState getState() {
        return state;
    }

    /**
     * Utility function to make given file name a loadable module (by require). Can be used to make script under test
     * available to test script itself without hacking paths.
     * 
     * @param filename File name to load.
     * @param modname Module name (the argument for require)
     */
    public void preloadFile(String filename, String modname) {
        getState().getGlobal("package");
        getState().getField(-1, "preload");
        // raw loadfile is used instead of JNLua's load to have filename on debug functions
        getState().getGlobal("loadfile");
        getState().pushString(filename);
        getState().call(1, 1);
        getState().setField(-2, modname);
        getState().pop(2);
    }

    /**
     * Adds a test suite to run suite. The suite will be ran by LunaTest runner.
     * 
     * @param suiteFile File which contains tests source
     * @param suiteName Test suite name
     */
    public void addTestSuite(String suiteFile, String suiteName) {
        preloadFile(suiteFile, suiteName);
        getState().getField(LuaState.REGISTRYINDEX, LUNATEST_INDEX);
        getState().getField(-1, "suite");
        getState().pushString(suiteName);
        getState().call(1, 0);
        getState().pop(1);
    }
}
