package de.thoughtworks.cobol.cobcserver;


import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class ExecuteCobolControllerTest {

    private static final String CODE = "IDENTIFICATION DIVISION.\n" +
            "PROGRAM-ID. PGAA0000.\n" +
            "DATA DIVISION.\n" +
            "WORKING-STORAGE SECTION.\n" +
            "LINKAGE SECTION.\n" +
            "PROCEDURE DIVISION.\n" +
            "SOME SECTION.\n" +
            "DISPLAY 'bla'.\n" +
            "STOP RUN.\n";

    private static final String CODE_WITH_ERROR = "IDENTIFICATION DIVISION.\n" +
            "PROGRAM-ID. PGAA0000.\n" +
            "DATA DIVISION.\n" +
            "WORKING-STORAGE SECTION.\n" +
            "LINKAGE SECTION.\n" +
            "PROCEDURE DIVISION.\n" +
            "SOME SECTION.\n" +
            "DISPLAY SOMETHING-UNKNOWN.\n" +
            "STOP RUN.\n";

    private ByteArrayOutputStream outStream = new ByteArrayOutputStream();
    private ByteArrayOutputStream errStream = new ByteArrayOutputStream();

    private PrintStream oldOutStream, oldErrStream;

    @Before
    public void setUp() throws Exception {
        oldOutStream = System.out;
        oldErrStream = System.err;
        System.setOut(new PrintStream(outStream));
        System.setErr(new PrintStream(errStream));
    }

    @After
    public void tearDown() throws Exception {
        System.setOut(oldOutStream);
        System.setErr(oldErrStream);
    }

    @Test
    public void shouldRunCobolCorrectly() throws Exception {
        ExecuteCobolController controller = new ExecuteCobolController();
        ExecuteCobolRequest request = new ExecuteCobolRequest(CODE);

        ResponseEntity<ExecutionResult> result = controller.compileAndExecuteCobol(request);

        assertThat(result.getStatusCode(), is(HttpStatus.OK));
        assertThat(result.getBody().getSystemoutput(), is("bla\n"));
        assertThat(outStream.toString(), is("bla\n\n"));
    }
    @Test
    public void shouldRetrieveErrors() throws Exception {
        ExecuteCobolController controller = new ExecuteCobolController();
        ExecuteCobolRequest request = new ExecuteCobolRequest(CODE_WITH_ERROR);

        ResponseEntity<ExecutionResult> result = controller.compileAndExecuteCobol(request);

        assertThat(result.getStatusCode(), is(HttpStatus.OK));
        assertThat(result.getBody().getSystemoutput(), is(""));
        assertThat(result.getBody().getSystemerror(), containsString("'SOMETHING-UNKNOWN' is not defined"));
        assertThat(errStream.toString(), containsString("'SOMETHING-UNKNOWN' is not defined"));
    }
}