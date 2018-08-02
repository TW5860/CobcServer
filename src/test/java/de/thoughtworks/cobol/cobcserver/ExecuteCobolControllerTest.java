package de.thoughtworks.cobol.cobcserver;


import de.thoughtworks.cobol.cobcserver.interfaces.ExecuteCobolRequest;
import de.thoughtworks.cobol.cobcserver.interfaces.ExecutionResult;
import org.junit.Test;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

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

    @Test
    public void shouldRunCobolCorrectly() throws Exception {
        ExecuteCobolController controller = new ExecuteCobolController();
        ExecuteCobolRequest request = new ExecuteCobolRequest(CODE);

        ResponseEntity<ExecutionResult> result = controller.compileAndExecuteCobol(request);

        assertThat(result.getStatusCode(), is(HttpStatus.OK));
        assertThat(result.getBody().getSystemoutput(), is("bla\n"));
    }
}