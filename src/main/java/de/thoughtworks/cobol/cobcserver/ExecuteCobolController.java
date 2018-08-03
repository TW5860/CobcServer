package de.thoughtworks.cobol.cobcserver;

import org.apache.commons.io.IOUtils;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

import static java.nio.charset.StandardCharsets.UTF_8;

@Controller
public class ExecuteCobolController {

    @RequestMapping(value = "/testCobol", method = RequestMethod.POST)
    public ResponseEntity<ExecutionResult> compileAndExecuteCobol(
            @RequestBody ExecuteCobolRequest testsuite) throws IOException {
        writeTmpFile(testsuite);
        ExecutionResult result = compileAndExecute();
        printResults(result);
        return ResponseEntity.ok()
                .body(result);
    }

    private void printResults(ExecutionResult result) {
        System.out.println(result.getSystemoutput());
        System.err.println(result.getSystemerror());
    }

    private void writeTmpFile(@RequestBody ExecuteCobolRequest testsuite) throws IOException {
        Files.createDirectories(Paths.get("scripts/"));
        Files.write(Paths.get("scripts/testdriver-" + getId() + ".cbl"), testsuite.getCobolCode().getBytes());
    }

    private ExecutionResult compileAndExecute() {
        ExecutionResult result = new ExecutionResult();
        Process process;
        try {
            process = Runtime.getRuntime()
                    .exec("sh build.sh " + getId());

            // IOUtils.toString waits for the stream to get to an end.
            // Therefore we don't have to explicitly wait for the process to be finished
            String output = IOUtils.toString(process.getInputStream(), UTF_8);
            String error = IOUtils.toString(process.getErrorStream(), UTF_8);

            result.setSystemoutput(output);
            result.setSystemerror(error);
        } catch (Exception e) {
            result.setSystemerror(e.getMessage());
        }
        return result;
    }

    private String getId() {
        return String.valueOf(Thread.currentThread().getId());
    }

}
