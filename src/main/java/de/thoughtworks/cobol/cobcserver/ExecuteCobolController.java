package de.thoughtworks.cobol.cobcserver;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.concurrent.Executors;

@Controller
public class ExecuteCobolController {

    @RequestMapping(value = "/testCobol", method = RequestMethod.POST)
    public ResponseEntity<ExecutionResult> compileAndExecuteCobol(
            @RequestBody ExecuteCobolRequest testsuite) throws IOException {
        writeTmpFile(testsuite);
        ExecutionResult result = compileAndExecute();
        printResults(result);
        return ResponseEntity.ok().body(result);
    }

    private void printResults(ExecutionResult result) {
        System.out.println(result.getSystemoutput());
        System.err.println(result.getSystemerror());
    }

    private void writeTmpFile(@RequestBody ExecuteCobolRequest testsuite) throws IOException {
        Files.createDirectories(Paths.get("scripts/"));
        Files.write(Paths.get("scripts/testdriver.cbl"), testsuite.getCobolCode().getBytes());
    }

    private ExecutionResult compileAndExecute() {
        ExecutionResult result = new ExecutionResult();
        Process process;
        try {
            process = Runtime.getRuntime()
                    .exec("sh build.sh");

            StringLineBuilder commandLineOutput = registerCLReader(process);
            StringLineBuilder commandLineError = registerCLErrorReader(process);

            process.waitFor();
            result.setSystemoutput(commandLineOutput.toString());
            result.setSystemerror(commandLineError.toString());
        } catch (Exception e) {
            result.setSystemerror(e.getMessage());
        }
        return result;
    }

    private StringLineBuilder registerCLErrorReader(Process process) {
        StreamToStringConsumerReader streamGobbler;

        StringLineBuilder commandLineError = new StringLineBuilder();
        streamGobbler =
                new StreamToStringConsumerReader(process.getErrorStream(), commandLineError::append);
        Executors.newSingleThreadExecutor().submit(streamGobbler);
        streamGobbler =
                new StreamToStringConsumerReader(process.getErrorStream(), System.err::println);
        Executors.newSingleThreadExecutor().submit(streamGobbler);
        return commandLineError;
    }

    private StringLineBuilder registerCLReader(Process process) {
        StringLineBuilder commandLineOutput = new StringLineBuilder();
        StreamToStringConsumerReader streamGobbler =
                new StreamToStringConsumerReader(process.getInputStream(), commandLineOutput::append);
        Executors.newSingleThreadExecutor().submit(streamGobbler);
        return commandLineOutput;
    }

    static class StringLineBuilder {
        StringBuilder builder = new StringBuilder();

        StringLineBuilder append(String line) {
            builder.append(line);
            builder.append("\n");
            return this;
        }

        public String toString() {
            return builder.toString();
        }
    }

}
