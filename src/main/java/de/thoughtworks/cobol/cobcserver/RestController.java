package de.thoughtworks.cobol.cobcserver;

import de.thoughtworks.cobol.cobcserver.interfaces.RestRequest;
import de.thoughtworks.cobol.cobcserver.interfaces.RestResult;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.concurrent.Executors;
import java.util.function.Consumer;

@Controller
public class RestController {

    @RequestMapping(value = "/testCobol", method = RequestMethod.POST)
    public ResponseEntity<RestResult> compileAndExecuteCobol(
            @RequestBody RestRequest testsuite) throws IOException {

        Files.deleteIfExists(Paths.get("scripts/testdriver.cbl"));
        Files.deleteIfExists(Paths.get("scripts/test"));

        // Write tmp file
        Files.write(Paths.get("scripts/testdriver.cbl"), testsuite.getCobolCode().getBytes());

        // compile and execute driver
        RestResult result = callShell("sh build.sh");

        // echo shell output
        System.out.println(result.getSystemoutput());
        System.err.println(result.getSystemerror());

        return ResponseEntity.ok().body(result);
    }

    private RestResult callShell(String command) {
        RestResult result = new RestResult();
        Process process;
        try {
            process = Runtime.getRuntime()
                    .exec(command);

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
        StreamGobbler streamGobbler;

        StringLineBuilder commandLineError = new StringLineBuilder();
        streamGobbler =
                new StreamGobbler(process.getErrorStream(), commandLineError::append);
        Executors.newSingleThreadExecutor().submit(streamGobbler);
        streamGobbler =
                new StreamGobbler(process.getErrorStream(), System.err::println);
        Executors.newSingleThreadExecutor().submit(streamGobbler);
        return commandLineError;
    }

    private StringLineBuilder registerCLReader(Process process) {
        StringLineBuilder commandLineOutput = new StringLineBuilder();
        StreamGobbler streamGobbler =
                new StreamGobbler(process.getInputStream(), commandLineOutput::append);
        Executors.newSingleThreadExecutor().submit(streamGobbler);
        return commandLineOutput;
    }

    private static class StreamGobbler implements Runnable {
        private InputStream inputStream;
        private Consumer<String> consumer;

        StreamGobbler(InputStream inputStream, Consumer<String> consumer) {
            this.inputStream = inputStream;
            this.consumer = consumer;
        }

        @Override
        public void run() {
            new BufferedReader(new InputStreamReader(inputStream)).lines()
                    .forEach(consumer);
        }
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
