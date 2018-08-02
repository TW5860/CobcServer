package de.thoughtworks.cobol.cobcserver;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.function.Consumer;

class StreamToStringConsumerReader implements Runnable {
    private InputStream inputStream;
    private Consumer<String> consumer;

    StreamToStringConsumerReader(InputStream inputStream, Consumer<String> consumer) {
        this.inputStream = inputStream;
        this.consumer = consumer;
    }

    @Override
    public void run() {
        new BufferedReader(new InputStreamReader(inputStream)).lines()
                .forEach(consumer);
    }
}
