package de.thoughtworks.cobol.cobcserver;

public class ExecutionResult {

    private String systemoutput;
    private String systemerror;

    public String getSystemerror() {
        return systemerror;
    }

    public void setSystemerror(String systemerror) {
        this.systemerror = systemerror;
    }

    public String getSystemoutput() {
        return systemoutput;
    }

    public void setSystemoutput(String systemoutput) {
        this.systemoutput = systemoutput;
    }
}
