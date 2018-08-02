package de.thoughtworks.cobol.cobcserver.interfaces;

public class ExecuteCobolRequest {
    private String cobolCode;

    public ExecuteCobolRequest(String cobolCode) {
        this.cobolCode = cobolCode;
    }

    public ExecuteCobolRequest() {
    }

    public String getCobolCode() {
        return cobolCode;
    }

    public void setCobolCode(String cobolCode) {
        this.cobolCode = cobolCode;
    }
}
