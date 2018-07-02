package de.thoughtworks.cobol.cobcserver.interfaces;

public class RestRequest {
    private String cobolCode;

    public RestRequest(String cobolCode) {
        this.cobolCode = cobolCode;
    }

    public RestRequest() {
    }

    public String getCobolCode() {
        return cobolCode;
    }

    public void setCobolCode(String cobolCode) {
        this.cobolCode = cobolCode;
    }
}
