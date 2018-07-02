IDENTIFICATION DIVISION.
PRO. EXAMPLE-TEST-CASE.
DATA DIVISION.
FILE SECTION.
WORKING-STORAGE SECTION.
01 C2222D.
  02 C2222D-ITEMS OCCURS 10.
    03 C2222D-NAME PIC X(50).
    03 C2222D-QUALITY PIC 999.
    03 C2222D-SELLIN PIC S999.
01 ZW-I PIC 99.
  01 VSP.
    02 VSP-RETURN-CODE      PIC 99 VALUE 0.

PROCEDURE DIVISION.
    MAIN-PROCEDURE.
    DISPLAY "TESTING".

FIRST-TEST SECTION.

    MOVE 1 TO ZW-I.
    MOVE "YOUNG BRIE" TO C2222D-NAME(ZW-I).
    MOVE 20 TO C2222D-SELLIN(ZW-I).
    MOVE 0 TO C2222D-QUALITY(ZW-I).

    PERFORM SOME-OTHER-PART.

    DISPLAY C2222D-ITEMS(ZW-I).
CONTINUE.
EXIT.


    STOP RUN.

SOME-OTHER-PART SECTION.
           IF C2222D-NAME(ZW-I) NOT EQUAL TO SPACES THEN
               IF C2222D-NAME(ZW-I) NOT EQUAL TO "Aged Brie" AND
                  C2222D-NAME(ZW-I) NOT EQUAL TO "Backstage" THEN
                   IF C2222D-QUALITY(ZW-I) GREATER THAN 0 THEN
                       IF C2222D-NAME(ZW-I) NOT EQUAL TO "Sulfuras" THEN
                           SUBTRACT 1 FROM C2222D-QUALITY(ZW-I)
                       END-IF
                   END-IF
               ELSE
                   IF C2222D-QUALITY(ZW-I) LESS THAN 50 THEN
                       ADD 1 TO C2222D-QUALITY(ZW-I)
                       IF C2222D-NAME(ZW-I) = "Backstage" THEN
                           IF C2222D-SELLIN(ZW-I) LESS THAN 11 THEN
                               IF C2222D-QUALITY(ZW-I) LESS THAN 50 THEN
                                   ADD 1 TO C2222D-QUALITY(ZW-I)
                               END-IF
                           END-IF
       
                           IF C2222D-SELLIN(ZW-I) LESS THAN 6 THEN
                               IF C2222D-QUALITY(ZW-I) LESS THAN 50 THEN
                                   ADD 1 TO C2222D-QUALITY(ZW-I)
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF.
       EXIT.
UPDATE-QUALITY SECTION.
    DISPLAY "MOCK WAS CALLED".
CONTINUE.EXIT.
END PROGRAM EXAMPLE-TEST-CASE.
