       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. SWIPEI.                                              00020000
       DATA DIVISION.                                                   00030000
       WORKING-STORAGE SECTION.                                         00040000
           COPY OZA128R.                                                00050022
           COPY DFHBMSCA.                                               00060000
           COPY DFHAID.                                                 00070000
       01 WS-BUFF.                                                      00071001
          10 WS-KEY PIC X(06).                                          00072001
          10 SP1 PIC X(01) VALUE SPACES.                                00072115
          10 WS-NAME PIC X(20).                                         00073012
          10 SP2 PIC X(01) VALUE SPACES.                                00073115
          10 WS-DATE PIC X(11).                                         00074012
          10 SP3 PIC X(01) VALUE SPACES.                                00075015
          10 WS-TIME PIC X(09).                                         00076012
       01 WS-COMMAREA.                                                  00080000
          10 WS-TRANSID     PIC X(04).                                  00090000
       01 WS-RESP PIC S9(08) COMP.                                      00091000
       77 WS-ABSDT PIC S9(15) COMP.                                     00093012
       77 WS-FDATE PIC X(10).                                           00094019
       77 WS-FTIME PIC X(08).                                           00095019
       LINKAGE SECTION.                                                 00100000
       01 DFHCOMMAREA.                                                  00110000
          10 LS-TRANSID     PIC X(04).                                  00120000
       PROCEDURE DIVISION.                                              00130000
       MAIN-PARA.                                                       00140000
             EXEC CICS ASKTIME                                          00140112
                  ABSTIME(WS-ABSDT)                                     00140212
             END-EXEC                                                   00140312
             EXEC CICS FORMATTIME                                       00140412
                  ABSTIME(WS-ABSDT)                                     00140512
                  TIME(WS-FTIME)                                        00140613
                  TIMESEP(':')                                          00140712
                  DDMMYYYY(WS-FDATE)                                    00140813
                  DATESEP('/')                                          00140912
             END-EXEC                                                   00141012
            MOVE LOW-VALUES TO  SWINI SWINO.                            00141100
            MOVE 'SWIPE IN' TO HEAD1I.                                  00142010
            PERFORM 1000-RECEIVE-PARA                                   00150000
            THRU    1000-RECEIVE-END.                                   00160000
       1000-RECEIVE-PARA.                                               00170000
            EXEC CICS SEND                                              00180000
                 MAP('SWIN')                                            00190000
                 MAPSET('OZA128R')                                      00200022
                 ERASE                                                  00201004
            END-EXEC                                                    00210019
            EXEC CICS RECEIVE                                           00210103
                 MAP('SWIN')                                            00210203
                 MAPSET('OZA128R')                                      00210322
            END-EXEC                                                    00210403
           MOVE LOW-VALUES TO WS-BUFF.                                  00211012
           MOVE EIDI    TO WS-KEY.                                      00220003
           EXEC CICS READ                                               00233020
                DATASET('OZA128F')                                      00234020
                INTO(WS-BUFF)                                           00235020
                RIDFLD(WS-KEY)                                          00236020
                RESP(WS-RESP)                                           00236121
                UPDATE                                                  00237020
                LENGTH(LENGTH OF WS-BUFF)                               00238020
           END-EXEC                                                     00239020
           MOVE NAMEI   TO WS-NAME.                                     00239121
           MOVE WS-FDATE TO WS-DATE.                                    00239221
           MOVE WS-FTIME TO WS-TIME.                                    00239321
           EXEC CICS REWRITE                                            00240020
                DATASET('OZA128F')                                      00250000
                FROM(WS-BUFF)                                           00260000
                RESP(WS-RESP)                                           00290000
           END-EXEC.                                                    00300000
           IF WS-RESP = DFHRESP(NORMAL) THEN                            00310000
              MOVE 'EMPLOYEE DATA ADDED!' TO MSG2I                      00320000
              MOVE LOW-VALUES TO LGONI LGONO                            00320107
              MOVE 'SWIPED IN SUCCESSFUL' TO HEAD2I                     00320211
              MOVE WS-KEY TO EMPIDO                                     00324113
              MOVE WS-NAME TO EMPNAMEO                                  00324213
              MOVE WS-DATE TO DATE1O                                    00325413
              MOVE WS-TIME TO TIME1O                                    00325513
               EXEC CICS SEND                                           00325606
                    MAP('LGON')                                         00325706
                    MAPSET('OZA128R')                                   00325822
                    ERASE                                               00325908
               END-EXEC                                                 00326006
               EXEC CICS RECEIVE                                        00326116
                    MAP('LGON')                                         00326216
                    MAPSSET('OZA128R')                                  00326322
               END-EXEC                                                 00326416
               IF RETURNI = 'Y'                                         00326518
                  EXEC CICS LINK                                        00326616
                       PROGRAM('OZA128T')                               00326722
                  END-EXEC                                              00326816
              EXEC CICS RETURN END-EXEC                                 00327000
               END-IF                                                   00328017
           ELSE                                                         00330000
             EXEC CICS LINK                                             00350024
                  PROGRAM('OZA128T5')                                   00351024
             END-EXEC                                                   00353024
           END-IF.                                                      00360000
       1000-RECEIVE-END.                                                00370000
           EXIT.                                                        00380000
       9999-END-PARA.                                                   00390000
           EXEC CICS RETURN END-EXEC.                                   00400003
       9999-END.                                                        00410000
           EXIT.                                                        00420000
