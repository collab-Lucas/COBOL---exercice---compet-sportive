IDENTIFICATION DIVISION.
PROGRAM-ID. participants.
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.

       select fp assign to "participants.dat"
       organization indexed
       access mode is dynamic
       record key is fp_cle
       alternate record key is fp_numA WITH DUPLICATES
       alternate record key is fp_numCo WITH DUPLICATES
       file status is cr_fp.
       
DATA DIVISION.
FILE SECTION.

FD fp.
       01 tamp_fp.
              02 fp_cle.
                     03 fp_numCo PIC 9(2).
                     03 fp_numA PIC 9(3).
              02 fp_classement PIC 9(3).
              02 fp_temps PIC 9(4).
              02 fp_penalties PIC 9(2).
              02 fp_points PIC 9(2).
              
WORKING-STORAGE SECTION.
       77 cr_fp PIC 9(2).
       77 Wfin PIC 9.
       77 WnumCo PIC 9(2).
       77 WnumA PIC 9(3).
       77 Wclassement PIC 9(3).
       77 Wtemps PIC 9(4).
       77 Wpenalties PIC 9(2).
       77 Wpoints PIC 9(2).
       77 Wtrouver PIC 9.

PROCEDURE DIVISION.
OPEN I-O fp
IF cr_fp=35 THEN
OPEN OUTPUT fp
END-IF
CLOSE fp
STOP RUN.

STOP RUN.

       Ajout_participant.
       OPEN INPUT fp
       OPEN INPUT fc
       PERFORM WITH TEST AFTER UNTIL Wtrouver = 0 
              DISPLAY "Rentrer numero Course"
              ACCEPT WnumCo
              MOVE WnumCo TO fc_numCo
              READ fc
              INVALID KEY DISPLAY "inexistant"
                          MOVE 0 To Wtrouver
              NOT INVALID KEY DISPLAY fc_numCo
                              MOVE 1 To Wtrouver
              END-READ
       END-PERFORM
       CLOSE fc
       DISPLAY "rentrer numéro de l'atlhète" 
       ACCEPT WnumA  
       MOVE WnumCo TO fp_numCo
       MOVE WnumA TO fp_numA
       MOVE 0 TO fp_classement
       MOVE 0 TO fp_temps
       MOVE 0 TO fp_penalties
       MOVE 0 TO fp_points
       OPEN EXTEND fp
       WRITE tamp_fp
       END-WRITE
       CLOSE fp.
       
       modifier_resultat.
       OPEN INPUT fc
       PERFORM WITH TEST AFTER UNTIL Wtrouver = 0 
              DISPLAY "Rentrer numero Course"
              ACCEPT WnumCo
              MOVE WnumCo TO fc_numCo
              READ fc
              INVALID KEY DISPLAY "inexistant"
                          MOVE 0 To Wtrouver
              NOT INVALID KEY DISPLAY fc_numCo
                              MOVE 1 To Wtrouver
              END-READ
       END-PERFORM
       CLOSE fc
       OPEN I-O fp
       MOVE 0 TO Wfin
       START fp, KEY IS = fp_numCo 
       INVALID KEY DISPLAY "inexistant"
       NOT INVALID KEY
              PERFORM WITH TEST AFTER UNTIL Wfin = 1
                     READ fp NEXT
                     AT END MOVE 1 TO Wfin 
                     NOT AT END
                            DISPLAY "rentrer numéro de l'atlhète" 
                            ACCEPT Wclassement 
                            DISPLAY "Participant:"fp_numA
                            DISPLAY "classement :"fp_classement
                            ACCEPT Wclassement 
                            DISPLAY "temps :"fc_nbpassage
                            DISPLAY "pénaliter :" fc_nbkms
                            DISPLAY "points :" fc_nbkms
                            MOVE Wclassement TO fp_classement
                            MOVE Wtemps TO fp_temps
                            MOVE Wpenalties TO fp_penalties
                            MOVE Wpoints TO fp_points
                            WRITE tamp_fp
                            END-WRITE
                     END-READ
              END-PERFORM
       END-START
       CLOSE fp.











