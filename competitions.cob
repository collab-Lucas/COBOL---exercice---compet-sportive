IDENTIFICATION DIVISION.
PROGRAM-ID. competitions.
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.

       select fco assign to "competitions.dat"
       organization sequential
       access mode is sequential
       file status is cr_fco.
       
DATA DIVISION.
FILE SECTION.

FD fco.
       01 tamp_fco.
              02 fco_ville PIC X(30).
              02 fco_pays PIC X(15).
              02 fco_semaine PIC 9(2).
              02 fco_nbj PIC 9(3).
              
WORKING-STORAGE SECTION.
       77 cr_fco PIC 9(2).
       77 Wfin PIC 9.
       77 Wville PIC X(30).
       77 Wpays PIC X(15).
       77 Wsemaine PIC 9(2).
       77 Wnbj PIC 9(3).
       77 Wtrouver PIC 9.
       77 Wvalide PIC 9.
       
PROCEDURE DIVISION.
       OPEN I-O fco
       IF cr_fco=35 THEN
       OPEN OUTPUT fco
       END-IF
       CLOSE fco
       PERFORM Affichage_competition


STOP RUN.
       
       Ajout_competition.
       OPEN INPUT fco
       MOVE 0 TO Wvalide
       PERFORM WITH TEST AFTER UNTIL Wvalide = 1
              MOVE 0 TO Wtrouver
              DISPLAY "rentrer ville" 
              ACCEPT Wville
              PERFORM WITH TEST AFTER UNTIL Wfin = 1 OR Wtrouver = 1
                     READ fco
                     AT END MOVE 1 TO Wfin 
                            IF Wtrouver = 0 
                                   MOVE 1 TO Wvalide  
                                   DISPLAY "inexistant"
                            END-IF
                     NOT AT END IF Wville=fco_ville
                     THEN MOVE 1 TO Wtrouver END-IF 
                     END-READ
              END-PERFORM
       END-PERFORM
       CLOSE fco
       OPEN INPUT fco  
       MOVE 0 TO Wvalide
       MOVE 0 TO Wfin
       MOVE 0 TO Wtrouver
       PERFORM WITH TEST AFTER UNTIL Wvalide = 1
              DISPLAY "rentrer semaine" 
              ACCEPT Wsemaine
              IF Wsemaine >0 AND Wsemaine <53
                     PERFORM WITH TEST AFTER UNTIL Wfin = 1 
                     OR Wtrouver = 1
                            READ fco
                            AT END MOVE 1 TO Wfin 
                                   IF Wtrouver = 0 
                                          MOVE 1 TO Wvalide
                                          DISPLAY "inexistant" 
                                   END-IF
                            NOT AT END IF Wsemaine=fco_semaine
                            THEN MOVE 1 TO Wtrouver END-IF 
                            END-READ
                     END-PERFORM
               END-IF
       END-PERFORM
       CLOSE fco  
       DISPLAY "rentrer pays" 
       ACCEPT Wpays
       DISPLAY "rentrer nombre de participants" 
       ACCEPT Wnbj      
       MOVE Wville TO fco_ville
       MOVE Wpays TO fco_pays
       MOVE Wnbj TO fco_nbj
       MOVE Wsemaine TO fco_semaine
       OPEN EXTEND fco
       WRITE tamp_fco
       END-WRITE
       CLOSE fco.
       
       Affichage_competition.
       OPEN INPUT fco
       MOVE 0 TO Wfin
       PERFORM WITH TEST AFTER UNTIL Wfin = 1
              READ fco
              AT END MOVE 1 TO Wfin 
              NOT AT END
                     DISPLAY "ville :"fco_ville
                     DISPLAY "pays :"fco_pays
                     DISPLAY "semaine :"fco_semaine
                     DISPLAY "nb participants :" fco_nbj
              END-READ
       END-PERFORM  
       CLOSE fco.
       
       recherche_semaine.
       OPEN INPUT fco
       MOVE 0 TO Wtrouver
       MOVE 0 TO Wvalide
       PERFORM WITH TEST AFTER UNTIL Wvalide = 1
              DISPLAY "rentrer semaine" 
              ACCEPT Wsemaine
              IF Wsemaine >0 AND Wsemaine <53
                     MOVE 1 TO Wvalide
                     PERFORM WITH TEST AFTER UNTIL Wfin = 1 
                     OR Wtrouver = 1
                            READ fco
                            AT END MOVE 1 TO Wfin 
                                   IF Wtrouver = 0 
                                          DISPLAY "inexistant" 
                                   END-IF
                            NOT AT END IF Wsemaine=fco_semaine
                                   THEN MOVE 1 TO Wtrouver
                                   DISPLAY  fco_pays
                                   DISPLAY  fco_ville
                                   END-IF 
                            END-READ
                     END-PERFORM
              END-IF
       END-PERFORM
       CLOSE fco.










