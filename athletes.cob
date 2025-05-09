       IDENTIFICATION DIVISION.
       PROGRAM-ID. athletes.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

              select fa assign to "athletes.dat"
              organization indexed
              access mode is dynamic
              record key is fa_numA
              alternate record key is fa_classementP
              alternate record key is fa_pays WITH DUPLICATES
              file status is cr_fa.

       DATA DIVISION.
       FILE SECTION.

       FD fa.
              01 tamp_fa.
                     02 fa_numA PIC 9(3).
                     02 fa_nom PIC X(30).
                     02 fa_prenom PIC X(30).
                     02 fa_pays PIC X(30).
                     02 fa_annee PIC 9(4).
                     02 fa_classementP PIC p(3).
       WORKING-STORAGE SECTION.
              77 cr_fa PIC 9(2).
              77 Wfin PIC 9.
              77 WnumA PIC 9(3).
              77 Wnom PIC X(30).
              77 Wprenom PIC X(30).
              77 Wpays PIC X(30).
              77 Wannee PIC 9(4).
              77 WclassementP PIC p(3).
              77 Wtrouver PIC 9.
              77 Wpoints PIC 9(4).
              77 Winitial PIC X(15).
              77 Wsprint PIC X(15).
              77 Wpoursuite PIC X(15).
              77 Wmassstart PIC X(15).


       PROCEDURE DIVISION.
       OPEN I-O fa
       IF cr_fa=35 THEN
       OPEN OUTPUT fa
       END-IF
       CLOSE fa
       PERFORM Affichage_athletes
       STOP RUN.

       Ajout_athletes.
       OPEN INPUT fa
       MOVE 0 TO Wtrouver
       PERFORM WITH TEST AFTER UNTIL Wtrouver = 0
              DISPLAY "Rentrer numero Athletes"
              ACCEPT WnumA
              MOVE WnumA TO fa_numA
              READ fa
              INVALID KEY DISPLAY "inexistant"
                          MOVE 0 To Wtrouver
              NOT INVALID KEY DISPLAY fa_numA
                              MOVE 1 To Wtrouver
              END-READ
       END-PERFORM
       CLOSE fa
       OPEN INPUT fa
       MOVE 0 TO Wtrouver
       PERFORM WITH TEST AFTER UNTIL Wtrouver = 0
              DISPLAY "Rentrer Classement"
              ACCEPT WclassementP
              MOVE WclassementP TO fa_classementP
              READ fa
              INVALID KEY DISPLAY "inexistant"
                          MOVE 0 To Wtrouver
              NOT INVALID KEY DISPLAY fa_classementP
                              MOVE 1 To Wtrouver
              END-READ
       END-PERFORM
       CLOSE fa
       DISPLAY "rentrer nom"
       ACCEPT Wnom
       DISPLAY "rentrer prenom"
       ACCEPT Wprenom
       DISPLAY "rentrer pays"
       ACCEPT Wpays
       PERFORM WITH TEST AFTER UNTIL Wtrouver = 0
              DISPLAY "rentrer annee"
              ACCEPT Wannee
              IF Wannee>0 AND Wannee<2223
                     MOVE 0 TO Wtrouver
              END-IF
       END-PERFORM
       MOVE Wnom TO fa_nom
       MOVE Wprenom TO fa_prenom
       MOVE Wpays TO fa_pays
       MOVE Wannee TO fa_annee
       OPEN EXTEND fa
       WRITE tamp_fa
       END-WRITE
       CLOSE fa.

       Affichage_athletes.
       OPEN INPUT fa
       MOVE 0 TO Wfin
       PERFORM WITH TEST AFTER UNTIL Wfin = 1
              READ fa NEXT
              AT END MOVE 1 TO Wfin
              NOT AT END
                     DISPLAY "numéro:"fa_numA
                     DISPLAY "nom :"fa_nom
                     DISPLAY "prenom :"fa_prenom
                     DISPLAY "pays :" fa_pays
                     DISPLAY "annee :" fa_annee
                     DISPLAY "classement :" fa_classementP
              END-READ
       END-PERFORM
       CLOSE fa.

       recherche_athlete.
       OPEN INPUT fa
       DISPLAY "rentrer nom"
       ACCEPT Wnom
       DISPLAY "rentrer prenom"
       ACCEPT Wprenom
       MOVE 0 TO Wfin
       MOVE 0 TO Wtrouver
       PERFORM WITH TEST AFTER UNTIL Wfin = 1
       OR Wtrouver = 1
              READ fa NEXT
              AT END MOVE 1 TO Wfin
                     IF Wtrouver = 0
                            DISPLAY "inexistant"
                     END-IF
              NOT AT END IF Wnom=fa_nom AND Wprenom=fa_prenom
                     THEN MOVE 1 TO Wtrouver
                     DISPLAY  fa_numA
              END-READ
       END-PERFORM
       CLOSE fa.

       recherche_pays.
       OPEN INPUT fa
       DISPLAY "rentrer pays"
       ACCEPT Wpays
       MOVE 0 TO Wfin
       MOVE 0 TO Wtrouver
       MOVE Wpays TO fa_pays
       START fa, KEY IS = fa_pays
       INVALID KEY DISPLAY "inexistant"
       NOT INVALID KEY
              PERFORM WITH TEST AFTER UNTIL Wfin = 1
                     READ fa NEXT
                     AT END MOVE 1 TO Wfin
                     NOT AT END
                            DISPLAY "numéro:"fa_numA
                            DISPLAY "nom :"fa_nom
                            DISPLAY "prenom :"fa_prenom
                            DISPLAY "pays :" fa_pays
                            DISPLAY "annee :" fa_annee
                            DISPLAY "classement :" fa_classementP
                     END-READ
              END-PERFORM
       END-START
       CLOSE fa.
