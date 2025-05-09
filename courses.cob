IDENTIFICATION DIVISION.
PROGRAM-ID. courses.
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.

       select fc assign to "courses.dat"
       organization indexed
       access mode is dynamic
       record key is fc_numCo
       alternate record key is fc_typeCo WITH DUPLICATES
       alternate record key is fc_villeCompet WITH DUPLICATES
       file status is cr_fc.

DATA DIVISION.
FILE SECTION.

FD fc.
       01 tamp_fc.
              02 fc_numCo PIC 9(3).
              02 fc_villeCompet PIC X(30).
              02 fc_typeCo PIC X(30).
              02 fc_nbpassage PIC 9(1).
              02 fc_nbkms PIC 9(2).

WORKING-STORAGE SECTION.
       77 cr_fc PIC 9(2).
       77 Wfin PIC 9.
       77 WnumCo PIC 9(3).
       77 WvilleCompet PIC X(30).
       77 WtypeCo PIC X(30).
       77 Wnbpassage PIC 9(1).
       77 Wnbkms PIC 9(2).
       77 Wtrouver PIC 9.
       77 Wpays PIC X(30).
       
       
PROCEDURE DIVISION.
OPEN I-O fc
IF cr_fc=35 THEN
OPEN OUTPUT fc
END-IF
CLOSE fc
PERFORM Ajout_course
STOP RUN.

       Ajout_course.
       OPEN INPUT fc
       MOVE 0 TO Wtrouver
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
       DISPLAY "rentrer type"
       ACCEPT WtypeCo
       DISPLAY "rentrer ville"
       ACCEPT WvilleCompet
       DISPLAY "rentrer nbpassage"
       ACCEPT Wnbpassage
       DISPLAY "rentrer nbkms"
       ACCEPT Wnbkms
       MOVE WtypeCo TO fc_typeCo
       MOVE WvilleCompet TO fc_villeCompet
       MOVE Wnbpassage TO fc_nbpassage
       MOVE Wnbkms TO fc_nbkms
       OPEN EXTEND fc
       WRITE tamp_fc
       END-WRITE
       CLOSE fc.
       
       Affichage_course.
       OPEN INPUT fc
       MOVE 0 TO Wfin
       PERFORM WITH TEST AFTER UNTIL Wfin = 1
              READ fc NEXT
              AT END MOVE 1 TO Wfin 
              NOT AT END
                     DISPLAY "numéro:"fc_numCo
                     DISPLAY "ville :"fc_villeCompet
                     DISPLAY "passage :"fc_nbpassage
                     DISPLAY "NB kms :" fc_nbkms
              END-READ
       END-PERFORM  
       CLOSE fc.
       
       recherche_course.
       OPEN INPUT fc
       DISPLAY "rentrer type" 
       ACCEPT WtypeCo
       DISPLAY "rentrer ville de compétition" 
       ACCEPT WvilleCompet
       MOVE 0 TO Wfin
       MOVE 0 TO Wtrouver
       MOVE WtypeCo TO fc_typeCo
       START fc, KEY IS = fc_typeCo
       INVALID KEY DISPLAY "inexistant"
       NOT INVALID KEY
              PERFORM WITH TEST AFTER UNTIL Wfin = 1 OR Wtrouver = 1
                     READ fc NEXT
                     AT END MOVE 1 TO Wfin
                     NOT AT END  
                            IF WtypeCo=fc_typeCo THEN
                                   IF WvilleCompet=fc_villeCompet THEN
                                     DISPLAY "numéro de course:"fc_numCo
                                     MOVE 1 TO Wtrouver
                                   END-IF
                            END-IF
                     END-READ
              END-PERFORM
       END-START
       IF Wtrouver=0 THEN
              DISPLAY "course non trouver"
       END-IF
       CLOSE fc.
       
       affichage_gagnant.
       PERFORM recherche_course
       OPEN INPUT fp
       MOVE 0 TO Wfin
       MOVE 0 TO Wtrouver
       MOVE fc_numCo TO fp_numCo 
       START fp, KEY IS = fp_numCo 
       INVALID KEY DISPLAY "inexistant"
       NOT INVALID KEY
              PERFORM WITH TEST AFTER UNTIL Wfin = 1 OR Wtrouver=1
                     READ fp NEXT
                     AT END MOVE 1 TO Wfin 
                     NOT AT END
                            IF fc_numCo =fp_numCo THEN
                                   IF fp_classement = 1 THEN
                                          MOVE 1 TO Wtrouver
                                   END-IF
                            END-IF
                     END-READ
              END-PERFORM
       END-START
       CLOSE fp
       OPEN INPUT fa
       MOVE 0 TO Wtrouver
       PERFORM WITH TEST AFTER UNTIL Wtrouver = 0 
              MOVE fp_numA TO fa_numA
              READ fa
              INVALID KEY DISPLAY "inexistant"
                     MOVE 0 To Wtrouver
              NOT INVALID KEY DISPLAY fa_numA
                     DISPLAY "nom :"fa_nom
                     DISPLAY "prenom :"fa_prenom
                     DISPLAY "pays :" fa_pays
                     MOVE 1 To Wtrouver
              END-READ
       END-PERFORM
       CLOSE fa.

       affichage_performance_pays.
       DISPLAY "rentrer Pays" 
       ACCEPT Wpays
       OPEN INPUT fp
       MOVE 0 TO Wfin
       MOVE 0 TO Wtrouver
       PERFORM WITH TEST AFTER UNTIL Wfin = 1 OR Wtrouver=1
              READ fp NEXT
              AT END MOVE 1 TO Wfin 
              NOT AT END
                     IF fp_classement = 1 THEN
                            OPEN INPUT fa
                            MOVE fp_numA TO fa_numA
                            READ fa
                                   INVALID KEY DISPLAY "inexistant" 
                                   NOT INVALID KEY IF fa_pays =Wpays THEN
                                        DISPLAY "nom :"fa_nom
                                        DISPLAY "prenom :"fa_prenom
                            END-READ
                            CLOSE fa
                            OPEN INPUT fc
                            MOVE fp_numCo TO fc_numCo
                            READ fc
                                   INVALID KEY DISPLAY "inexistant" 
                                   NOT INVALID KEY
                                        DISPLAY "type Co :"fc_typeCo
                                        DISPLAY "Compet :"fc_villeCompet
                            END-READ
                            CLOSE fc
                            MOVE 1 TO Wtrouver
                     END-IF
              END-READ
       END-PERFORM
       CLOSE fp.


