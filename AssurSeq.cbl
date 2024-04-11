      *    *************************************************************
      *    PROGRAMME ASSURSEQ
      *    Ce programme lit un fichier séquentiel 'assurances.dat',
      *    et affiche spécifiquement les enregistrements 3 et 7. Après
      *    avoir traité ces enregistrements, le programme indique la
      *    fin de la lecture du fichier et termine son exécution.
      *    *************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASSURSEQ.
       AUTHOR. Pierre.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FICHIER-ASSUR ASSIGN TO 'assurances.dat'
               ORGANIZATION IS LINE SEQUENTIAL

      *    Ici, 'STATUS IS WS-FILE-STATUS' sert à enregistrer 
      *    le code de statut de chaque opération sur le fichier.
               STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  FICHIER-ASSUR.

      *     Définition de la structure d'un enregistrement 
      *    du fichier assurances.
       01  ASSUR-REC.
           05  ASSUR-ID           PIC X(8).
           05  FILLER             PIC X(1).
           05  ASSUR-NOM          PIC X(14).
           05  FILLER             PIC X(1).
           05  ASSUR-DESC         PIC X(14).
           05  FILLER             PIC X(1).
           05  ASSUR-TYPE         PIC X(41).
           05  FILLER             PIC X(1).
           05  ASSUR-STATUT       PIC X(8).
           05  FILLER             PIC X(1).
           05  ASSUR-DATE-DEB     PIC X(8).
           05  FILLER             PIC X(1).
           05  ASSUR-DATE-FIN     PIC X(8).
           05  FILLER             PIC X(1).
           05  ASSUR-MONTANT      PIC X(9).
           05  FILLER             PIC X(1).
           05  ASSUR-DEVISE       PIC X(3).

       WORKING-STORAGE SECTION.

      *    Variable pour stocker le code de statut de l'opération
      *    de fichier.
       01  WS-FILE-STATUS        PIC XX.

      *    Compteur pour suivre le nombre d'enregistrements lus.
       01  WS-REC-COUNT          PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.

      *    Ouverture du fichier pour lecture.
           OPEN INPUT FICHIER-ASSUR.
            
      *    Boucle jusqu'à la fin du fichier, indiquée par le code '10'.
           PERFORM UNTIL WS-FILE-STATUS = '10'
               
               READ FICHIER-ASSUR INTO ASSUR-REC

      *    Si fin de fichier, mettre à jour le code de statut.
                   AT END
                       MOVE '10' TO WS-FILE-STATUS
                       
                   NOT AT END
              
      *    Incrémenter le compteur d'enregistrements.
                       ADD 1 TO WS-REC-COUNT
                       
                       EVALUATE TRUE

      *    Traitement spécifique pour les enregistrements 3 et 7.
                           WHEN WS-REC-COUNT = 3 OR WS-REC-COUNT = 7
                               
                               DISPLAY "ID: ", ASSUR-ID,
                                       " NOM: ", ASSUR-NOM,
                                       " DESCRIPTION: ", ASSUR-DESC,
                                       " TYPE: ", ASSUR-TYPE,
                                       " STATUT: ", ASSUR-STATUT,
                                       " DATE DEBUT: ", ASSUR-DATE-DEB,
                                       " DATE FIN: ", ASSUR-DATE-FIN,
                                       " MONTANT: ", ASSUR-MONTANT,
                                       " DEVISE: ", ASSUR-DEVISE
                           
      *    Pour tous les autres enregistrements, ne rien faire.
                           WHEN OTHER
                               CONTINUE
                       END-EVALUATE
               END-READ

      *    Fin de la boucle de lecture.
           END-PERFORM.
           
      *    Fermeture du fichier après traitement.
           CLOSE FICHIER-ASSUR.

      *    Message indiquant la fin du traitement.
           DISPLAY "FIN DE TRAITEMENT DES ENREGISTREMENTS."
           
       STOP RUN.
