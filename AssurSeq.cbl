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
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FICHIER-ASSUR.

      *    Séparateur entre les champs.

      *    Définition de la structure d'un enregistrement dans le 
      *    fichier.
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
           05  ASSUR-DEVISE       PIC X(1).



       WORKING-STORAGE SECTION.

      *    Contrôle de la fin de fichier.
       01  WS-EOF                  PIC X VALUE 'N'.
           88  EOF                 VALUE 'Y'.
           88  NOT-EOF             VALUE 'N'.

      *    Compteur pour les enregistrements lus.
       01  WS-REC-COUNT            PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.

      *    Ouverture du fichier et initialisation du traitement.
           OPEN INPUT FICHIER-ASSUR

      *    Boucle de lecture jusqu'à la fin du fichier.
           PERFORM UNTIL EOF
               READ FICHIER-ASSUR INTO ASSUR-REC

      *        Si fin de fichier atteinte, arrêter la lecture.
                   AT END
                       SET EOF TO TRUE

      *        Sinon, traiter l'enregistrement lu.
                   NOT AT END
                       ADD 1 TO WS-REC-COUNT

      *            Sélectionne les enregistrements 3 et 7 pour 
      *            l'affichage.
                       EVALUATE TRUE
                           WHEN WS-REC-COUNT = 3 OR WS-REC-COUNT = 7

      *                        Affiche les détails des enregistrements 
      *                        sélectionnés.
                               DISPLAY "ID: ", ASSUR-ID,
                                       " NOM: ", ASSUR-NOM,
                                       " DESCRIPTION: ", ASSUR-DESC,
                                       " TYPE: ", ASSUR-TYPE,
                                       " STATUT: ", ASSUR-STATUT,
                                       " DATE DEBUT: ", ASSUR-DATE-DEB,
                                       " DATE FIN: ", ASSUR-DATE-FIN,
                                       " MONTANT: ", ASSUR-MONTANT,
                                       " DEVISE: ", ASSUR-DEVISE
                           WHEN OTHER

      *                    Passe à l'enregistrement suivant sans action.
                               CONTINUE
                       END-EVALUATE
               END-READ
           END-PERFORM

      *    Fermeture du fichier après traitement.
           CLOSE FICHIER-ASSUR

      *    Affiche un message à la fin du traitement des enregistrements.
           DISPLAY "FIN DE TRAITEMENT DES ENREGISTREMENTS."

       STOP RUN.
