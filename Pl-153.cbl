       IDENTIFICATION DIVISION.
      * Tarea 101027 - 27/10/2009 Analisis de Mani
      * 104730 - Playa - Cierre CTG - Datos para certificados de deposito - Datos para liquidar Ind. Caja
      * 107021 - Playa - Modificacion por mejora: se cambia Z-MUESTRA de Z(5) a Z(6).
       PROGRAM-ID. PL-153.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.
      *{Bench}activex-def
      *{Bench}end
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY "CAMIONES.SEL".
       COPY "CIRCUITO.SEL".
       COPY "PRODUCTO.SEL".
       COPY "LISTADO.SEL".
       COPY "RUBROS.SEL".
       COPY "DEFANAL.SEL".
       COPY "ANALISIS.SEL".
       COPY "ANALISIS1.SEL".
       COPY "CLIENTES.SEL".
       COPY "PLANTA.SEL".
103871 COPY "PLANTA3.SEL".
       COPY "BANDAS.SEL".
       COPY "CONVENIO.SEL".
       COPY "PANTAL.SEL".

           SELECT OPTIONAL AR-WORK ASSIGN TO TEMPORARIO
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS CLAVE-WORK.

       DATA DIVISION.
       FILE SECTION.

       COPY "CAMIONES.CPY".
       COPY "LISTADO.CPY".
       COPY "CIRCUITO.CPY".
       COPY "PRODUCTO.CPY".
       COPY "RUBROS.CPY".
       COPY "ANALISIS.CPY".
       COPY "ANALISIS1.CPY".
       COPY "CLIENTES.CPY".
       COPY "PLANTA.CPY".
103871 COPY "PLANTA3.CPY".
       COPY "DEFANAL.CPY".
       COPY "BANDAS.CPY".
       COPY "CONVENIO.CPY".
       copy "PANTAL.cpy".

      $XFD FILE = WORK153
       FD  AR-WORK LABEL RECORD STANDARD.
       01  AR-WORK-REC.
           02 CLAVE-WORK.
              04 MUESTRA-WORK     PIC 9(12).
              04 CLAVE-NES-WORK   PIC X(24).
           02 REGISTRO-WORK       PIC X(4096).

       WORKING-STORAGE SECTION.

       01  AREA-CALL.
           02 TRANS-1900               PIC 9(12) VALUE ZEROS.
           02 TRANS-1901               PIC 9(12) VALUE ZEROS.
           02 FER                      PIC 9     VALUE ZEROS.
           02 SEM                      PIC X(3)  VALUE SPACES.

       01 ETAPAS-NES.
          03 ALL-ETAPAS-NES OCCURS 40 TIMES.
              05 ETAPA-NES       PIC  X(3).
              05 FECHA-ETAPA-NES PIC  9(8).
              05 HORA-ETAPA-NES  PIC  9(4).
              05 USUA-ETAPA-NES  PIC  X(15).

       01 II PIC 9(5).

       01 RUBROS-ANAL.
101027*   02 OCCURS-TABLA-RUBROS OCCURS 50 TIMES.
101027    02 OCCURS-TABLA-RUBROS OCCURS 100 TIMES.
             03 RUBRO-ANAL       PIC  9(4).
             03 PORC-ANAL        PIC  S9(3)V9(4).
             03 PORCA-ANAL REDEFINES PORC-ANAL  PIC  X(7).
             03 MERMA-ANAL       PIC  S9(9).
             03 COEF-ANAL        PIC  S9(3)V99.
             03 PESO1-ANAL       PIC  S9(3)V9(4).
             03 PESO2-ANAL       PIC  S9(3)V9(4).
             03 PESO3-ANAL       PIC  S9(3)V9(4).
             03 PESO4-ANAL       PIC  S9(3)V9(4).
             03 PESOG-ANAL       PIC  S9(3)V9(4).
             03 ORDEN-CAL-ANAL   PIC  999.
             03 ALFA-ANAL        PIC  9.
             03 BASE-ANAL        PIC  S9(9)V99.
             03 CALIDAD-ANAL     PIC  9.
             03 SIGNO-ANAL       PIC  X.
             03 VALOR-ANAL       PIC  S9(3)V99.
             03 GENAUT-ANAL      PIC  9.
             03 ORDEN-ING-ANAL   PIC  999.

       01 ARCHIVO-VISION PIC X(200).
       01 ARCH-VISION PIC X(200).

       01   DESDE-Z.
         03   AA-D-Z         PIC 9999.
         03   FILLER         PIC X VALUE '-'.
         03   MM-D-Z         PIC 99.
         03   FILLER         PIC X VALUE '-'.
         03   DD-D-Z         PIC 99.
       01   HASTA-Z.
         03   AA-H-Z         PIC 9999.
         03   FILLER         PIC X VALUE '-'.
         03   MM-H-Z         PIC 99.
         03   FILLER         PIC X VALUE '-'.
         03   DD-H-Z         PIC 99.
       01  TEMPORARIO-W.
           02 FILLER      PIC X VALUE "T".
           02 HORA-TMP    PIC 9(7).
 
       01  TEMPORARIO     PIC X(128).

            COPY "acugui.def".
            COPY "fonts.def".
            COPY "crtvars.def".
            COPY "winprint.def".
            COPY "Acucobol.def".

       77  KEY-STATUS IS SPECIAL-NAMES CRT STATUS PIC 9(5) VALUE 0.
           88 Screen-No-Input-Field               VALUE 97. 
           88 EXIT-PUSHED                  VALUE 27.
           88 EVENT-OCCURRED               VALUE 96.

            COPY "PL-153.WRK".
            COPY "COPY.CPY".
            copy "pantal.wrk".

       01  COL-PRT   PIC 9(5) VALUE 130.
       77  CUIT-CEDE PIC 9(13) VALUE ZEROS.
       77  ALTA-LNK  PIC 9.
       77  VAN       PIC 9(8) VALUE ZEROS.
       77  PUNT      PIC 9(8) VALUE ZEROS.
       77  SENAL-UNO PIC 9(8) VALUE ZEROS.
       77  TOT-1     PIC 9(8) VALUE ZEROS.
       77  TOT-2     PIC 9(8) VALUE ZEROS.
       77  TOT-3     PIC 9(8) VALUE ZEROS.
       77  TOT-4     PIC 9(8) VALUE ZEROS.
       77  T-BRUTO   PIC S9(11)   COMP-3 VALUE ZEROS.
       77  T-TARA    PIC S9(11)   COMP-3 VALUE ZEROS.
       77  T-NETO    PIC S9(11)   COMP-3 VALUE ZEROS.
       77  T-MERMAS  PIC S9(11)   COMP-3 VALUE ZEROS.
       77  T-NETON   PIC S9(11)   COMP-3 VALUE ZEROS.
       77  NETON     PIC S9(11)   COMP-3 VALUE ZEROS.
       77  TOTAL-HUM PIC S9(11)   COMP-3 VALUE ZEROS.
       77  TOTAL-NETO PIC S9(11)   COMP-3 VALUE ZEROS.
       77  AUX       PIC S9(11)   COMP-3 VALUE ZEROS.
       77  PORC      PIC S9(7)V99 COMP-3 VALUE ZEROS.
       01  AMD-W           PIC 9(8) VALUE ZERO.
       01  AMD-R-W REDEFINES AMD-W.
           03 AA-W2        PIC 9999.
           03 MM-W2        PIC 99.
           03 DD-W2        PIC 99.
 
        
       01 FECHA      PIC X(10).
       01 FECHA-R REDEFINES FECHA.
           03 DD-R      PIC 9(2).
           03 FILLER    PIC X.
           03 MM-R      PIC 9(2).
           03 FILLER    PIC X.
           03 AA-R      PIC 9(4).
****************************** AREA IMPRESION **************************
       01  HORA-D             PIC 9(8).
       01  HORA-DR REDEFINES HORA-D.
           02 HO-D            PIC 99.
           02 MI-D            PIC 99.
           02 SS-D            PIC 99.
           02 DE-D            PIC 99.
       01  HORA-H             PIC 9(4).
       01  HORA-HR REDEFINES HORA-H.
           02 HO-H            PIC 99.
           02 MI-H            PIC 99.
       01  QUE           PIC 99 VALUE ZEROS.
       01  CUAL.
           02 PARTE1     PIC X(4).
           02 PARTE2     PIC 9(4).
       01 NETO-NETO      PIC 9(7).
       01 NETO           PIC 9(15).
       77  K             PIC 9999 VALUE ZEROS.
       77  I             PIC 9999 VALUE ZEROS.
       01 JJ             PIC 9(04) VALUE ZEROS.
       01 Z-LINEA-01.
          02 Z-EMPRESA          PIC X(50).
          02 FILLER             PIC X(80) VALUE ALL SPACES.
          02 FILLER             PIC X(06) VALUE 'Hoja: '.
          02 Z-HOJA             PIC ZZZ9.
       01 Z-LINEA-01A.
          02 Z-PLANTA          PIC X(50).
       01 Z-LINEA-01B.
          02 Z-NOMCLI          PIC X(45).
          02 Z-CEDE            PIC X(45).
          02 Z-CONV            PIC Z(12).

       01 NRO-S                 PIC ZZZZ9.

       01 SUBRAY.
          02 FILLER        PIC X(200) VALUE ALL '_'.

       01 Z-LINEA-ESPA.
           02 FILLER PIC X(200) VALUE ALL SPACES.

       01 Z-LINEA-02.
           02 FILLER PIC X(34) VALUE
              'DETALLE DE ANALISIS desde el dia '.
           02 Z-DD-D            PIC 9(2)/. 
           02 Z-MM-D            PIC 9(2)/. 
           02 Z-AA-D            PIC 9(4). 
           02 FILLER PIC X(14) VALUE ' hasta el dia '.
           02 Z-DD-H            PIC 9(2)/.
           02 Z-MM-H            PIC 9(2)/.
           02 Z-AA-H            PIC 9(4).

       01 Z-LINEA-03.
          02 FILLER PIC X(2) VALUE "R".
          02 FILLER PIC X(6) VALUE "Mstra".
          02 FILLER PIC X(2) VALUE "Pr".
          02 FILLER PIC X(7) VALUE "Varie.".
          02 FILLER PIC X(11) VALUE " Calidad ".
          02 FILLER PIC X(06) VALUE " Hum".
          02 FILLER PIC X(02) VALUE "Af".
          02 FILLER PIC X(07) VALUE " Tierra".
          02 FILLER PIC X(07) VALUE " Casco.".
          02 FILLER PIC X(07) VALUE " Palos ".
          02 FILLER PIC X(07) VALUE " CExtr.".
          02 FILLER PIC X(07) VALUE " Casca.".
          02 FILLER PIC X(07) VALUE " GrSue.".
          02 FILLER PIC X(07) VALUE " GrDes.".
          02 FILLER PIC X(07) VALUE " MohIn.".
          02 FILLER PIC X(07) VALUE " MohEx.".
          02 FILLER PIC X(07) VALUE " D.xIn.".
          02 FILLER PIC X(07) VALUE "  Hela.".
          02 FILLER PIC X(07) VALUE " Podri.".
          02 FILLER PIC X(07) VALUE " Ardid.".
          02 FILLER PIC X(07) VALUE " Brota.".
          02 FILLER PIC X(07) VALUE " DaTot.".
          02 FILLER PIC X(07) VALUE " OColor".
          02 FILLER PIC X(07) VALUE " Manch.".
          02 FILLER PIC X(07) VALUE " MohCar".
          02 FILLER PIC X(07) VALUE " Pelado".
          02 FILLER PIC X(07) VALUE " Podrid".
          02 FILLER PIC X(13) VALUE " C.Porte".

       01 Z-LINEA-03-EXCEL.
          02 FILLER PIC X(2) VALUE "R".
          02 FILLER PIC X(6) VALUE "Mstra".
          02 FILLER PIC X(2) VALUE "Pr".
          02 FILLER PIC X(7) VALUE "Varie.".
          02 FILLER PIC X(11) VALUE " Calidad ".
          02 FILLER PIC X(06) VALUE " Hum".
          02 FILLER PIC X(02) VALUE "Af".
          02 FILLER PIC X(07) VALUE " Tierra".
          02 FILLER PIC X(07) VALUE " Casco.".
          02 FILLER PIC X(07) VALUE " Palos ".
          02 FILLER PIC X(07) VALUE " CExtr.".
          02 FILLER PIC X(07) VALUE " Casca.".
          02 FILLER PIC X(07) VALUE " GrSue.".
          02 FILLER PIC X(07) VALUE " GrDes.".
          02 FILLER PIC X(07) VALUE " MohIn.".
          02 FILLER PIC X(07) VALUE " MohEx.".
          02 FILLER PIC X(07) VALUE " D.xIn.".
          02 FILLER PIC X(07) VALUE "  Hela.".
          02 FILLER PIC X(07) VALUE " Podri.".
          02 FILLER PIC X(07) VALUE " Ardid.".
          02 FILLER PIC X(07) VALUE " Brota.".
          02 FILLER PIC X(07) VALUE " DaTot.".
          02 FILLER PIC X(07) VALUE " OColor".
          02 FILLER PIC X(07) VALUE " Manch.".
          02 FILLER PIC X(07) VALUE " Arrug.".
          02 FILLER PIC X(07) VALUE " Pelado".
          02 FILLER PIC X(07) VALUE " Podrid".
          02 FILLER PIC X(07) VALUE " MohCar".
          02 FILLER PIC X(07) VALUE " Ac.Ole".
          02 FILLER PIC X(13) VALUE " C.Porte".

        01 Z-LINEA-04.
           02 Z-REPRO           PIC XX.
107021*    02 Z-MUESTRA         PIC Z(5)B.
107021     02 Z-MUESTRA         PIC Z(6).
           02 Z-PRODUCTO        PIC XB.
           02 Z-VARIEDAD        PIC X(6)B.
           02 Z-CALIDAD         PIC X(11).
           02 Z-HUMEDAD         PIC ZZZ,ZZ.
           02 Z-CANT REDEFINES Z-HUMEDAD PIC Z(6).
           02 Z-AFLA            PIC BX.
           02 Z-PORCE           OCCURS 22 PIC ZZZ9,99 BLANK WHEN ZERO.
           02 Z-PORTE           PIC Z(13).

       01  Z-LINEA-05.
           02 FILLER            PIC X(10) VALUE SPACES.
           02 Z-LEYE            PIC X(20).
           02 Z-CANTNO          PIC ZZZ.ZZ9.

       01  FECHA PIC 9(6).
       01  FECH REDEFINES FECHA.
           02 DIA PIC 99.
           02 MES PIC 99.
           02 ANO PIC 99.
       01  AREA-TRABAJO.
            03 ORD                PIC X VALUE SPACES.
      
       01 NRO-CLIENTE PIC 9(05).
       01  DESDE-W              PIC 9(8).
       01  DESDE-WR REDEFINES DESDE-W.
           02 AA-W            PIC 9(4). 
           02 MM-W            PIC 9(2). 
           02 DD-W            PIC 9(2). 
       01  HASTA              PIC 9(8).
       01  HASTA-R REDEFINES HASTA.
           02 AA-H            PIC 9(4).
           02 MM-H            PIC 9(2).
           02 DD-H            PIC 9(2).
       01  HASTA-R2 REDEFINES HASTA.
           02 HASTA6-1        PIC 9(2).
           02 HASTA6-2        PIC 9(6).
       77  RENGLON PIC 99 VALUE 0.
       77  LIO                PIC XX.
       77  OPCION             PIC 99.
       77  DONDE              PIC 99.
       01  SALIDA-PRN         PIC X(12).
       01 SALIDA-PRN-R REDEFINES SALIDA-PRN.
          03 SALIDA-PRN-1 PIC 9(08).
          03 SALIDA-PRN-2 PIC X(04).

       01  CUAL-PASA PIC 9999 VALUE ZEROS.

       01 AREA-PROMEDIO.
101027*   02 ALL-AREA-PROMEDIO OCCURS 50.
101027    02 ALL-AREA-PROMEDIO OCCURS 100.
             04 RUBROPROM   PIC 9(5).
             04 ACUM1       PIC S9(11)V99.
             04 ACUM2       PIC S9(11)V99.
             
XXXXX  77  PLANTA-AUX             PIC 9(8).

103871 01 TABLA-CEDENTE.
103871   03 LISTA-CEDENTE  PIC 9(12) OCCURS 3.
103871 01 INDICE-CEDE    PIC 9.
       LINKAGE SECTION.

       SCREEN SECTION.
           COPY "WINPRINT.SCR".
           COPY "PL-153.scr".

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           IF USUARIO-EXT NOT > SPACES
              EXIT PROGRAM
              STOP RUN
           END-IF.

           PERFORM ACU-INITIAL-ROUTINE.
           PERFORM ACU-PL-153-SF-1-SCREEN-CREATE-WIN.

           MOVE NOMEMPRESA-EXT  TO Z-EMPRESA.
           MOVE NOMPLANTA-EXT   TO Z-PLANTA.
           MOVE 572 TO PLANTA-AUX
           ACCEPT HORA-TMP FROM TIME.
           MOVE SPACES TO TEMPORARIO.
           STRING TEMP-EXT      DELIMITED BY LOW-VALUE
                  TEMPORARIO-W  DELIMITED BY SIZE
                  INTO TEMPORARIO
           END-STRING.

           OPEN INPUT AR-CAMIONES AR-CIRCUITO AR-PRODUCTO AR-ANALISIS
                      AR-RUBROS AR-DEFANAL AR-CLIENTES AR-PLANTA
                      AR-CONVENIO AR-BANDAS AR-ANALISIS1
103871                AR-PLANTA3
                      .

           MOVE EMPRESA-EXT TO EMPRESA-PLANTA
           MOVE PLANTA-EXT TO CODIGO-PLANTA.
           READ AR-PLANTA INVALID KEY 
                INITIALIZE AR-PLANTA-REC
           END-READ.           
           CLOSE AR-PLANTA.
103871     MOVE EMPRESA-EXT TO EMPRESA-PLANTA3
103871     MOVE PLANTA-EXT TO CODIGO-PLANTA3.
103871     MOVE 1 TO DATOS-PLAYA-PLANTA3
103871     READ AR-PLANTA3 INVALID KEY
103871          INITIALIZE AR-PLANTA3-REC
103871     END-READ.
103871     CLOSE AR-PLANTA3.

       INICIO.
           MOVE ZEROS TO FECHA-DESDE-INPUT FECHA-HASTA-INPUT
                         HORA-DESDE-INPUT HORA-HASTA-INPUT.

           INITIALIZE PROD-INPUT PROD-OUTPUT ACOP-INPUT ACOP-OUTPUT
                      CONV-INPUT CONV-OUTPUT CAL-INPUT CAL-OUTPUT
                      CEDE-INPUT CEDE-OUTPUT.

       SF1.
           DISPLAY PL-153-SF-1-Screen.
           ACCEPT PL-153-SF-1-Screen.
           IF Screen-No-Input-Field
               accept omitted time 300
           END-IF.

           IF KEY-STATUS = 96
              IF EVENT-TYPE = CMD-CLOSE
                 GO TO END-PROGRAMA
              END-IF
           END-IF.

           IF KEY-STATUS = 27 GO TO INICIO.

           IF KEY-STATUS = 2
            IF CONTROL-ID = 9
              CALL "PL-016" USING PROD-INPUT
              CANCEL "PL-016"
              IF PROD-INPUT = ZEROS
                 MOVE 1 TO ACCEPT-CONTROL
                 MOVE 5 TO CONTROL-VALUE
                 GO TO SF1
              ELSE
                 MOVE PROD-INPUT TO CODIGO-PROD
                 READ AR-PRODUCTO INVALID KEY
                    MOVE 1 TO MSG-TIPO
                    MOVE 1 TO BUTTON-TIPO
                    MOVE 1 TO DEFAULT-BUTTON
                    MOVE SPACES TO MSG-ERROR
                    MOVE 'CODIGO DE PRODUCTO INCORRECTO'
                        TO MSG-1
                    CALL 'MSG' USING MSG-TIPO, MSG-RESPUESTA
                                MSG-1, MSG-2, MSG-3
                    MOVE 1 TO ACCEPT-CONTROL
                    MOVE 7 TO CONTROL-VALUE
                    GO TO SF1
                 END-READ
                 MOVE NOM-PROD TO PROD-OUTPUT
                 MOVE 1 TO ACCEPT-CONTROL
                 MOVE 8 TO CONTROL-VALUE
                 GO TO SF1
              END-IF
            END-IF
            IF CONTROL-ID = 26
              CALL "PL-123A" USING CONV-INPUT
              CANCEL "PL-123A"
              IF CONV-INPUT = ZEROS
                 MOVE 1 TO ACCEPT-CONTROL
                 MOVE 2 TO CONTROL-VALUE
                 GO TO SF1
              ELSE
                 PERFORM LEO-CONV THRU EXIT-LEO-CONV
                 GO TO SF1
              END-IF
            END-IF
            IF CONTROL-ID = 28
              CALL "PL-623" USING CAL-INPUT
              CANCEL "PL-623"
              IF CAL-INPUT = ZEROS
                 MOVE 1 TO ACCEPT-CONTROL
                 MOVE 3 TO CONTROL-VALUE
                 GO TO SF1
              ELSE
                 PERFORM LEO-CAL THRU EXIT-LEO-CAL
                 GO TO SF1
              END-IF
            END-IF
            IF CONTROL-ID = 23
              CALL "PL-010" USING ACOP-INPUT " "
              CANCEL "PL-010"
              IF ACOP-INPUT = ZEROS
                 MOVE 1 TO ACCEPT-CONTROL
                 MOVE 1 TO CONTROL-VALUE
                 GO TO SF1
              ELSE
                 MOVE ACOP-INPUT TO CODIGO-CLI
                 READ AR-CLIENTES INVALID KEY
                    MOVE 1 TO MSG-TIPO
                    MOVE 1 TO BUTTON-TIPO
                    MOVE 1 TO DEFAULT-BUTTON 
                    MOVE SPACES TO MSG-ERROR
                    MOVE 'CODIGO DE CLIENTES INCORRECTO' 
                        TO MSG-1
                    CALL 'MSG' USING MSG-TIPO, MSG-RESPUESTA
                                MSG-1, MSG-2, MSG-3
                    MOVE 1 TO ACCEPT-CONTROL
                    MOVE 1 TO CONTROL-VALUE
                    GO TO SF1
                 END-READ
                 MOVE NOM-CLI TO ACOP-OUTPUT
                 MOVE 1 TO ACCEPT-CONTROL
                 MOVE 2 TO CONTROL-VALUE
                 GO TO SF1
              END-IF
            END-IF
            IF CONTROL-ID = 13
              CALL "PL-010" USING CEDE-INPUT " "
              CANCEL "PL-010"
              IF CEDE-INPUT = ZEROS
                 MOVE 1 TO ACCEPT-CONTROL
                 MOVE 2 TO CONTROL-VALUE
                 GO TO SF1
              ELSE
                 MOVE CEDE-INPUT TO CODIGO-CLI
                 READ AR-CLIENTES INVALID KEY
                    MOVE 1 TO MSG-TIPO
                    MOVE 1 TO BUTTON-TIPO
                    MOVE 1 TO DEFAULT-BUTTON 
                    MOVE SPACES TO MSG-ERROR
                    MOVE 'CODIGO DE CEDENTE INCORRECTO' 
                        TO MSG-1
                    CALL 'MSG' USING MSG-TIPO, MSG-RESPUESTA
                                MSG-1, MSG-2, MSG-3
      *              MOVE 1 TO ACCEPT-CONTROL
      *              MOVE 1 TO CONTROL-VALUE
      *              GO TO SF1
                    MOVE SPACES TO NOM-CLI
                 END-READ
                 MOVE NOM-CLI TO CEDE-OUTPUT
                 MOVE 1 TO ACCEPT-CONTROL
                 MOVE 3 TO CONTROL-VALUE
                 GO TO SF1
              END-IF
             END-IF
           END-IF.

           IF KEY-STATUS = 1313
              MOVE 1 TO SENAL-PANTAL
              MOVE 13 TO KEY-STATUS
           END-IF.

           IF KEY-STATUS NOT = 13 GO TO SF1.

           MOVE FECHA-DESDE-INPUT TO DMA-CPY.
           MOVE DD-CPY TO DD-DES-CPY Z-DD-D.
           MOVE MM-CPY TO MM-DES-CPY Z-MM-D.
           MOVE AA-CPY TO AA-DES-CPY Z-AA-D.
           MOVE FECHA-HASTA-INPUT TO DMA-CPY.
           MOVE DD-CPY TO DD-HAS-CPY Z-DD-H.
           MOVE MM-CPY TO MM-HAS-CPY Z-MM-H.
           MOVE AA-CPY TO AA-HAS-CPY Z-AA-H.
           IF DESDE-CPY = ZEROS OR DD-DES-CPY < 01 
                                OR DD-DES-CPY > 31 
                                OR MM-DES-CPY < 01 
                                OR MM-DES-CPY > 12 
                                OR AA-DES-CPY < 1998
                    MOVE 1 TO MSG-TIPO
                    MOVE 1 TO BUTTON-TIPO
                    MOVE 1 TO DEFAULT-BUTTON 
                    MOVE SPACES TO MSG-ERROR
                    MOVE 'LA FECHA DESDE ES INCORRECTA' 
                        TO MSG-1
                    CALL 'MSG' USING MSG-TIPO, MSG-RESPUESTA
                                MSG-1, MSG-2, MSG-3
                    MOVE 1 TO ACCEPT-CONTROL
                    MOVE 1 TO CONTROL-VALUE
                    GO TO SF1
           END-IF.             
           
           IF HASTA-CPY = ZEROS OR DD-HAS-CPY < 01 
                                OR DD-HAS-CPY > 31 
                                OR MM-HAS-CPY < 01 
                                OR MM-HAS-CPY > 12 
                                OR AA-HAS-CPY < 1998
                    MOVE 1 TO MSG-TIPO
                    MOVE 1 TO BUTTON-TIPO
                    MOVE 1 TO DEFAULT-BUTTON 
                    MOVE SPACES TO MSG-ERROR
                    MOVE 'LA FECHA HASTA ES INCORRECTA' 
                        TO MSG-1
                    CALL 'MSG' USING MSG-TIPO, MSG-RESPUESTA
                                MSG-1, MSG-2, MSG-3
                    MOVE 1 TO ACCEPT-CONTROL
                    MOVE 2 TO CONTROL-VALUE
                    GO TO SF1
           END-IF.             
           
           IF DESDE-CPY GREATER HASTA-CPY
                    MOVE 1 TO MSG-TIPO
                    MOVE 1 TO BUTTON-TIPO
                    MOVE 1 TO DEFAULT-BUTTON 
                    MOVE SPACES TO MSG-ERROR
                    MOVE 'LA FECHA DESDE ES MAYOR A LA FECHA HASTA' 
                        TO MSG-1
                    CALL 'MSG' USING MSG-TIPO, MSG-RESPUESTA
                                MSG-1, MSG-2, MSG-3
                    MOVE 1 TO ACCEPT-CONTROL
                    MOVE 1 TO CONTROL-VALUE
                    GO TO SF1
           END-IF.             
           IF SENAL-PANTAL = 1
              ACCEPT NOMBRE-LISTADO FROM TIME
              MOVE SPACES TO ARCH-VISION
              STRING NOMBRE-LISTADO '-HOST' DELIMITED BY SIZE
                     INTO ARCH-VISION
              SET ENVIRONMENT ARCH-VISION TO 'VISION'

jc            MOVE SPACES TO CUAL-TMP-LISTADO
              STRING TEMP-EXT DELIMITED BY LOW-VALUE
                     ARCHIVO-TMP-LISTADO DELIMITED BY SIZE
                     INTO CUAL-TMP-LISTADO
              END-STRING

              OPEN OUTPUT PANTAL FOR BULK-ADDITION
              MOVE 1 TO CLAVE-PANTAL
              MOVE Z-LINEA-03-EXCEL TO REGISTRO-PANTAL
              WRITE PANTAL-REC
              GO TO ES-EXP1
           END-IF.
           
       OTRO-WINPRINT.
           MOVE Z-LINEA-02 TO titulo-print.
           MOVE 1 TO SENAL-COMP.
           PERFORM WIN-PRINT THRU EXIT-WIN-PRINT.
           IF KEY-STATUS NOT = PRINT-BUTTON-PRESSED AND
                               BROWSE-BUTTON-PRESSED
             destroy main-screen-prt
             destroy printing-window
              GO TO END-PROGRAMA
           END-IF.
           IF KEY-STATUS = PRINT-BUTTON-PRESSED 
             MOVE "-P SPOOLER-DIRECT" TO WHERE-PRINT
           ELSE
             PERFORM FILE-SPL THRU EXIT-FILE-SPL
             MOVE ARCHIVO-SPL TO WHERE-PRINT
             INSPECT WHERE-PRINT REPLACING TRAILING SPACES BY LOW-VALUE
             SET ENVIRONMENT "WIN_SPOOLER_PORT" TO WHERE-PRINT
           END-IF.
           OPEN OUTPUT LISTADO.
           write linea from k-restaurar after 0.
           write linea from k-apaisadaI after 0.
           write linea from k-chica     after 0.
           write linea from k-negra     after 0.  
       ES-EXP1.           
           IF CEDE-INPUT > ZEROS
              MOVE CEDE-INPUT TO CODIGO-CLI
              READ AR-CLIENTES INVALID KEY
                   INITIALIZE AR-CLIENTES-REC
              END-READ
              MOVE CUIT-CLI TO CUIT-CEDE
           ELSE
              MOVE 9999999999999 TO CUIT-CEDE
           END-IF.
           MOVE SPACES TO ARCHIVO-VISION
           STRING TEMPORARIO-W DELIMITED BY "  " "-HOST" 
                  DELIMITED BY SIZE INTO ARCHIVO-VISION
           INSPECT ARCHIVO-VISION REPLACING TRAILING SPACE BY LOW-VALUE
      
           SET ENVIRONMENT ARCHIVO-VISION TO 'VISION'
           OPEN OUTPUT AR-WORK FOR BULK-ADDITION.
           PERFORM MSG-PRINT.
                         
           MOVE 9999 TO TL.
           MOVE 0 TO PRIMERO Z-HOJA.
           MOVE 41 TO LINES-PER-PAGE.


           MOVE ZEROS TO VAN T-TARA T-BRUTO T-NETO T-MERMAS T-NETON.

           INITIALIZE AREA-PROMEDIO AR-CAMIONES-REC.

           MOVE EMPRESA-EXT       TO EMPRESA-NES.
           MOVE PLANTA-EXT        TO PLANTA-NES.
           MOVE DESDE-CPY         TO FEC-NETO-NES.

       SUMO-DIA.
           MOVE DD-DES-CPY TO DD-CPY.
           MOVE MM-DES-CPY TO MM-CPY.
           MOVE AA-DES-CPY TO AA-CPY.
           CALL 'DMAD' USING DMA-CPY TRANS-1900 TRANS-1901.
           SUBTRACT 15 FROM TRANS-1900.
           CALL 'DDMA' USING TRANS-1900 DMA-CPY SEM FER.
           MOVE DD-CPY TO DD-W.
           MOVE MM-CPY TO MM-W.
           MOVE AA-CPY TO AA-W.
           MOVE ACOP-OUTPUT TO Z-NOMCLI.
           MOVE CEDE-OUTPUT TO Z-CEDE.
           MOVE CONV-INPUT TO Z-CONV.
           INITIALIZE AR-CAMIONES-REC.
           MOVE EMPRESA-EXT          TO EMPRESA-NES.
           MOVE PLANTA-EXT           TO PLANTA-NES.
           MOVE DESDE-W              TO FEC-CAL-NES.
           MOVE ZEROS                TO HORA-CAL-NES.
*************************************************************************
           MOVE DD-DES-CPY TO DD-D-Z
           MOVE MM-DES-CPY TO MM-D-Z
           MOVE AA-DES-CPY TO AA-D-Z
           MOVE FECHA-HASTA-INPUT TO DMA-CPY.
           MOVE DD-CPY TO DD-HAS-CPY DD-H-Z.
           MOVE MM-CPY TO MM-HAS-CPY MM-H-Z.
           MOVE AA-CPY TO AA-HAS-CPY AA-H-Z.
           MOVE "EMPRESA_CODIGO = EEEEE AND PLANTA_CODIGO = PPPPP AND
      -      "CAMIONES_FEC_CAL BETWEEN 'DDDDDDDDDD' AND 'HHHHHHHHHH'"
                TO a4gl-where-constraint

           Inspect A4GL_WHERE_CONSTRAINT REPLACING ALL
                              'EEEEE'      BY EMPRESA-EXT
                              'PPPPP'      BY PLANTA-EXT
                              'DDDDDDDDDD' BY DESDE-Z
                              'HHHHHHHHHH' BY HASTA-Z

           Inspect A4GL_WHERE_CONSTRAINT
                   replacing trailing spaces by low-values.
*************************************************************************

           START AR-CAMIONES KEY NOT < ALT-CAMIONES-3 INVALID KEY
                 GO TO START-CAMITRAN
           END-START.

       LEO-CAMIONES.
           READ AR-CAMIONES NEXT RECORD AT END
                GO TO START-CAMITRAN
           END-READ.
        
           IF EMPRESA-NES   NOT EQUAL EMPRESA-EXT OR
              PLANTA-NES    NOT EQUAL PLANTA-EXT 
                            GO TO START-CAMITRAN
           END-IF.

           IF PROD-INPUT NOT = ZEROS
              IF PROD-INPUT NOT EQUAL PRODUCTO-NES
                 GO TO LEO-CAMIONES.

           IF ACOP-INPUT > ZEROS
              IF ACOP-INPUT NOT = ACOP-NES
                 GO TO LEO-CAMIONES.

103871     IF SISROM-PLANTA = 1
              IF CEDE-INPUT > ZEROS
                 IF ACOP2-NES = CEDE-INPUT OR
                   CUIT-CEDE = ORDEN1-CUIT-ADIZ OR
                   CUIT-CEDE = ORDEN2-CUIT-ADIZ
                   CONTINUE
                 ELSE
                   GO TO LEO-CAMIONES
                 END-IF
              END-IF
103871     ELSE
103871        IF CEDE-INPUT > ZEROS
103871           IF TITULAR-CP-INPUT = 1
103871              IF REMITE-ADIZ = CEDE-INPUT
103871                CONTINUE
103871              ELSE
103871                GO TO LEO-CAMIONES
103871              END-IF
103871           ELSE
103871             PERFORM BUSCAR-CEDENTE THRU EXIT-BUSCAR-CEDENTE
103871             IF  LISTA-CEDENTE(2) > 0
103871                IF CEDE-INPUT = LISTA-CEDENTE(2)
103871                   CONTINUE
103871                ELSE
103871                  GO TO LEO-CAMIONES
103871                END-IF
103871             ELSE
103871              GO TO LEO-CAMIONES
103871             END-IF
103871           END-IF
103871        END-IF
103871     END-IF.

           IF CONV-INPUT > ZEROS
              IF CONV-INPUT NOT = CONVENIO-NES
                 GO TO LEO-CAMIONES.
                 
           IF MUESTRA-NES NOT > ZEROS GO TO LEO-CAMIONES.

104730     MOVE EMPRESA-NES TO EMPRESA-CIRCUITO
104730     MOVE PLANTA-NES TO PLANTA-CIRCUITO
104730     MOVE CIRCUITO-NES TO CODIGO-CIRCUITO
104730     READ AR-CIRCUITO INVALID KEY
104730        INITIALIZE AR-CIRCUITO-REC
104730     END-READ.
104730     IF PMC-PLANTA = 1 AND REPRO-CIRCUITO = 1 AND  | ** SE DESCARTAN LOS HIJOS QUE SE PONDERAN, EN SU LUGAR QUEDA EL HIJO PONDERADO
104730        SIN-TOLVA-ADIZ  = 5
104730           INITIALIZE AR-CIRCUITO-REC   | ** ESTO SE HACE PORQUE EL CODIGO ANTERIOR NUNCA LEIA LA CIRCUITO PERO SI LA USABA
104730           GO TO LEO-CAMIONES
104730     END-IF
104730     INITIALIZE AR-CIRCUITO-REC   | ** ESTO SE HACE PORQUE EL CODIGO ANTERIOR NUNCA LEIA LA CIRCUITO PERO SI LA USABA


           INITIALIZE AR-ANALISIS-REC.
           MOVE EMPRESA-NES  TO EMPRESA-ANAL
           MOVE PLANTA-NES   TO T1PlaId
           MOVE MUESTRA-NES  TO T1Muestra
           MOVE PRODUCTO-NES TO T1Prd_Cod
           IF MUESTRA-NES = ZEROS
              MOVE INTERNO-NES TO T1Muestra
           END-IF
      **     READ AR-ANALISIS INVALID KEY
      **          INITIALIZE AR-ANALISIS-REC
      **     END-READ.

           INITIALIZE AR-ANALISIS1-REC
           MOVE EMPRESA-NES  TO EMPRESA-ANAL1
           MOVE PLANTA-nes   TO T1PlaId1
           MOVE MUESTRA-nes  TO T1Muestra1
           MOVE PRODUCTO-nes TO T1Prd_Cod1
           MOVE 1            TO LC06CodSec1
           MOVE 1            TO T2SubNro1
           IF MUESTRA-nes = ZEROS
              MOVE INTERNO-nes TO T1Muestra1
           END-IF
           READ AR-ANALISIS1 INVALID KEY
                INITIALIZE AR-ANALISIS1-REC
           END-READ.

      *****     CALL "SQL-ANA2" USING "L" CLAVE-ANALISIS RUBROS-ANAL

           IF CAL-INPUT NOT = ZEROS
              IF CAL-INPUT = 1
                 IF CALPMC-ANAL NOT = 0 AND 1 
                    GO TO LEO-CAMIONES
                 END-IF
              END-IF
              IF CAL-INPUT = 2
                 IF CALPMC-ANAL NOT = 2
                    GO TO LEO-CAMIONES
                 END-IF
                 IF AREA-ANAL = 1 
                    GO TO LEO-CAMIONES
                 END-IF                                                                 
              END-IF
              IF CAL-INPUT = 3
                 IF AREA-ANAL NOT = 1 
                    GO TO LEO-CAMIONES
                 END-IF
              END-IF
              IF CAL-INPUT = 4
                 IF CALPMC-ANAL NOT = 4
                    GO TO LEO-CAMIONES
                 END-IF
              END-IF
           END-IF.

           MOVE ZEROS TO I.

           CALL "SQL-CAM3" USING "L" CLAVE-CAMIONES ETAPAS-NES

           PERFORM VARYING K FROM 1 BY 1 UNTIL K > 40
                          OR ETAPA-NES(K) = SPACES OR I = 1
              IF FECHA-ETAPA-NES(K) > ZEROS AND 
                 FECHA-ETAPA-NES(K) < FEC-CAL-NES
                     MOVE FEC-CAL-NES TO FECHA-ETAPA-NES(K)
              END-IF
              IF ((CALPMC-ANAL = 1 OR = 2) AND ETAPA-NES(K) = "AFL")
              OR ((CALPMC-ANAL NOT = 1 AND NOT = 2) 
                            AND ETAPA-NES(K) = "AN3")
              OR (REPRO-INPUT = 1 AND ETAPA-NES(K) = 'AN1')
                  IF FECHA-ETAPA-NES(K) < DESDE-CPY OR > HASTA-CPY 
                           GO TO LEO-CAMIONES
                  END-IF
 
                  IF FECHA-ETAPA-NES(K) = DESDE-CPY 
                     AND HORA-ETAPA-NES(K) < HORA-DESDE-INPUT
                           GO TO LEO-CAMIONES
                  END-IF
                  IF FECHA-ETAPA-NES(K) = HASTA-CPY
                     AND HORA-ETAPA-NES(K) > HORA-HASTA-INPUT
                           GO TO LEO-CAMIONES
                  END-IF
                  MOVE 1 TO I
              END-IF
           END-PERFORM.

           IF I = 0 
              GO TO LEO-CAMIONES
           END-IF.

           MOVE MUESTRA-NES      TO MUESTRA-WORK.
           MOVE CLAVE-CAMIONES   TO CLAVE-NES-WORK.
           MOVE AR-CAMIONES-REC  TO REGISTRO-WORK.

           WRITE AR-WORK-REC INVALID KEY
                MOVE 1 TO MSG-TIPO
                MOVE 1 TO BUTTON-TIPO
                MOVE 1 TO DEFAULT-BUTTON 
                MOVE SPACES TO MSG-ERROR
                MOVE 'NO PUEDO GRABAR EL ARCHIVO TEMPORAL' 
                      TO MSG-1
                CALL 'MSG' USING MSG-TIPO, MSG-RESPUESTA
                                MSG-1, MSG-2, MSG-3
                END-CALL
           END-WRITE.

           GO TO LEO-CAMIONES.

       START-CAMITRAN.
      *     INITIALIZE AR-CAMITRAN-REC.
      *     MOVE EMPRESA-EXT          TO EMPRESA-TRAN.
      *     MOVE PLANTA-EXT           TO PLANTA-TRAN.
      *     MOVE DESDE-W              TO FEC-CAL-TRAN.
      *     MOVE ZEROS                TO HORA-CAL-TRAN.
      *
      *     START AR-CAMITRAN KEY NOT LESS THAN ALT-CAMITRAN-3
      *                                             INVALID KEY
      *     GO TO PREPARAR-WORK.
      * LEO-CAMITRAN.
      *     READ AR-CAMITRAN NEXT RECORD AT END
      *          GO TO PREPARAR-WORK.
      *  
      *     IF EMPRESA-TRAN   NOT EQUAL EMPRESA-EXT OR
      *        PLANTA-TRAN    NOT EQUAL PLANTA-EXT 
      *                       GO TO PREPARAR-WORK
      *     END-IF.
      *
      *     IF PROD-INPUT NOT = ZEROS
      *        IF PROD-INPUT NOT EQUAL PRODUCTO-TRAN
      *           GO TO LEO-CAMITRAN.
      *
      *     IF ACOP-INPUT > ZEROS
      *        IF ACOP-INPUT NOT = ACOP-TRAN
      *           GO TO LEO-CAMITRAN.
      *
      *     IF CEDE-INPUT > ZEROS
      *        IF CEDE-INPUT NOT = ACOP2-TRAN
      *           GO TO LEO-CAMITRAN.
      *
      *     IF CONV-INPUT > ZEROS
      *        IF CONV-INPUT NOT = CONVENIO-TRAN
      *           GO TO LEO-CAMITRAN.
      *
      *     IF MUESTRA-TRAN NOT > ZEROS GO TO LEO-CAMITRAN.
      *
      *     MOVE EMPRESA-TRAN TO EMPRESA-ANAL.
      *     MOVE PLANTA-TRAN  TO PLANTA-ANAL.
      *     MOVE INTERNO-TRAN TO INTERNO-ANAL.
      *     READ AR-ANALISIS INVALID KEY
      *          INITIALIZE AR-ANALISIS-REC
      *     END-READ.
      *     IF CAL-INPUT NOT = ZEROS
      *        IF CAL-INPUT = 1
      *           IF CALPMC-ANAL NOT = 0 AND 1 
      *              GO TO LEO-CAMITRAN
      *           END-IF
      *        END-IF
      *        IF CAL-INPUT = 2
      *           IF CALPMC-ANAL NOT = 2
      *              GO TO LEO-CAMITRAN
      *           END-IF
      *           IF AREA-ANAL = 1 
      *              GO TO LEO-CAMITRAN
      *           END-IF
      *        END-IF
      *        IF CAL-INPUT = 3
      *           IF AREA-ANAL NOT = 1 
      *              GO TO LEO-CAMITRAN
      *           END-IF
      *        END-IF
      *        IF CAL-INPUT = 4
      *           IF CALPMC-ANAL NOT = 4
      *              GO TO LEO-CAMITRAN
      *           END-IF
      *        END-IF
      *     END-IF.
      *     MOVE 0 TO I.
      *     PERFORM VARYING K FROM 1 BY 1 UNTIL K > 40
      *                    OR ETAPA-TRAN(K) = SPACES OR I = 1
      *        IF ((CALPMC-ANAL = 1 OR = 2) AND ETAPA-TRAN(K) = "AFL")
      *        OR ((CALPMC-ANAL NOT = 1 AND NOT = 2)
      *                           AND ETAPA-TRAN(K) = "AN3")
      *        OR (REPRO-INPUT = 1 AND ETAPA-TRAN(K) = 'AN1')
      *            IF FECHA-ETAPA-TRAN(K) < DESDE-CPY OR > HASTA-CPY
      *                     GO TO LEO-CAMITRAN
      *            END-IF
      *
      *            IF FECHA-ETAPA-TRAN(K) = DESDE-CPY
      *                 AND HORA-ETAPA-TRAN(K) < HORA-DESDE-INPUT
      *                     GO TO LEO-CAMITRAN
      *            END-IF
      *            IF FECHA-ETAPA-TRAN(K) = HASTA-CPY
      *               AND HORA-ETAPA-TRAN(K) > HORA-HASTA-INPUT
      *                     GO TO LEO-CAMITRAN
      *            END-IF
      *            MOVE 1 TO I
      *        END-IF
      *     END-PERFORM.
      *
      *     IF I = 0 GO LEO-CAMITRAN.
      *
      *     MOVE MUESTRA-TRAN      TO MUESTRA-WORK.
      *     MOVE CLAVE-CAMITRAN    TO CLAVE-NES-WORK.
      *     MOVE AR-CAMITRAN-REC   TO REGISTRO-WORK.
      *
      *     WRITE AR-WORK-REC INVALID KEY
      *          MOVE 1 TO MSG-TIPO
      *          MOVE 1 TO BUTTON-TIPO
      *          MOVE 1 TO DEFAULT-BUTTON 
      *          MOVE SPACES TO MSG-ERROR
      *          MOVE 'NO PUEDO GRABAR EL ARCHIVO TEMPORAL'
      *                TO MSG-1
      *          CALL 'MSG' USING MSG-TIPO, MSG-RESPUESTA
      *                          MSG-1, MSG-2, MSG-3
      *          END-CALL
      *     END-WRITE.
      *
      *     GO TO LEO-CAMITRAN.
      * PREPARAR-WORK.

           INITIALIZE AR-WORK-REC TOTAL-HUM TOTAL-NETO.

           CLOSE AR-WORK.
           OPEN INPUT AR-WORK.

           START AR-WORK KEY NOT LESS THAN CLAVE-WORK INVALID KEY
                 GO TO END-LISTADO
           END-START.

       LEO-WORK.
           READ AR-WORK NEXT RECORD AT END 
                     GO TO END-LISTADO
           END-READ.

           MOVE REGISTRO-WORK TO AR-CAMIONES-REC.

           INITIALIZE AR-ANALISIS-REC.
           MOVE EMPRESA-NES  TO EMPRESA-ANAL
           MOVE PLANTA-NES   TO T1PlaId
           MOVE MUESTRA-NES  TO T1Muestra
           MOVE PRODUCTO-NES TO T1Prd_Cod
           IF MUESTRA-NES = ZEROS
              MOVE INTERNO-NES TO T1Muestra
           END-IF
           READ AR-ANALISIS INVALID KEY
                INITIALIZE AR-ANALISIS-REC
           END-READ.

           INITIALIZE AR-ANALISIS1-REC
           MOVE EMPRESA-NES  TO EMPRESA-ANAL1
           MOVE PLANTA-nes   TO T1PlaId1
           MOVE MUESTRA-nes  TO T1Muestra1
           MOVE PRODUCTO-nes TO T1Prd_Cod1
           MOVE 1            TO LC06CodSec1
           MOVE 1            TO T2SubNro1
           IF MUESTRA-nes = ZEROS
              MOVE INTERNO-nes TO T1Muestra1
           END-IF
           READ AR-ANALISIS1 INVALID KEY
                INITIALIZE AR-ANALISIS1-REC
           END-READ.

           CALL "SQL-ANA2" USING "L" CLAVE-ANALISIS RUBROS-ANAL

           MOVE PRODUCTO-NES TO CODIGO-PROD.
           READ AR-PRODUCTO INVALID KEY
                INITIALIZE AR-PRODUCTO-REC
           END-READ.

           INITIALIZE Z-LINEA-04.

           IF INTERNO-NES > ZEROS AND CAMIONES_TURREPRO_1 > ZEROS 
              MOVE 'R' TO Z-REPRO
           ELSE
              MOVE ' ' TO Z-REPRO
           END-IF.
           MOVE MUESTRA-NES     TO Z-MUESTRA.
           IF GB-CIRCUITO = 1  MOVE "G" TO Z-PRODUCTO.
           IF GB-CIRCUITO = 2  MOVE "B" TO Z-PRODUCTO.
           MOVE CARTA-PORTE-NES TO Z-PORTE.
           MOVE APODO-PROD      TO Z-VARIEDAD.
           IF AREA-ANAL = 1
              MOVE CAL3-CPY   TO Z-CALIDAD
              ADD 1 TO TOT-4
              GO TO SALTO-4
           END-IF.
           IF CALPMC-ANAL = 0 OR = 1 
              MOVE CAL1-CPY   TO Z-CALIDAD
      *        MOVE "CONFORME" TO Z-CALIDAD
              ADD 1 TO TOT-1
           END-IF
           IF CALPMC-ANAL = 2 
              MOVE CAL2-CPY   TO Z-CALIDAD
      *        MOVE "NO CONFORME" TO Z-CALIDAD
              ADD 1 TO TOT-2
           END-IF.
           IF CALPMC-ANAL > 2 
              MOVE CAL4-CPY   TO Z-CALIDAD
      *        MOVE "INDUSTRIA" TO Z-CALIDAD
              ADD 1 TO TOT-3
           END-IF.
*********CASO 74640
           IF CALPMC-ANAL = 2 OR 3
              IF AREA-ANAL = 1 OR CALPMC-ANAL = 3
                 MOVE CAL3-CPY TO Z-CALIDAD
              ELSE
                 MOVE CAL2-CPY TO Z-CALIDAD
              END-IF
           END-IF.
*********FIN CASO 74640

       SALTO-4.
           MOVE HUMEDAD-NES TO Z-HUMEDAD.
           COMPUTE NETON ROUNDED = NETO-NES * HUMEDAD-NES.
           ADD NETON TO TOTAL-HUM.
           ADD NETO-NES TO TOTAL-NETO.
           IF RESAFLA-ANAL = 1 MOVE "N" TO Z-AFLA.
           IF RESAFLA-ANAL = 2 MOVE "P" TO Z-AFLA.

101027*    PERFORM VARYING II FROM 1 BY 1 UNTIL II > 50
101027     PERFORM VARYING II FROM 1 BY 1 UNTIL II > 100
              IF PORC-ANAL(II) > ZEROS
101027*          PERFORM VARYING K FROM 1 BY 1 UNTIL K > 50
101027           PERFORM VARYING K FROM 1 BY 1 UNTIL K > 100
                   IF RUBRO-ANAL(II) = RUBROPROM(K) OR
                      RUBROPROM(K) = ZEROS
                         COMPUTE NETON ROUNDED = NETO-NES
                                                  * PORC-ANAL(II)
                         ADD NETON TO ACUM1(K)
                         ADD NETO-NES TO ACUM2(K)
                         MOVE RUBRO-ANAL(II) TO RUBROPROM(K)
101027*                  MOVE 55 TO K
101027                   MOVE 105 TO K
                   END-IF
                 END-PERFORM
              END-IF
              IF RUBRO-ANAL(II) = 152
                 MOVE PORC-ANAL(II) TO Z-PORCE(1)
              END-IF
              IF RUBRO-ANAL(II) = 153
                 MOVE PORC-ANAL(II) TO Z-PORCE(2)
              END-IF
              IF RUBRO-ANAL(II) = 154
                 MOVE PORC-ANAL(II) TO Z-PORCE(3)
              END-IF
              IF RUBRO-ANAL(II) = 155
                 MOVE PORC-ANAL(II) TO Z-PORCE(4)
              END-IF
              IF RUBRO-ANAL(II) = 156
                 MOVE PORC-ANAL(II) TO Z-PORCE(5)
              END-IF
              IF RUBRO-ANAL(II) = 157
                 MOVE PORC-ANAL(II) TO Z-PORCE(6)
              END-IF
              IF RUBRO-ANAL(II) = 158
                 MOVE PORC-ANAL(II) TO Z-PORCE(7)
              END-IF
              IF RUBRO-ANAL(II) = 352
                 MOVE PORC-ANAL(II) TO Z-PORCE(8)
              END-IF
              IF RUBRO-ANAL(II) = 353
                 MOVE PORC-ANAL(II) TO Z-PORCE(9)
              END-IF
              IF RUBRO-ANAL(II) = 354
                 MOVE PORC-ANAL(II) TO Z-PORCE(10)
              END-IF
              IF RUBRO-ANAL(II) = 355
                 MOVE PORC-ANAL(II) TO Z-PORCE(11)
              END-IF
              IF RUBRO-ANAL(II) = 356
                 MOVE PORC-ANAL(II) TO Z-PORCE(12)
              END-IF
              IF RUBRO-ANAL(II) = 357
                 MOVE PORC-ANAL(II) TO Z-PORCE(13)
              END-IF
              IF RUBRO-ANAL(II) = 358
                 MOVE PORC-ANAL(II) TO Z-PORCE(14)
              END-IF
              IF RUBRO-ANAL(II) = 359
                 MOVE PORC-ANAL(II) TO Z-PORCE(15)
              END-IF
              IF RUBRO-ANAL(II) = 360
                 MOVE PORC-ANAL(II) TO Z-PORCE(16)
              END-IF
              IF RUBRO-ANAL(II) = 361
                 MOVE PORC-ANAL(II) TO Z-PORCE(17)
              END-IF
              IF RUBRO-ANAL(II) = 362
                 MOVE PORC-ANAL(II) TO Z-PORCE(18)
              END-IF
              IF RUBRO-ANAL(II) = 363
                 MOVE PORC-ANAL(II) TO Z-PORCE(19)
              END-IF
              IF RUBRO-ANAL(II) = 365
                 MOVE PORC-ANAL(II) TO Z-PORCE(20)
              END-IF
              IF RUBRO-ANAL(II) = 367
                 MOVE PORC-ANAL(II) TO Z-PORCE(21)
              END-IF
              IF RUBRO-ANAL(II) = 37
                 MOVE PORC-ANAL(II) TO Z-PORCE(22)
              END-IF
              IF SENAL-PANTAL NOT = 1
                 MOVE Z-PORCE(21) TO Z-PORCE(18)
              END-IF
           END-PERFORM.
           IF SENAL-PANTAL = 1
              ADD 1 TO CLAVE-PANTAL
              MOVE Z-LINEA-04 TO REGISTRO-PANTAL
              MOVE 2 TO NRO-SALTO-PANTAL
              WRITE PANTAL-REC
           ELSE
              PERFORM 1
              WRITE LINEA FROM Z-LINEA-04 AFTER 1
              ADD 1 TO TL
           END-IF.
           GO TO LEO-WORK.
       END-LISTADO.
           MOVE SPACES TO Z-LINEA-04.
           COMPUTE PORC ROUNDED = TOTAL-HUM / TOTAL-NETO.
           MOVE PORC TO Z-HUMEDAD.
101027*    PERFORM VARYING II FROM 1 BY 1 UNTIL II > 50
101027     PERFORM VARYING II FROM 1 BY 1 UNTIL II > 100
              IF RUBROPROM(II) > ZEROS
      *           COMPUTE PORC ROUNDED = ACUM1(II) / ACUM2(II)
                 COMPUTE PORC ROUNDED = ACUM1(II) / TOTAL-NETO
              END-IF
              IF RUBROPROM(II) = 152 MOVE PORC TO Z-PORCE(1)
              END-IF
              IF RUBROPROM(II) = 153 MOVE PORC TO Z-PORCE(2)
              END-IF
              IF RUBROPROM(II) = 154 MOVE PORC TO Z-PORCE(3)
              END-IF
              IF RUBROPROM(II) = 155 MOVE PORC TO Z-PORCE(4)
              END-IF
              IF RUBROPROM(II) = 156 MOVE PORC TO Z-PORCE(5)
              END-IF
              IF RUBROPROM(II) = 157 MOVE PORC TO Z-PORCE(6)
              END-IF
              IF RUBROPROM(II) = 158 MOVE PORC TO Z-PORCE(7)
              END-IF
              IF RUBROPROM(II) = 352 MOVE PORC TO Z-PORCE(8)
              END-IF
              IF RUBROPROM(II) = 353 MOVE PORC TO Z-PORCE(9)
              END-IF
              IF RUBROPROM(II) = 354 MOVE PORC TO Z-PORCE(10)
              END-IF
              IF RUBROPROM(II) = 355 MOVE PORC TO Z-PORCE(11)
              END-IF
              IF RUBROPROM(II) = 356 MOVE PORC TO Z-PORCE(12)
              END-IF
              IF RUBROPROM(II) = 357 MOVE PORC TO Z-PORCE(13)
              END-IF
              IF RUBROPROM(II) = 358 MOVE PORC TO Z-PORCE(14)
              END-IF
              IF RUBROPROM(II) = 359 MOVE PORC TO Z-PORCE(15)
              END-IF
              IF RUBROPROM(II) = 360 MOVE PORC TO Z-PORCE(16)
              END-IF
              IF RUBROPROM(II) = 361 MOVE PORC TO Z-PORCE(17)
              END-IF
              IF RUBROPROM(II) = 362 MOVE PORC TO Z-PORCE(18)
              END-IF
              IF RUBROPROM(II) = 363 MOVE PORC TO Z-PORCE(19)
              END-IF
              IF RUBROPROM(II) = 365 MOVE PORC TO Z-PORCE(20)
              END-IF
              IF RUBROPROM(II) = 367 MOVE PORC TO Z-PORCE(21)
              END-IF
              IF RUBROPROM(II) = 37  MOVE PORC TO Z-PORCE(22)
              END-IF
           END-PERFORM.
           IF SENAL-PANTAL = 1
              ADD 1 TO CLAVE-PANTAL
              MOVE Z-LINEA-04 TO REGISTRO-PANTAL
              MOVE 2 TO NRO-SALTO-PANTAL
              WRITE PANTAL-REC
           ELSE
              MOVE Z-PORCE(21) TO Z-PORCE(18)
              PERFORM 1
              WRITE LINEA FROM Z-LINEA-04 AFTER 2
              ADD 2 TO TL
           END-IF.
           INITIALIZE Z-LINEA-04.
           MOVE CAL1-CPY TO Z-CALIDAD.
      *     MOVE 'CANT.CONFORME' TO Z-LEYE.
           MOVE TOT-1 TO Z-CANT.
           IF SENAL-PANTAL = 1
              ADD 1 TO CLAVE-PANTAL
              MOVE Z-LINEA-04 TO REGISTRO-PANTAL
              MOVE 2 TO NRO-SALTO-PANTAL
              WRITE PANTAL-REC
           ELSE
              PERFORM 1
              WRITE LINEA FROM Z-LINEA-04 AFTER 2
              ADD 2 TO TL
           END-IF.
           MOVE CAL2-CPY TO Z-CALIDAD.
      *     MOVE 'CANT.NO CONFORME' TO Z-LEYE.
           MOVE TOT-2 TO Z-CANT.
           IF SENAL-PANTAL = 1 
              ADD 1 TO CLAVE-PANTAL
              MOVE Z-LINEA-04 TO REGISTRO-PANTAL
              MOVE 1 TO NRO-SALTO-PANTAL
              WRITE PANTAL-REC
           ELSE
              PERFORM 1
              WRITE LINEA FROM Z-LINEA-04 AFTER 1
              ADD 1 TO TL
           END-IF.
           MOVE CAL4-CPY TO Z-CALIDAD.
      *     MOVE 'CANT.INDUSTRIA' TO Z-LEYE.
           MOVE TOT-3 TO Z-CANT.
           IF SENAL-PANTAL = 1 
              ADD 1 TO CLAVE-PANTAL
              MOVE Z-LINEA-04 TO REGISTRO-PANTAL
              MOVE 1 TO NRO-SALTO-PANTAL
              WRITE PANTAL-REC
           ELSE
              PERFORM 1
              WRITE LINEA FROM Z-LINEA-04 AFTER 1
              ADD 1 TO TL
           END-IF.
           MOVE CAL3-CPY TO Z-CALIDAD.
      *     MOVE 'NO CONF. A REASG' TO Z-LEYE.
           MOVE TOT-4 TO Z-CANT.
           IF SENAL-PANTAL = 1 
              ADD 1 TO CLAVE-PANTAL
              MOVE Z-LINEA-04 TO REGISTRO-PANTAL
              MOVE 1 TO NRO-SALTO-PANTAL
              WRITE PANTAL-REC
           ELSE
              PERFORM 1
              WRITE LINEA FROM Z-LINEA-04 AFTER 1
              ADD 1 TO TL
           END-IF.
       DESTROY-MSG.
           DESTROY PRINTING-WINDOW.
           DESTROY msg-printing-window.
           CLOSE AR-WORK.
           DELETE FILE AR-WORK.
           SET ENVIRONMENT ARCHIVO-VISION TO SPACES
           IF SENAL-PANTAL = 1 
              GO TO ES-FINAL-PANTAL
           END-IF.
           MOVE SPACES TO LINEA.
           WRITE LINEA AFTER PAGE.
           CLOSE LISTADO.
           IF KEY-STATUS = BROWSE-BUTTON-PRESSED 
             STRING 'SVIEW.EXE ' ARCHIVO-SPL DELIMITED BY SIZE
                    INTO BROWSE-SPL
             END-STRING
             CALL  'C$RUN' USING BROWSE-SPL
             END-CALL
           END-IF.
           destroy main-screen-prt.
           destroy printing-window.
           IF KEY-STATUS NOT = PRINT-BUTTON-PRESSED 
              SET ENVIRONMENT "WIN_SPOOLER_PORT" TO SPACES
           END-IF
           GO TO END-PROGRAMA.
    
        1.
           IF TL > LINES-PER-PAGE
              ADD 1 TO HOJA
              MOVE HOJA TO Z-HOJA
              IF PRIMERO = 0
                 WRITE LINEA FROM Z-LINEA-ESPA AFTER 0
                 MOVE 1 TO PRIMERO
              ELSE
                 WRITE LINEA FROM Z-LINEA-ESPA AFTER PAGE
              END-IF
              WRITE LINEA FROM Z-LINEA-01 AFTER 0
              WRITE LINEA FROM Z-LINEA-01A AFTER 1
              WRITE LINEA FROM Z-LINEA-01B AFTER 2
              WRITE LINEA FROM Z-LINEA-02 AFTER 2
              WRITE LINEA FROM SUBRAY     AFTER 2
              WRITE LINEA FROM Z-LINEA-03 AFTER 1
              WRITE LINEA FROM SUBRAY     AFTER 1
              WRITE LINEA FROM Z-LINEA-ESPA AFTER 2
              MOVE 10 TO TL
           END-IF.
           COPY "WINPRINT.PRD".
       file-spl.
           move spaces to archivo-spl.
           accept horaf-spl from time.
           string TEMP-EXT delimited by low-value
                  'P' horaf-spl(2:) '.spl' delimited by size
               into archivo-spl
           end-string.
       exit-file-spl.
           exit.
       ES-FINAL-PANTAL.
           MOVE Z-LINEA-02 TO TITULO-PANTALLA.
           MOVE Z-LINEA-01 TO RENGLON-TITULO(1).
           MOVE Z-LINEA-01A TO RENGLON-TITULO(2).
           MOVE Z-LINEA-01B TO RENGLON-TITULO(3).
           MOVE Z-LINEA-02 TO RENGLON-TITULO(4).
           INITIALIZE FORMA-ARCHIVO.
           MOVE 02 TO FORMA-ARCHIVO-TAB(01).
           MOVE 06 TO FORMA-ARCHIVO-TAB(02).
           MOVE 02 TO FORMA-ARCHIVO-TAB(03).
           MOVE 07 TO FORMA-ARCHIVO-TAB(04).
           MOVE 11 TO FORMA-ARCHIVO-TAB(05).
           MOVE 06 TO FORMA-ARCHIVO-TAB(06).
           MOVE 02 TO FORMA-ARCHIVO-TAB(07).
           MOVE 07 TO FORMA-ARCHIVO-TAB(08).
           MOVE 07 TO FORMA-ARCHIVO-TAB(09).
           MOVE 07 TO FORMA-ARCHIVO-TAB(10).
           MOVE 07 TO FORMA-ARCHIVO-TAB(11).
           MOVE 07 TO FORMA-ARCHIVO-TAB(12).
           MOVE 07 TO FORMA-ARCHIVO-TAB(13).
           MOVE 07 TO FORMA-ARCHIVO-TAB(14).
           MOVE 07 TO FORMA-ARCHIVO-TAB(15).
           MOVE 07 TO FORMA-ARCHIVO-TAB(16).
           MOVE 07 TO FORMA-ARCHIVO-TAB(17).
           MOVE 07 TO FORMA-ARCHIVO-TAB(18).
           MOVE 07 TO FORMA-ARCHIVO-TAB(19).
           MOVE 07 TO FORMA-ARCHIVO-TAB(20).
           MOVE 07 TO FORMA-ARCHIVO-TAB(21).
           MOVE 07 TO FORMA-ARCHIVO-TAB(22).
           MOVE 07 TO FORMA-ARCHIVO-TAB(23).
           MOVE 07 TO FORMA-ARCHIVO-TAB(24).
           MOVE 07 TO FORMA-ARCHIVO-TAB(25).
           MOVE 07 TO FORMA-ARCHIVO-TAB(26).
           MOVE 07 TO FORMA-ARCHIVO-TAB(27).
           MOVE 07 TO FORMA-ARCHIVO-TAB(28).
           MOVE 07 TO FORMA-ARCHIVO-TAB(29).
           MOVE 13 TO FORMA-ARCHIVO-TAB(30).
           MOVE "LRLLLRLRRRRRRRRRRRRRRRRRRRRRRR" TO FORMA-ALINEAR.
jc    *     MOVE ARCHIVO-TMP-LISTADO TO NOMBRE-ARCHIVO.
jc         MOVE CUAL-TMP-LISTADO TO NOMBRE-ARCHIVO.
           INITIALIZE WFONT-DATA FONT-HANDLE-PANTAL.
           MOVE 12 TO WFONT-SIZE
      *     MOVE "Courier" TO WFONT-NAME
           MOVE "Arial" TO WFONT-NAME
           SET WFCHARSET-DONT-CARE TO TRUE
           SET WFONT-BOLD TO TRUE
           SET WFONT-ITALIC TO FALSE
           SET WFONT-UNDERLINE TO FALSE
           SET WFONT-STRIKEOUT TO FALSE
           SET WFFAMILY-DONT-CARE TO TRUE
           SET WFONT-FIXED-PITCH TO FALSE
           MOVE 1 TO WFONT-DEVICE.
           CALL "W$FONT" USING WFONT-GET-FONT, FONT-HANDLE-PANTAL,
                WFONT-DATA
           END-CALL.
           MOVE LOGO-HANDLE TO BITMAP.
           MOVE ZERO TO BITMAP.
           MOVE 01  TO BITMAP-ROW.
           MOVE 80  TO BITMAP-COL.
           MOVE 5   TO BITMAP-HEIGHT.
           MOVE 10  TO BITMAP-WIDTH.
           MOVE 2   TO BITMAP-FLAGS.
           CLOSE PANTAL.

           CALL "PANTALLA" USING PARAMETROS-GRILLA.
           CANCEL "PANTALLA".

           DESTROY FONT-HANDLE-PANTAL.

           DELETE FILE PANTAL.
           SET ENVIRONMENT ARCH-VISION TO SPACES

           GO TO END-PROGRAMA.
       END-PROGRAMA.
            CLOSE AR-CAMIONES AR-CIRCUITO AR-PRODUCTO 
                  AR-RUBROS AR-ANALISIS AR-DEFANAL AR-ANALISIS1. 

            PERFORM VARYING IP FROM 1 BY 1 UNTIL IP > 50
                 IF PGM-EXT(IP) = 'PL-153'
                    MOVE SPACES TO PGM-EXT(IP)
                    MOVE 150 TO IP
                 END-IF
            END-PERFORM.

            DESTROY PL-153-SF-1-Screen.
            DESTROY PL-153-SF-1-Handle.

            EXIT PROGRAM.
            STOP RUN.

      * Event Procedures ...
       PL-153-SF-1-EVENT. 
       LEO-PROD.
           IF PROD-INPUT NOT = ZEROS
              MOVE PROD-INPUT TO CODIGO-PROD
              READ AR-PRODUCTO INVALID KEY
                   MOVE 1 TO MSG-TIPO
                   MOVE 1 TO BUTTON-TIPO
                   MOVE 1 TO DEFAULT-BUTTON 
                   MOVE SPACES TO MSG-ERROR
                   MOVE 'CODIGO DE PRODUCTO INCORRECTO' TO MSG-1
                   CALL 'MSG' USING MSG-TIPO, MSG-RESPUESTA
                         MSG-1, MSG-2, MSG-3
                   MOVE 1 TO ACCEPT-CONTROL
                   MOVE 9 TO CONTROL-VALUE
                   EXIT PARAGRAPH
              END-READ

              MOVE NOM-PROD TO PROD-OUTPUT
              DISPLAY PL-153-LA-10-SCREEN
              MOVE 1 TO ACCEPT-CONTROL
              MOVE 10 TO CONTROL-VALUE
           END-IF.
       EXIT-LEO-PROD.
           EXIT.

       LEO-ACOP.
           IF ACOP-INPUT NOT = ZEROS
                 MOVE ACOP-INPUT TO CODIGO-CLI
                 READ AR-CLIENTES INVALID KEY
                    MOVE 1 TO MSG-TIPO
                    MOVE 1 TO BUTTON-TIPO
                    MOVE 1 TO DEFAULT-BUTTON 
                    MOVE SPACES TO MSG-ERROR
                    MOVE 'CODIGO DE CLIENTE INCORRECTO' 
                        TO MSG-1
                    CALL 'MSG' USING MSG-TIPO, MSG-RESPUESTA
                                MSG-1, MSG-2, MSG-3
                    MOVE 1 TO ACCEPT-CONTROL
                    MOVE 1 TO CONTROL-VALUE
                    EXIT PARAGRAPH
                 END-READ
                 MOVE NOM-CLI TO ACOP-OUTPUT
                 DISPLAY PL-153-LA-10-Screena
                 MOVE 1 TO ACCEPT-CONTROL
                 MOVE 2 TO CONTROL-VALUE
           END-IF.
       EXIT-LEO-ACOP.
           EXIT.
       LEO-CEDE.
           IF CEDE-INPUT NOT = ZEROS
                 MOVE CEDE-INPUT TO CODIGO-CLI
                 READ AR-CLIENTES INVALID KEY
                    MOVE 1 TO MSG-TIPO
                    MOVE 1 TO BUTTON-TIPO
                    MOVE 1 TO DEFAULT-BUTTON 
                    MOVE SPACES TO MSG-ERROR
                    MOVE 'CODIGO DE CEDENTE INCORRECTO' 
                        TO MSG-1
                    CALL 'MSG' USING MSG-TIPO, MSG-RESPUESTA
                                MSG-1, MSG-2, MSG-3
      *              MOVE 1 TO ACCEPT-CONTROL
      *              MOVE 1 TO CONTROL-VALUE
      *              EXIT PARAGRAPH
                     MOVE SPACES TO NOM-CLI
                 END-READ
                 MOVE NOM-CLI TO CEDE-OUTPUT
                 DISPLAY PL-153-LA-10-Screenab
                 MOVE 1 TO ACCEPT-CONTROL
                 MOVE 3 TO CONTROL-VALUE
           END-IF.
       EXIT-LEO-CEDE.
           EXIT.
       LEO-CONV.
           IF CONV-INPUT NOT = ZEROS
                 MOVE EMPRESA-EXT TO EMPRESA-CONVENIO
                 MOVE PLANTA-AUX TO PLANTA-CONVENIO
                 MOVE CONV-INPUT TO CODIGO-CONVENIO
                 READ AR-CONVENIO INVALID KEY
                    MOVE 1 TO MSG-TIPO
                    MOVE 1 TO BUTTON-TIPO
                    MOVE 1 TO DEFAULT-BUTTON
                    MOVE SPACES TO MSG-ERROR
                    MOVE 'CODIGO DE CONVENIO INCORRECTO'
                        TO MSG-1
                    CALL 'MSG' USING MSG-TIPO, MSG-RESPUESTA
                                MSG-1, MSG-2, MSG-3
                    MOVE 1 TO ACCEPT-CONTROL
                    MOVE 3 TO CONTROL-VALUE
                    EXIT PARAGRAPH
                 END-READ
                 MOVE NOMBRE-CONVENIO TO CONV-OUTPUT
                 DISPLAY PL-153-LA-10-Screenaa
                 MOVE 1 TO ACCEPT-CONTROL
                 MOVE 4 TO CONTROL-VALUE
           END-IF.
       EXIT-LEO-CONV.
           EXIT.
       LEO-CAL.
           IF CAL-INPUT NOT = ZEROS
                 MOVE CAL-INPUT TO CODIGO-BANDAS
                 READ AR-BANDAS INVALID KEY
                    MOVE 1 TO MSG-TIPO
                    MOVE 1 TO BUTTON-TIPO
                    MOVE 1 TO DEFAULT-BUTTON 
                    MOVE SPACES TO MSG-ERROR
                    MOVE 'CODIGO DE CALIDAD INCORRECTO' 
                        TO MSG-1
                    CALL 'MSG' USING MSG-TIPO, MSG-RESPUESTA
                                MSG-1, MSG-2, MSG-3
                    MOVE 1 TO ACCEPT-CONTROL
                    MOVE 4 TO CONTROL-VALUE
                    EXIT PARAGRAPH
                 END-READ
                 MOVE NOMBRE-BANDAS TO CAL-OUTPUT
                 DISPLAY PL-153-LA-10-Screenaaa
                 MOVE 1 TO ACCEPT-CONTROL
                 MOVE 5 TO CONTROL-VALUE
           END-IF.
       EXIT-LEO-CAL.
           EXIT.
       
103871 BUSCAR-CEDENTE.
103871     MOVE 0 TO INDICE-CEDE
103871     INITIALIZE TABLA-CEDENTE.
103871     IF RTE-COMER-NES > 0
103871        ADD 1 TO INDICE-CEDE
103871        MOVE RTE-COMER-NES TO  LISTA-CEDENTE(INDICE-CEDE)
103871     END-IF
103871     IF INTERMED-NES > 0
103871        ADD 1 TO INDICE-CEDE
103871        MOVE INTERMED-NES TO LISTA-CEDENTE(INDICE-CEDE)
103871     END-IF
103871     IF REMITE-ADIZ > 0
103871        ADD 1 TO INDICE-CEDE
103871        MOVE REMITE-ADIZ TO LISTA-CEDENTE(INDICE-CEDE)
103871     END-IF.
103871 EXIT-BUSCAR-CEDENTE.
103871     EXIT.

       COPY "PL-153.PRD".           
