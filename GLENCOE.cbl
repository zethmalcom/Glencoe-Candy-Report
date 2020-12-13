        IDENTIFICATION DIVISION.
        PROGRAM-ID. PROGRAM-3.
        AUTHOR. ZETH MALCOM.
      ****************************************************************
      *  This is a program for Glencoe Candy, Ltd that creates a 
      *  detailed listing of candy inventory . This report is grouped 
      *  by five different warehouses and four different candy vendors 
      *  CEO Dr. Yari Jensen purchases from .
      * ***************
      *  INPUT:
      *     The CANDY RECORD FILE contains the following
      *     data in each record:
      *         1.  WAREHOUSE ID
      *         2.  VENDOR ID
      *         3.  CANDY ID
      *         4.  CANDY NAME
      *         5.  CANDY BOX SIZE
      *         6.  NUMBER OF CASES IN STOCK
      *         7.  PURCHASE PRICE  
      *
      * ***************
      *  OUTPUT:
      *      The DETAILED CANDY REPORT contains 
      *      the following information:
      *    ************
      *    DETAIL LINE:
      *         1.  WAREHOUSE ID
      *         2.  EXPANDED VENDOR ID 
      *         3.  CANDY ID
      *         4.  CANDY NAME
      *         5.  EXPANDED CANDY SIZE
      *         6.  NUMBER OF CASES IN STOCK
      *         7.  TOTAL COST
      *
      *    *************
      *    FINAL TOTALS:
      *         1.  TOTAL COST FOR EACH CANDY, VENDOR, WAREHOUSE, AND 
      *             ALSO A GRANT TOTAL LINE
      *
      ****************************************************************

        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER.    IBM-PC.
        OBJECT-COMPUTER.    IBM-PC.

        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT CANDY-RECORD-FILE
                ASSIGN TO 'PR3FA19.TXT'
                ORGANIZATION IS LINE SEQUENTIAL.
            SELECT DETAILED-CANDY-REPORT
                ASSIGN TO PRINTER 'GLENCOE-DCR.TXT'.

        DATA DIVISION.
        FILE SECTION.

        FD    CANDY-RECORD-FILE
            RECORD CONTAINS 143 CHARACTERS.

        01  CANDY-RECORD.
            05  WAREHOUSE-ID          PIC X(4).
            05  VENDOR-ID             PIC X(1).
            05  CANDY-ID              PIC X(3).
            05  CANDY-ARRAY OCCURS 5 TIMES.
                10  CANDY-NAME        PIC X(15).
                10  BOX-SIZE          PIC A.
                10  CANDY-TYPE        PIC AA.
                10  CASES-IN-STOCK   PIC S9(4).
                10  PURCHASE-PRICE    PIC S999V99.

        FD   DETAILED-CANDY-REPORT
            RECORD CONTAINS 65 CHARACTERS.
        
        01  REPORT-RECORD             PIC X(80).

        WORKING-STORAGE SECTION.

        01  FLAGS-N-SWITCHES.

            05  FIRST-RECORD            PIC X(3)      VALUE 'YES'.
            05  EOF-FLAG                PIC X         VALUE ' '.
                88  NO-MORE-DATA                      VALUE 'N'.
        
        01  SUB  PIC 99   VALUE 1.

        01  TEMPORARY-FIELDS.

            05  TOTAL-PRICE-TMP        PIC 999999V99.
            05  TOTAL-CANDY-TMP        PIC 9999999V99.
            05  TOTAL-VENDOR-TMP       PIC 9999999V99.
            05  TOTAL-WAREHOUSE-TMP    PIC 99999999V99.
            05  GRAND-TOTAL-TMP        PIC 999999999V99.
            05  VENDOR-NAME-TMP        PIC X(18).
            05  CANDY-NAME-TMP         PIC X(13).

        01  DETAIL-FIELDS.
            
            05  DF-WAREHOUSE-ID       PIC X(4).
            05  DF-VENDOR-ID          PIC X.
            05  DF-CANDY-ID           PIC X(3).
            05  DF-CANDY-NAME         PIC X(15).
            05  DF-CASES-IN-STOCK     PIC 9(4).
            05  DF-PURCHASE-PRICE     PIC 999V99.
            05  DF-TOTAL-CANDY        PIC 9999999V99.
            05  DF-TOTAL-VENDOR       PIC 9999999V99.
            05  DF-TOTAL-WAREHOUSE    PIC 99999999V99.

        01  TOTAL-FIELDS.

            05  TF-TOTAL-CANDY        PIC 9999999V99.
            05  TF-TOTAL-VENDOR       PIC 9999999V99.
            05  TF-TOTAL-WAREHOUSE    PIC 99999999V99.


        01  REPORT-FIELDS.
            05  PROPER-SPACING             PIC S9   VALUE +1.
            05  WS-PAGE-NUMBER             PIC S9   VALUE +0.

        01  WS-DATE.
            05  RUN-YEAR                PIC 9(4).
            05  RUN-MONTH               PIC 99.
            05  RUN-DAY                 PIC 99.

        01  HEADING-ONE.
            
            05  FILLER            PIC X(31)  VALUE SPACES.
            05                    PIC X(18)  VALUE 'GLENCOE CADNY, LTD'.
            05  FILLER            PIC X(16)  VALUE SPACES.

        01  HEADING-TWO.

            05  FILLER            PIC X(7)   VALUE SPACES.
            05  H1-MONTH          PIC 99/.
            05  H1-DAY            PIC 99/.
            05  H1-YEAR           PIC 9999.
            05  FILLER            PIC X(15)  VALUE SPACES.
            05                    PIC X(16)  VALUE 'INVENTORY REPORT'.
            05  FILLER            PIC X(9)   VALUE SPACES.
            05                    PIC X(6)   VALUE 'PAGE: '.
            05  PAGE-NUM          PIC 99.

        01  HEADING-THREE.

            05                    PIC X(13)  VALUE '  WAREHOUSE: '.
            05  WAREHOUSE-HEADER  PIC X(4).
            05  FILLER            PIC X(48)  VALUE SPACES.

        01  HEADING-FOUR.

            05  FILLER            PIC X(5)  VALUE SPACES.
            05                    PIC X(8)  VALUE 'VENDOR: '.
            05  VENDOR-HEADER     PIC X(18).
            05  FILLER            PIC X(34)  VALUE SPACES.
            
        01  HEADING-FIVE.

            05  FILLER            PIC X(6)  VALUE SPACES.
            05                    PIC X(7)  VALUE 'CANDY: '.
            05  CANDY-HEADER      PIC X(3).
            05  FILLER            PIC X(49)  VALUE SPACES.

        01  HEADING-SIX.
            
            05  FILLER            PIC X(10)  VALUE SPACES.
            05                    PIC X(4)   VALUE 'NAME'.
            05  FILLER            PIC X(10)  VALUE SPACES.
            05                    PIC X(4)   VALUE 'SIZE'.
            05  FILLER            PIC X(8)   VALUE SPACES.
            05                    PIC X(4)   VALUE 'TYPE'.
            05  FILLER            PIC X(3)   VALUE SPACES.
            05                    PIC X(8)   VALUE 'IN STOCK'.
            05  FILLER            PIC X(4)   VALUE SPACES.
            05                    PIC X(10)  VALUE 'TOTAL COST'.

        01  DETAIL-LINE.

            05  FILLER             PIC X(5)  VALUE SPACES.
            05  DL-CANDY-NAME      PIC X(13).
            05  FILLER             PIC X(4)  VALUE SPACES.
            05  DL-BOX-SIZE        PIC X(10).  
            05  FILLER             PIC X(5)  VALUE SPACES.
            05  DL-CANDY-TYPE      PIC X(2).
            05  FILLER             PIC X(6)  VALUE SPACES.
            05  DL-CASES-IN-STOCK  PIC Z999.
            05  FILLER             PIC X(6)  VALUE SPACES.
            05  DL-TOTAL-COST      PIC $$$,$$$.99.            

        01  CANDY-TOTAL-LINE.
          
            05  FILLER             PIC X(18)  VALUE SPACES.
            05                     PIC X(13)  VALUE 'TOTAL CANDY: '.
            05  CTL-CANDY-NAME     PIC X(13).
            05  FILLER             PIC X(8)  VALUE SPACES.
            05  CTL-TOTAL-COST     PIC $,$$$,$$$.99.

        01  VENDOR-TOTAL-LINE.

            05  FILLER            PIC X(14)  VALUE SPACES.
            05                    PIC X(18)  VALUE 'TOTAL FOR VENDOR: '.
            05  VTL-VENDOR-NAME   PIC X(18).
            05  FILLER            PIC X(3)  VALUE SPACES.
            05  VTL-TOTAL-COST    PIC $,$$$,$$$.99.
        
        01  WAREHOUSE-TOTAL-LINE.

            05  FILLER         PIC X(11)  VALUE SPACES. 
            05                 PIC X(21)  VALUE 'TOTAL FOR WAREHOUSE: '.
            05  WTL-WAREHOUSE-ID  PIC X(4).
            05  FILLER         PIC X(16)  VALUE SPACES. 
            05  WTL-TOTAL-COST    PIC $$,$$$,$$$.99.

        01  GRAND-TOTAL-LINE.
 
            05  FILLER           PIC X(19)  VALUE SPACES. 
            05                   PIC X(12)  VALUE 'GRAND TOTAL:'.
            05  FILLER           PIC X(20)  VALUE SPACES.             
            05  GTL-TOTAL-COST   PIC $$$,$$$,$$$.99.
        


        PROCEDURE DIVISION.
        
        100-CONTROL-MODULE.

           PERFORM 150-HOUSEKEEPING-ROUTINE
           PERFORM 250-PROCESS-EMPLOYEE-DATA
           PERFORM 800-END-OF-FILE-ROUTINE

           .
           
        
        150-HOUSEKEEPING-ROUTINE.

           OPEN INPUT CANDY-RECORD-FILE
               OUTPUT DETAILED-CANDY-REPORT
           ACCEPT WS-DATE FROM DATE YYYYMMDD 
           MOVE WS-DATE(1:4) TO H1-YEAR
           MOVE WS-DATE(5:2) TO H1-MONTH
           MOVE WS-DATE(7:2) TO H1-DAY

           
           PERFORM 200-HEADER-ROUTINE
           .

        200-HEADER-ROUTINE.
           
           ADD 1 TO WS-PAGE-NUMBER
           MOVE WS-PAGE-NUMBER TO PAGE-NUM

           WRITE REPORT-RECORD FROM HEADING-ONE
               AFTER ADVANCING PAGE
           
           MOVE 1 TO PROPER-SPACING
           MOVE HEADING-TWO TO REPORT-RECORD 
           PERFORM 350-WRITE-A-LINE
           MOVE 2 TO PROPER-SPACING
           .

        250-PROCESS-EMPLOYEE-DATA.

            PERFORM UNTIL NO-MORE-DATA
               READ CANDY-RECORD-FILE
                  AT END
                     MOVE 'N' TO EOF-FLAG
                  NOT AT END
                     PERFORM 300-DATA-INPUT-ROUTINE
                END-READ
                      
            END-PERFORM
            .

        300-DATA-INPUT-ROUTINE.

           EVALUATE TRUE
                   WHEN FIRST-RECORD = 'YES'
                       MOVE 'NO' TO FIRST-RECORD
                       MOVE WAREHOUSE-ID TO DF-WAREHOUSE-ID
                       MOVE CANDY-ID TO DF-CANDY-ID
                       
                       EVALUATE TRUE
                         WHEN VENDOR-ID = "A"
                            MOVE "ATOMIC SWEETS" TO VTL-VENDOR-NAME
                         WHEN VENDOR-ID = "B"
                            MOVE "BOOZIE SWEETS" TO VTL-VENDOR-NAME
                         WHEN VENDOR-ID = "N"
                            MOVE "NELLIES SWEET SHOP" TO VTL-VENDOR-NAME 
                         WHEN VENDOR-ID = "T"
                            MOVE "TIGER TREATS" TO VTL-VENDOR-NAME
                         WHEN OTHER
                            STRING 
                               "INVALID" DELIMITED BY " " 
                               " " DELIMITED BY SIZE
                               VENDOR-ID DELIMITED BY " "
                               INTO VTL-VENDOR-NAME

                        END-EVALUATE 

                       PERFORM 375-PRINT-WAREHOUSE-HEADER
                       PERFORM 450-PRINT-VENDOR-HEADER
                       PERFORM 550-PRINT-CANDY-HEADER

                   
             
            END-EVALUATE     

            PERFORM VARYING SUB 
                  FROM 1 BY 1 UNTIL SUB > 5

               MOVE CANDY-NAME(SUB) TO CANDY-NAME-TMP
               MOVE CANDY-TYPE(SUB) TO DL-CANDY-TYPE
               MOVE CASES-IN-STOCK(SUB) TO DL-CASES-IN-STOCK
               
               IF SUB = 1 
                   MOVE CANDY-NAME-TMP TO DL-CANDY-NAME, CTL-CANDY-NAME
               ELSE
                   MOVE SPACES TO DL-CANDY-NAME 

                END-IF 
                   
               EVALUATE TRUE
                  WHEN BOX-SIZE(SUB) = "L"
                      MOVE "LARGE" TO DL-BOX-SIZE
                  WHEN BOX-SIZE(SUB) = "M"
                      MOVE "MEDIUM" TO DL-BOX-SIZE
                  WHEN BOX-SIZE(SUB) = "S"
                      MOVE "SMALL" TO DL-BOX-SIZE
                  WHEN BOX-SIZE(SUB) = "G"
                      MOVE "GIFT" TO DL-BOX-SIZE
                  WHEN BOX-SIZE(SUB) = "X"
                      MOVE "SAMPLE" TO DL-BOX-SIZE
                  WHEN BOX-SIZE(SUB) = " "
                      MOVE SPACES TO DL-BOX-SIZE
                  WHEN OTHER
                      STRING 
                         "BAD- " DELIMITED BY " " 
                         " " DELIMITED BY SIZE
                         BOX-SIZE(SUB) DELIMITED BY " "
                         INTO DL-BOX-SIZE

                END-EVALUATE


                IF CASES-IN-STOCK(SUB) IS NUMERIC
                   MOVE CASES-IN-STOCK(SUB) TO DL-CASES-IN-STOCK,
                   DF-CASES-IN-STOCK 
                
                ELSE 
                   MOVE '000' TO DF-CASES-IN-STOCK,DL-CASES-IN-STOCK

                   
               END-IF

               IF PURCHASE-PRICE(SUB) IS NUMERIC
                   MOVE PURCHASE-PRICE(SUB) TO DF-PURCHASE-PRICE       
               ELSE 
                   MOVE ZEROES TO DF-PURCHASE-PRICE
                   
               END-IF
                
                MULTIPLY DF-CASES-IN-STOCK BY DF-PURCHASE-PRICE
                GIVING DL-TOTAL-COST, TOTAL-PRICE-TMP

                ADD TOTAL-PRICE-TMP TO TF-TOTAL-WAREHOUSE,
                TF-TOTAL-VENDOR, TF-TOTAL-CANDY, GRAND-TOTAL-TMP
            

            MOVE DETAIL-LINE TO REPORT-RECORD
            PERFORM 350-WRITE-A-LINE
            MOVE 1 TO PROPER-SPACING
            
            END-PERFORM

        .

        350-WRITE-A-LINE.

           WRITE REPORT-RECORD
               AFTER ADVANCING PROPER-SPACING
           .
       
       375-PRINT-WAREHOUSE-HEADER.

          MOVE WAREHOUSE-ID TO WAREHOUSE-HEADER
          MOVE WAREHOUSE-ID TO DF-WAREHOUSE-ID
          WRITE REPORT-RECORD FROM HEADING-THREE
               AFTER ADVANCING 2 LINES

       .


       400-WAREHOUSE-BREAK.

          MOVE 0 TO WS-PAGE-NUMBER
          MOVE DF-WAREHOUSE-ID TO WTL-WAREHOUSE-ID
          MOVE TF-TOTAL-WAREHOUSE TO WTL-TOTAL-COST

          MOVE 2 TO PROPER-SPACING
          PERFORM 500-VENDOR-BREAK
          MOVE WAREHOUSE-TOTAL-LINE TO REPORT-RECORD
          PERFORM 350-WRITE-A-LINE
         
          MOVE ZEROS TO TF-TOTAL-WAREHOUSE
           

          PERFORM 200-HEADER-ROUTINE

       .

       450-PRINT-VENDOR-HEADER.

          MOVE VTL-VENDOR-NAME TO VENDOR-HEADER
          MOVE VENDOR-ID TO DF-VENDOR-ID
          WRITE REPORT-RECORD FROM HEADING-FOUR
               AFTER ADVANCING 2 LINES

       .


       500-VENDOR-BREAK.

          MOVE TF-TOTAL-VENDOR TO VTL-TOTAL-COST
          EVALUATE TRUE
                         WHEN VENDOR-ID = "A"
                            MOVE "ATOMIC SWEETS" TO VENDOR-NAME-TMP
                         WHEN VENDOR-ID = "B"
                            MOVE "BOOZIE SWEETS" TO VENDOR-NAME-TMP
                         WHEN VENDOR-ID = "N"
                            MOVE "NELLIES SWEET SHOP" TO VENDOR-NAME-TMP 
                         WHEN VENDOR-ID = "T"
                            MOVE "TIGER TREATS" TO VENDOR-NAME-TMP
                         WHEN OTHER
                            STRING 
                               "INVALID -" DELIMITED BY " " 
                               " " DELIMITED BY SIZE
                               VENDOR-ID DELIMITED BY " "
                               INTO VENDOR-NAME-TMP

                        END-EVALUATE 

          MOVE 2 TO PROPER-SPACING
          PERFORM 600-CANDY-BREAK
          MOVE VENDOR-TOTAL-LINE TO REPORT-RECORD
          PERFORM 350-WRITE-A-LINE
          MOVE VENDOR-NAME-TMP TO VTL-VENDOR-NAME

          
          MOVE ZEROS TO TF-TOTAL-VENDOR
          

          .


       550-PRINT-CANDY-HEADER.

          MOVE CANDY-ID TO CANDY-HEADER
          WRITE REPORT-RECORD FROM HEADING-FIVE
               AFTER ADVANCING 2 LINES
          WRITE REPORT-RECORD FROM HEADING-SIX
               AFTER ADVANCING 2 LINES

       .


       600-CANDY-BREAK.

          
          MOVE TF-TOTAL-CANDY TO CTL-TOTAL-COST

          MOVE 2 TO PROPER-SPACING
          MOVE CANDY-TOTAL-LINE TO REPORT-RECORD
          PERFORM 350-WRITE-A-LINE

          MOVE CANDY-ID TO DF-CANDY-ID
          MOVE ZEROS TO TF-TOTAL-CANDY

          .

       650-GRAND-TOTAL-BREAK.
          
          MOVE 0 TO WS-PAGE-NUMBER
          MOVE DF-WAREHOUSE-ID TO WTL-WAREHOUSE-ID
          MOVE TF-TOTAL-WAREHOUSE TO WTL-TOTAL-COST

          MOVE 2 TO PROPER-SPACING
          PERFORM 500-VENDOR-BREAK
          MOVE WAREHOUSE-TOTAL-LINE TO REPORT-RECORD
          PERFORM 350-WRITE-A-LINE
         
          MOVE ZEROS TO TF-TOTAL-WAREHOUSE
           

       .
       700-END-OF-JOB-ROUTINE.

          PERFORM 650-GRAND-TOTAL-BREAK
          PERFORM 750-GRAND-TOTAL-ROUTINE

          .

       750-GRAND-TOTAL-ROUTINE.
          
          MOVE GRAND-TOTAL-TMP TO GTL-TOTAL-COST
          WRITE REPORT-RECORD FROM GRAND-TOTAL-LINE
               AFTER ADVANCING 3 LINES
       .


       800-END-OF-FILE-ROUTINE.

          PERFORM 700-END-OF-JOB-ROUTINE

           CLOSE CANDY-RECORD-FILE
               DETAILED-CANDY-REPORT
           STOP RUN
           .

        
               
        
