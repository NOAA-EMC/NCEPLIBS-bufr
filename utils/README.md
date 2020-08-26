# BUFR Utilities

This is a collection of commonly used BUFR utilities:
[readbp](#readbp) - Read prepbufr file and print reports
[readmp](#readmp) - Read any BUFR file and dump contents of each subset one at a time
[binv](#binv) - Print BUFR file inventory by message type
[sinv](#sinv) - Print inventory of satellite data by platform and instrument type
[cmpbqm](#cmpbqm) - Print inventory of prepbufr observations by variable, report type and quality marks

---

## readbp

A utility to read prepbufr files which prints each report one at a time, or jumps to a report with characteristics defined by various keys such as report type, subset type, xy locagtion, station id, etc. Keys can be entered as arguments to the script or entered while the program is running. Basic operation is to print one report at a time with the default being starting at the beginning and continuing until the end or the user enters 'q'. Summary of keys follows. A key is entered after a report is output. Most keys require an additional input after entering the key. The 'd' key dumps the BUFR contents of the report without further input.

|key| description                               | sample input     |
|:---:|-----------------------------------------|:----------------:|
|  r  | ON29 report type and/or instrument type |    10 20         |
|  k  | OI report type                          |      30          |
|  m  | BUFR message type                       |     ADPUPA       |
|  w  | lat/lon box                             |   x1, x2, y1, y2 |
|  s  | station id                              |      72016       |
|  d  | dump                                    | BUFR contents    |

Sample output for:  `./readbp gdas.20200812/00/gdas.t00z.prepbufr'`
```
--------------------------------------------------------------------------------
MESSAGE: ADPUPA       1     2    20081200
STATION: 89642      140.02   -66.67
TIME:      20081200      0.00
ELV:        43.00
PSL:     ********
TYPE:         11.     177.     220.
SOURCE:  vH7B
SEQUENCE **********
DATA:
 LVL CAT        POB        SPH        TOB        ZOB        UOB        VOB
   1   3  1002.0(2) *******(*) *******(*) *******(*)    -3.8(2)     8.2(2)
   2   1  1000.0(2) *******(*) *******(*) *******(*)    -3.8(2)     8.2(2)
   3   1   925.0(2) *******(*) *******(*) *******(*)    -5.1(2)     6.1(2)
   4   1   850.0(2) *******(*) *******(*) *******(*)    -4.2(2)     4.2(2)
   5   1   700.0(2) *******(*) *******(*) *******(*)     3.1(2)     8.5(2)
   6   1   500.0(2) *******(*) *******(*) *******(*)    21.2(2)    21.2(2)
   7   1   400.0(2) *******(*) *******(*) *******(*)    29.1(2)    24.4(2)
   8   1   300.0(2) *******(*) *******(*) *******(*)    26.9(2)    26.9(2)
   9   5   273.0(2) *******(*) *******(*) *******(*)    21.9(2)    26.0(2)
  10   1   250.0(2) *******(*) *******(*) *******(*)    29.5(2)    20.6(2)
  11   5   200.0(2) *******(*) *******(*) *******(*)    29.5(2)    20.6(2)
  12   1   150.0(2) *******(*) *******(*) *******(*)    35.5(2)    20.5(2)
  13   1   100.0(2) *******(*) *******(*) *******(*)    42.6(2)    19.9(2)
  14   1    70.0(2) *******(*) *******(*) *******(*)    49.8(2)    23.2(2)
  15   1    50.0(2) *******(*) *******(*) *******(*)    57.1(2)    26.6(2)
  16   1    30.0(2) *******(*) *******(*) *******(*)    70.5(2)    18.9(2)
  17   1    20.0(2) *******(*) *******(*) *******(*)    83.1(2)    22.3(2)
--------------------------------------------------------------------------------
```
---

## readmp

A utility to read any bufr file and dump the entire contents of each subset one at a time.

Sample output for: `./readmp gdas.20200812/00/gdas.t00z.sfcshp.tm00.bufr_d`

```
 MESSAGE TYPE NC001001

004001  YEAR                      2020.0  YEAR                          YEAR
004002  MNTH                         8.0  MONTH                         MONTH
004003  DAYS                        11.0  DAY                           DAY
004004  HOUR                        21.0  HOUR                          HOUR
004005  MINU                         0.0  MINUTE                        MINUTE
001198  RPID                     WCX7445  ( 8)CCITT IA5                 REPORT IDENTIFIER
           <ID1SQ>     1 REPLICATIONS
001203  SHPC8                    WCX7445  ( 8)CCITT IA5                 SHIP CALL SIGN (8 CHARACTERS)
           <ID2SQ>     0 REPLICATIONS
           <ID3SQ>     0 REPLICATIONS
005002  CLAT                      -53.10  DEGREES                       LATITUDE (COARSE ACCURACY)
006002  CLON                      -70.90  DEGREES                       LONGITUDE (COARSE ACCURACY)
007001  SELV                         0.0  METERS                        HEIGHT OF STATION
033215  CORN                         0.0  CODE TABLE                    CORRECTED REPORT INDICATOR
035200  RSRD                       256.0  FLAG TABLE(1)                 RESTRICTIONS ON REDISTRIBUTION
035201  EXPRSRD                  MISSING  HOURS                         EXPIRATION OF RESTRICTIONS ON REDISTRIBUTION
           {BID}     1 REPLICATIONS
035195  SEQNUM                       477  ( 4)CCITT IA5                 CHANNEL SEQUENCE NUMBER
035021  BUHD                      SIVC70  ( 6)CCITT IA5                 BULLETIN BEING MONITORED (TTAAii)
035023  BORG                        KWBC  ( 4)CCITT IA5                 BULLETIN BEING MONITORED (CCCC)
035022  BULTIM                    112100  ( 6)CCITT IA5                 BULLETIN BEING MONITORED (YYGGgg)
035194  BBB                      MISSING  ( 6)CCITT IA5                 BULLETIN BEING MONITORED (BBB)
           {RCPTIM}     1 REPLICATIONS
008202  RCTS                         0.0  CODE TABLE                    RECEIPT TIME SIGNIFICANCE
004200  RCYR                      2020.0  YEAR                          YEAR   - TIME OF RECEIPT
004201  RCMO                         8.0  MONTH                         MONTH  - TIME OF RECEIPT
004202  RCDY                        11.0  DAY                           DAY    - TIME OF RECEIPT
004203  RCHR                        21.0  HOUR                          HOUR   - TIME OF RECEIPT
004204  RCMI                        22.0  MINUTE                        MINUTE - TIME OF RECEIPT
002193  ITSO                         2.0  CODE TABLE                    IND TYPE OF STATION OPERATION PAST/PRESENT WEATH
002001  TOST                         1.0  CODE TABLE                    TYPE OF STATION
013194  INPC                         4.0  CODE TABLE                    INDIC INCLUSION/OMISSION OF PREC
020001  HOVI                     MISSING  METERS                        HORIZONTAL VISIBILITY
002002  TIWM                        12.0  FLAG TABLE(1,2)               TYPE OF INSTRUMENTATION FOR WIND MEASUREMENT
033195  QMWN                     MISSING  CODE TABLE                    SDMEDIT/QUIPS QUALITY MARK FOR WIND
011001  WDIR                       310.0  DEGREES TRUE                  WIND DIRECTION
011002  WSPD                         7.2  METERS/SECOND                 WIND SPEED
           <WNDSQ2>     0 REPLICATIONS
033193  QMAT                     MISSING  CODE TABLE                    SDMEDIT/QUIPS QUALITY MARK FOR TEMPERATURE
012101  TMDB                      276.45  DEGREES KELVIN                TEMPERATURE/DRY BULB TEMPERATURE
033194  QMDD                     MISSING  CODE TABLE                    SDMEDIT/QUIPS QUALITY MARK FOR MOISTURE
012103  TMDP                      274.85  DEGREES KELVIN                DEW POINT TEMPERATURE
002038  MSST                     MISSING  CODE TABLE                    METHOD OF SEA SURFACE TEMPERATURE MEASUREMENT
033218  QMST                     MISSING  CODE TABLE                    SDMEDIT/QUIPS QUALITY MARK FOR SEA SURFACE TEMPE
022043  SST1                     MISSING  DEGREES KELVIN                SEA TEMPERATURE
           <TMPSQ2>     0 REPLICATIONS
           <TMPSQ3>     0 REPLICATIONS
033207  QMPR                     MISSING  CODE TABLE                    SDMEDIT/QUIPS QUALITY MARK FOR PRESSURE
010004  PRES                     MISSING  PASCALS                       PRESSURE
010051  PMSL                     98660.0  PASCALS                       PRESSURE REDUCED TO MSL
010063  CHPT                         7.0  CODE TABLE                    CHARACTERISTIC OF PRESSURE TENDENCY
010061  3HPC                      -350.0  PASCALS                       3 HOUR PRESSURE CHANGE
010062  24PC                     MISSING  PASCALS                       24 HOUR PRESSURE CHANGE
013021  TP06                     MISSING  KG/METER**2                   TOTAL PRECIPITATION PAST 6 HOURS
           <PCPSQ2>     0 REPLICATIONS
           <PCPSQ3>     0 REPLICATIONS
020010  TOCC                     MISSING  %                             CLOUD COVER (TOTAL)
020201  HBLCS                       14.0  CODE TABLE                    HEIGHT ABOVE SURFACE OF BASE OF LOWEST CLOUD SEE
           {CLDSQ1}     0 REPLICATIONS
           <PPWSQ1>     0 REPLICATIONS
           <WAVSQ1>     0 REPLICATIONS
           <WAVSQ2>     0 REPLICATIONS
           {WAVSQ3}     0 REPLICATIONS
           <MPLSQ1>     1 REPLICATIONS
001193  TDMP                         0.0  CODE TABLE                    TRUE DIRECTION OF SHIP DURING PAST 3 HOURS
001200  ASMP                         0.0  CODE TABLE                    AVG SPD OF SHIP DURING PAST 3 HOURS
           <ICESQ1>     0 REPLICATIONS
           <RPSEC3>     0 REPLICATIONS
           {RAWRPT}     0 REPLICATIONS

 >>> END OF SUBSET <<<
```
---

## binv

A utility to print a BUFR file inventory by message type.

Sample output for: `./binv gdas.20200812/00/gdas.t00z.prepbufr`
```
type        messages       subsets         bytes

ADPUPA           363          1427       3091984        3.93
AIRCAR          1817        169107      18052250       93.07
AIRCFT           298         29002       2956428       97.32
SATWND          1439        192430      14317604      133.72
PROFLR             1             1          1050        1.00
VADWND            76          1853        733606       24.38
ADPSFC          1266        135037      12582648      106.66
SFCSHP           156         20001       1552260      128.21
GPSIPW             5           577         31428      115.40
RASSDA            10           200         90976       20.00
ASCATW          1390        225151      13809272      161.98
SYNDAT             2           102         13234       51.00
TOTAL           6823        774888      67232740
```
---

## sinv

Utility to print an inventory of satellite data by platform and instrument type.

Sample output for: `./sinv gdas.20200812/00/gdas.t00z.satwnd.tm00.bufr_d`
```
003  METOP-1           7220
004  METOP-2           8911
055  METEOSAT-8      172430
070  METEOSAT-1      176712
173  Himawari-8      133715
209  NOAA-18            147
223  NOAA-19           1565
224  NPP              41006  NOAA Imaging multi-spectral radiometer (vis/IR) VIIRS (Visible/infrared imager r
225  NOAA-20          47747  NOAA Imaging multi-spectral radiometer (vis/IR) VIIRS (Visible/infrared imager r
270  GOES-16        1156231  NOAA Imaging multi-spectral radiometer ABI (Advanced baseline imager)
271  GOES-17        1890729  NOAA Imaging multi-spectral radiometer ABI (Advanced baseline imager)
471  INSAT-3D         24108
473  INSAT-3DR        79427
784  AQUA              2850
```
---

## cmpbqm

An inventory of prepbufr observations by variable, report type, and quality mark made from a prepbufr file. The ob type,total count,and quality marks are listed by column. The cka and ckb columns are counts of observed values with missing qm, or qms with missing observations. The cka and ckb should be zero but sometimes they're not. The qm values are found in [bufr code tables](https://www.emc.ncep.noaa.gov/mmb/data_processing/prepbufr.doc/table_7.htm), but below a quick summary. The GSI qms are added by a program run by the `fit2obs` system which copies that information from the convstat files. The sample output prepbufr was after prep but pre-analysis.
|Quality Marker| Description|
|:--:|-----------|
|0   |  keep flag|
|1   |  passed checks and/or corrected by cqc|
|2   |  not checked|
|3   |  suspicious|
|4-7 | rejected by oiqc (original mark + 4)|
|8   |  Ps more than 100mb off|
|9   |  filtered by missing ob errors in gsi error table|
|10  |  rejected by gsi gross check|
|11  |  rejected by gsi varqc|
|12  |  on manual reject list|
|13  |  rejected by cqc or acqc|
|14  |  sdm reject (manual purge)|
|15  |  rejected by prepdata code (ie failed various sanity checks)|

Sample output for: `./cmpbqm gdas.20200811/00/gdas.t00z.prepbufr`
```
DATA  VALID AT  2020081100

 PRESSURE

typ   tot    0-3    4-7      8      9    10     11    12    13    14    15    cka    ckb
----------------------------------------------------------------------------------------
120 32154| 31952|     0|    43|     2|     0     0     0     1   156     0|     0|     0
126  1169|  1169|     0|     0|     0|     0     0     0     0     0     0|     0|     0
130   602|   601|     0|     1|     0|     0     0     0     0     0     0|     0|     0
131 10321| 10321|     0|     0|     0|     0     0     0     0     0     0|     0|     0
133 86356| 86356|     0|     0|     0|     0     0     0     0     0     0|     0|     0
134  4170|  4170|     0|     0|     0|     0     0     0     0     0     0|     0|     0
135   208|   208|     0|     0|     0|     0     0     0     0     0     0|     0|     0
180  9296|  9026|     0|     6|     0|     0     0     0     0   203    61|     0|     0
181 23224| 20989|     0|   326|    10|     0     0     0     0    87  1812|     0|     0
183  3626|     0|     0|    12|  3582|     0     0     0     0    32     0|     0|     0
187 42698| 42300|     0|   112|     0|     0     0     0     0   286     0|     0|     0
220 39385| 39194|     0|     2|     0|     0     0     0     0   189     0|     0|     0
221   720|   720|     0|     0|     0|     0     0     0     0     0     0|     0|     0
224 16527| 16527|     0|     0|     0|     0     0     0     0     0     0|     0|     0
229     4|     4|     0|     0|     0|     0     0     0     0     0     0|     0|     0
230   166|   166|     0|     0|     0|     0     0     0     0     0     0|     0|     0
231 10319| 10319|     0|     0|     0|     0     0     0     0     0     0|     0|     0
233 86112| 86112|     0|     0|     0|     0     0     0     0     0     0|     0|     0
234  4170|  4170|     0|     0|     0|     0     0     0     0     0     0|     0|     0
235   208|   208|     0|     0|     0|     0     0     0     0     0     0|     0|     0
242 27875| 25521|     0|     0|     0|     0     0     0  2354     0     0|     0|     0
243  7040|  7040|     0|     0|     0|     0     0     0     0     0     0|     0|     0
250 58010| 52503|     0|     0|     0|     0     0     0  5507     0     0|     0|     0
252 53788| 48854|     0|     0|     0|     0     0     0  4934     0     0|     0|     0
253 13601| 13601|     0|     0|     0|     0     0     0     0     0     0|     0|     0
254 34156| 34156|     0|     0|     0|     0     0     0     0     0     0|     0|     0
257  8139|  8139|     0|     0|     0|     0     0     0     0     0     0|     0|     0
258  4127|  4127|     0|     0|     0|     0     0     0     0     0     0|     0|     0
259  5208|  5208|     0|     0|     0|     0     0     0     0     0     0|     0|     0
280  8310|  8049|     0|     0|     0|     0     0     0     0   203    58|     0|     0
281 22689| 20662|     0|   190|     0|     0     0     0     0    87  1750|     0|     0
282   240|   240|     0|     0|     0|     0     0     0     0     0     0|     0|     0
284  3542|  3497|     0|    12|     0|     0     0     0     0    32     1|     0|     0
287 42327| 41985|     0|    73|     0|     0     0     0     0   268     1|     0|     0
290231701|231701|     0|     0|     0|     0     0     0     0     0     0|     0|     0

 SPECIFIC HUMIDTY

120 28995| 14978|     0|     1| 13528|     0     0     0     3   219   266|     0|     0
133 13662|  9428|     0|     0|  3623|     0     0     0   329   216    66|     0|     0
134  3948|  2408|     0|     0|   165|     0     0     0   680   689     6|     0|     0
180  4327|  4299|     0|     0|     0|     0     0     0     0     0    28|     0|     0
181 22680|     0|     0|   311| 20525|     0     0     0     0  1844     0|     0|     0
183  2594|     0|     0|    12|  2550|     0     0     0     0    32     0|     0|     0
187 42303|     0|     0|   112| 41463|     0     0     0     0   728     0|     0|     0

 TEMPERATURE

120 31833| 31540|     0|     1|     2|     0     0     0    71   219     0|     0|     0
126  1169|     0|     0|     0|   861|     0     0     0   308     0     0|     0|     0
130   436|   386|     0|     0|     0|     0     0     0    30    20     0|     0|     0
131 10321|  9297|     0|     0|     0|     0     0     0   890   134     0|     0|     0
133 86356| 81038|     0|     0|     0|     0     0     0  3780  1538     0|     0|     0
134  4170|  2876|     0|     0|     0|     0     0     0   677   617     0|     0|     0
135   208|    70|     0|     0|   131|     0     0     0     7     0     0|     0|     0
180  8029|  7675|     0|     3|    28|     0     0     0     0   296    27|     0|     0
181 23082|     0|     0|  2158| 20892|     0     0     0     0    32     0|     0|     0
183  3612|     0|     0|    44|  3568|     0     0     0     0     0     0|     0|     0
187 42396|     0|     0|   931| 41446|     0     0     0     0    19     0|     0|     0

 HEIGHT

120 10060|  9935|     0|     0|     0|     0     0     0    31    94     0|     0|     0
126  1169|  1169|     0|     0|     0|     0     0     0     0     0     0|     0|     0
130   602|   602|     0|     0|     0|     0     0     0     0     0     0|     0|     0
131 10321| 10321|     0|     0|     0|     0     0     0     0     0     0|     0|     0
133 86356| 86356|     0|     0|     0|     0     0     0     0     0     0|     0|     0
134  4170|  4170|     0|     0|     0|     0     0     0     0     0     0|     0|     0
135   208|   208|     0|     0|     0|     0     0     0     0     0     0|     0|     0
180  9296|  9026|     0|     0|     0|     0     0     0     0   203    67|     0|     0
181 23224| 21295|     0|     0|     0|     0     0     0     0    90  1839|     0|     0
183  3626|  3590|     0|     0|     0|     0     0     0     0    34     2|     0|     0
187 42698| 42411|     0|     0|     0|     0     0     0     0   286     1|     0|     0
220 10750| 10750|     0|     0|     0|     0     0     0     0     0     0|     0|     0
221   570|   570|     0|     0|     0|     0     0     0     0     0     0|     0|     0
224 16527| 16527|     0|     0|     0|     0     0     0     0     0     0|     0|     0
229     4|     4|     0|     0|     0|     0     0     0     0     0     0|     0|     0
230   166|   166|     0|     0|     0|     0     0     0     0     0     0|     0|     0
231 10319| 10319|     0|     0|     0|     0     0     0     0     0     0|     0|     0
233 86112| 86112|     0|     0|     0|     0     0     0     0     0     0|     0|     0
234  4170|  4170|     0|     0|     0|     0     0     0     0     0     0|     0|     0
235   208|   208|     0|     0|     0|     0     0     0     0     0     0|     0|     0
242 27875| 25521|     0|     0|     0|     0     0     0  2354     0     0|     0|     0
243  7040|  7040|     0|     0|     0|     0     0     0     0     0     0|     0|     0
250 58010| 52503|     0|     0|     0|     0     0     0  5507     0     0|     0|     0
252 53788| 48854|     0|     0|     0|     0     0     0  4934     0     0|     0|     0
253 13601| 13601|     0|     0|     0|     0     0     0     0     0     0|     0|     0
254 34156| 34156|     0|     0|     0|     0     0     0     0     0     0|     0|     0
257  8139|  8139|     0|     0|     0|     0     0     0     0     0     0|     0|     0
258  4127|  4127|     0|     0|     0|     0     0     0     0     0     0|     0|     0
259  5208|  5208|     0|     0|     0|     0     0     0     0     0     0|     0|     0

 WIND COMPONENTS

220 39385| 38684|     0|     2|     0|     0     0     0     0   696     3|     0|     0
221   720|   686|     0|     0|     0|     0     0     0     0    33     1|     0|     0
224 16527| 13837|     0|     0|     0|     0     0     0  2690     0     0|     0|     0
229     4|     4|     0|     0|     0|     0     0     0     0     0     0|     0|     0
230   166|   148|     0|     0|     0|     0     0     0     6    12     0|     0|     0
231 10319|  9346|     0|     0|     0|     0     0     0   914    59     0|     0|     0
233 86112| 81300|     0|     0|     0|     0     0     0  4185   627     0|     0|     0
234  4170|  1051|     0|     0|     0|     0     0     0  1419  1700     0|     0|     0
235   208|   200|     0|     0|     0|     0     0     0     8     0     0|     0|     0
242 27875| 25521|     0|     0|     0|     0     0     0  2354     0     0|     0|     0
243  7040|  7040|     0|     0|     0|     0     0     0     0     0     0|     0|     0
250 58010| 52503|     0|     0|     0|     0     0     0  5507     0     0|     0|     0
252 53788| 48854|     0|     0|     0|     0     0     0  4934     0     0|     0|     0
253 13601| 13601|     0|     0|     0|     0     0     0     0     0     0|     0|     0
254 34156| 34156|     0|     0|     0|     0     0     0     0     0     0|     0|     0
257  8139|  8139|     0|     0|     0|     0     0     0     0     0     0|     0|     0
258  4127|  4115|     0|     0|    12|     0     0     0     0     0     0|     0|     0
259  5208|  5050|     0|     0|   158|     0     0     0     0     0     0|     0|     0
280  7923|  7826|     0|    30|     0|     0     0     0     0    12    55|     0|     0
281 22659|     0|     0|   190| 20673|     0     0     0     0  1796     0|     0|     0
282   192|   192|     0|     0|     0|     0     0     0     0     0     0|     0|     0
284  3540|     0|     0|    24|  3484|     0     0     0     0    32     0|     0|     0
287 40070|     0|     0|    59| 39770|     0     0     0     0   241     0|     0|     0
290231701|231701|     0|     0|     0|     0     0     0     0     0     0|     0|     0

 PRECIPITABLE H2O

153   644|     0|     0|     0|   644|     0     0     0     0     0     0|     0|     0

 RELATIVE HUMIDTY


 ******CMPBQM PROCESSED         7066  BUFR RECORDS******
```
---
