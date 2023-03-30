
# DX BUFR Tables
@anchor dfbftab

<div id="bftab">
This document describes the format and contents of a DX BUFR tables file for use
with the BUFRLIB software.  Any such file consists of three distinct sections [Section 1](#section1),
[Section 2](#section2) and [Section 3](#section3), each of which is described in
further detail below.
For the purposes of this tutorial, we'll use the following sample DX BUFR tables file
and refer back to it at several points throughout the remainder of the discussion:

## Sample DX BUFR tables file

<pre>
.------------------------------------------------------------------------------.
| ------------   USER DEFINITIONS FOR TABLE-A TABLE-B TABLE D   -------------- |
|------------------------------------------------------------------------------|
| MNEMONIC | NUMBER | DESCRIPTION                                              |
|----------|--------|----------------------------------------------------------|
|          |        |                                                          |
| NC001003 | A63212 | MESSAGE TYPE 001-003  SURFACE MARINE FIXED BUOY          |
|          |        |                                                          |
| NC002001 | A63218 | MESSAGE TYPE 002-001  RAWINSONDE - FIXED LAND            |
|          |        |                                                          |
| NC002007 | A63223 | MESSAGE TYPE 002-007  WIND PROFILER                      |
|          |        |                                                          |
| TBLAEX1  | A58251 | TABLE A CONTRIVED EXAMPLE #1                             |
| TBLAEX2  | A58252 | TABLE A CONTRIVED EXAMPLE #2                             |
| TBLAEX3  | A58253 | TABLE A CONTRIVED EXAMPLE #3                             |
| TBLAEX4  | A58254 | TABLE A CONTRIVED EXAMPLE #4                             |
|          |        |                                                          |
| PRGPTMDP | 303003 | GEOPOTENTIAL/TEMPERATURE/DEWPOINT AT PRESSURE LEVEL      |
|          |        |                                                          |
| RPSEC1   | 361036 | SYNOPTIC REPORT WMO FM 12/13 SECTION 1 DATA              |
| TMPSQ1   | 361037 | SYNOPTIC REPORT TEMPERATURE DATA                         |
| TMPSQ2   | 361038 | SYNOPTIC REPORT WET BULB TEMPERATURE DATA                |
| TMPSQ3   | 361039 | SYNOPTIC REPORT MAXIMUM AND MINIMUM TEMPERATURE DATA     |
| WNDSQ1   | 361042 | SYNOPTIC REPORT WIND DATA                                |
| WNDSQ2   | 361043 | SYNOPTIC REPORT HIGHEST WIND GUST DATA                   |
| WNDSQ3   | 361044 | SYNOPTIC REPORT PEAK WIND DATA                           |
| PRSSQ1   | 361045 | SYNOPTIC REPORT PRESSURE DATA                            |
| PCPSQ2   | 361049 | SYNOPTIC REPORT PRECIPITATION DATA 2                     |
| PCPSQ3   | 361050 | SYNOPTIC REPORT PRECIPITATION DATA 3                     |
| WAVSQ1   | 361051 | SYNOPTIC REPORT INSTRUMENT WAVE DATA                     |
| WAVSQ2   | 361052 | SYNOPTIC REPORT WIND WAVE DATA                           |
| WAVSQ3   | 361053 | SYNOPTIC REPORT SWELL WAVE DATA                          |
| PPWSQ1   | 361054 | SYNOPTIC REPORT PRESENT AND PAST WEATHER DATA            |
| MRPSC0   | 361076 | MARINE REPORT WMO FM 13 SECTION 0 DATA                   |
| MRPIDS   | 361077 | MARINE REPORT IDS                                        |
| ID1SQ    | 361078 | SHIP'S CALL SIGN SEQUENCE                                |
| ID2SQ    | 361079 | NUMERIC BUOY PLATFORM ID                                 |
| ID3SQ    | 361080 | CMAN / FIXED BUOY PLATFORM ID                            |
| CTWNDS   | 361083 | CONTINUOUS WIND DATA                                     |
| MPCPSQ   | 361085 | MARINE REPORT PRECIPITATION DATA                         |
|          |        |                                                          |
| UASID    | 361121 | RADIOSONDE STATION ID DATA                               |
| UARID    | 361122 | RADIOSONDE REPORT ID DATA                                |
| UARLV    | 361123 | RADIOSONDE REPORT LEVEL DATA                             |
| UATMP    | 361125 | RADIOSONDE TEMPERATURE DATA                              |
| UAWND    | 361126 | RADIOSONDE WIND DATA                                     |
| UAWSH    | 361127 | RADIOSONDE WIND SHEAR DATA                               |
| UACLD    | 361128 | RADIOSONDE CLOUD DATA                                    |
| UASDG    | 361129 | RADIOSONDE SOUNDING SYSTEM DATA                          |
| UAADF    | 361130 | RADIOSONDE 101AA "ADDITIONAL DATA" DATA                  |
| UARDCS   | 361131 | RADIOSONDE REPORT DIAGNOSTIC DATA                        |
| UARTM    | 361132 | RADIOSONDE REPORT TIME DATA                              |
| UAGP07   | 361133 | RADIOSONDE CLASS 7 GEOPOTENTIAL DATA                     |
| UAGP10   | 361134 | RADIOSONDE CLASS 10 GEOPOTENTIAL DATA                    |
|          |        |                                                          |
| OBSEQ    | 361161 | PROFILER OBSERVATION SEQUENCE                            |
|          |        |                                                          |
| BID      | 363001 | BULLETIN ID DATA                                         |
| RAWRPT   | 363002 | RAW REPORT                                               |
| RCPTIM   | 363003 | REPORT RECEIPT TIME DATA                                 |
|          |        |                                                          |
| WMOB     | 001001 | WMO BLOCK NUMBER                                         |
| WMOS     | 001002 | WMO STATION NUMBER                                       |
| WMOR     | 001003 | WMO REGION NUMBER                                        |
| BPID     | 001005 | BUOY/PLATFORM IDENTIFIER                                 |
| SBPI     | 001010 | STATIONARY BUOY PLATFORM ID                              |
| ICLI     | 001063 | ICAO LOCATION IDENTIFIER                                 |
| SHPC8    | 001166 | SHIP CALL SIGN (8 CHARACTERS)                            |
| UAPART   | 001192 | RADIOSONDE PART NAME                                     |
| BUHD     | 001194 | BULLETIN HEADER                                          |
| RSML     | 001197 | RADIOSONDE SHIP, DROP, OR MOBIL STATION ID               |
| RPID     | 001198 | REPORT IDENTIFIER                                        |
|          |        |                                                          |
| TOST     | 002001 | TYPE OF STATION                                          |
| A4ME     | 002003 | TYPE OF MEASURING EQUIPMENT USED                         |
| RATP     | 002011 | RADIOSONDE TYPE                                          |
| SIRC     | 002013 | SOLAR AND INFRARED RADIATION CORRECTION                  |
| TTSS     | 002014 | TRACKING TECHNIQUE/STATUS OF SYSTEM USED                 |
| MSST     | 002038 | METHOD OF SEA SURFACE TEMPERATURE MEASUREMENT            |
| MWBT     | 002039 | METHOD OF WET BULB TEMPERATURE MEASUREMENT               |
| ITSO     | 002193 | IND TYPE OF STATION OPERATION PAST/PRESENT WEATHER       |
| SEQNUM   | 002195 | LDM CHANNEL SEQUENCE NUMBER                              |
|          |        |                                                          |
| YEAR     | 004001 | YEAR                                                     |
| MNTH     | 004002 | MONTH                                                    |
| DAYS     | 004003 | DAY                                                      |
| HOUR     | 004004 | HOUR                                                     |
| MINU     | 004005 | MINUTE                                                   |
| TPMI     | 004025 | TIME PERIOD OR DISPLACEMENT                              |
| .DTH.... | 004031 | DURATION OF TIME IN HOURS RELATING TO FOLLOWING VALUE    |
| .DTM.... | 004032 | DURATION OF TIME IN MINUTES RELATING TO FOLLOWING VALUE  |
| RCYR     | 004200 | YEAR   - TIME OF RECEIPT                                 |
| RCMO     | 004201 | MONTH  - TIME OF RECEIPT                                 |
| RCDY     | 004202 | DAY    - TIME OF RECEIPT                                 |
| RCHR     | 004203 | HOUR   - TIME OF RECEIPT                                 |
| RCMI     | 004204 | MINUTE - TIME OF RECEIPT                                 |
| UALNHR   | 004210 | RADIOSONDE LAUNCH HOUR                                   |
| UALNMN   | 004211 | RADIOSONDE LAUNCH MINUTE                                 |
|          |        |                                                          |
| CLAT     | 005002 | LATITUDE (COARSE ACCURACY)                               |
|          |        |                                                          |
| CLON     | 006002 | LONGITUDE (COARSE ACCURACY)                              |
|          |        |                                                          |
| SELV     | 007001 | HEIGHT OF STATION                                        |
| PRLC     | 007004 | PRESSURE                                                 |
| HINC     | 007005 | HEIGHT INCREMENT                                         |
| HEIT     | 007007 | HEIGHT                                                   |
| GP07     | 007008 | GEOPOTENTIAL                                             |
| XMPRLC   | 007195 | EXTRAPOLATED MANDATORY LEVEL PRESSURE                    |
|          |        |                                                          |
| VSIG     | 008001 | VERTICAL SOUNDING SIGNIFICANCE                           |
| TSIG     | 008021 | TIME SIGNIFICANCE                                        |
| ACAV     | 008022 | TOTAL NUMBER (WITH RESPECT TO ACCUMULATION OR AVERAGE)   |
| SUWS     | 008199 | SOURCE UNITS OF WIND SPEED                               |
| RCTS     | 008202 | RECEIPT TIME SIGNIFICANCE                                |
|          |        |                                                          |
| GEOP     | 010003 | GEOPOTENTIAL                                             |
| PRES     | 010004 | PRESSURE                                                 |
| GP10     | 010008 | GEOPOTENTIAL                                             |
| PMSL     | 010051 | PRESSURE REDUCED TO MSL                                  |
| 3HPC     | 010061 | 3 HOUR PRESSURE CHANGE                                   |
| CHPT     | 010063 | CHARACTERISTIC OF PRESSURE TENDENCY                      |
| XMGP10   | 010196 | EXTRAPOLATED MANDATORY LEVEL GEOPOTENTIAL                |
|          |        |                                                          |
| WDIR     | 011001 | WIND DIRECTION                                           |
| WSPD     | 011002 | WIND SPEED                                               |
| UCMP     | 011003 | U-COMPONENT                                              |
| VCMP     | 011004 | V-COMPONENT                                              |
| WCMP     | 011006 | W-COMPONENT                                              |
| MXGS     | 011041 | MAXIMUM WIND SPEED (GUSTS)                               |
| MWDL     | 011044 | MEAN WIND DIRECTION FOR SURFACE-1500M LAYER              |
| MWSL     | 011045 | MEAN WIND SPEED FOR SURFACE-1500M LAYER                  |
| SDHS     | 011050 | STANDARD DEVIATION OF HORIZONTAL WIND SPEED              |
| SDVS     | 011051 | STANDARD DEVIATION OF VERTICAL WIND SPEED                |
| AWSB     | 011061 | ABSOLUTE WIND SHEAR IN 1 KM LAYER BELOW                  |
| AWSA     | 011062 | ABSOLUTE WIND SHEAR IN 1 KM LAYER ABOVE                  |
| PKWDDR   | 011202 | PEAK WIND DIRECTION                                      |
| PKWDSP   | 011203 | PEAK WIND SPEED                                          |
| XS10     | 011223 | 10 METER EXTRAPOLATED WIND SPEED                         |
| XS20     | 011224 | 20 METER EXTRAPOLATED WIND SPEED                         |
| MWDH     | 011221 | MEAN WIND DIRECTION FOR 1500M-3000M LAYER                |
| MWSH     | 011222 | MEAN WIND SPEED FOR 1500M-3000M LAYER                    |
| WDRC     | 011227 | CONTINUOUS WIND DIRECTION IN DEGREES                     |
| WDSC     | 011228 | CONTINUOUS WIND SPEED IN M/SEC                           |
|          |        |                                                          |
| TMDBST   | 012001 | TEMPERATURE/DRY BULB TEMPERATURE (SCALE 1)               |
| TMDB     | 012101 | TEMPERATURE/DRY BULB TEMPERATURE                         |
| TMWB     | 012102 | WET BULB TEMPERATURE                                     |
| TMDP     | 012103 | DEW POINT TEMPERATURE                                    |
| MXTM     | 012228 | MAXIMUM TEMPERATURE                                      |
| MITM     | 012229 | MINIMUM TEMPERATURE                                      |
|          |        |                                                          |
| REHU     | 013003 | RELATIVE HUMIDITY                                        |
| TOPC     | 013011 | TOTAL PRECIPITATION/TOTAL WATER EQUIVALENT               |
| REQV     | 013014 | RAINFALL/WATER EQUIVALENT OF SNOW (AVERAGE RATE)         |
| TP01     | 013019 | TOTAL PRECIPITATION PAST 1 HOUR                          |
| TP03     | 013020 | TOTAL PRECIPITATION PAST 3 HOURS                         |
| TP06     | 013021 | TOTAL PRECIPITATION PAST 6 HOURS                         |
| TP12     | 013022 | TOTAL PRECIPITATION PAST 12 HOURS                        |
| TP24     | 013023 | TOTAL PRECIPITATION PAST 24 HOURS                        |
| INPC     | 013194 | INDIC INCLUSION/OMISSION OF PREC                         |
| STBS5    | 013195 | MODIFIED SHOWALTER STABILITY INDEX                       |
|          |        |                                                          |
| HOVI     | 020001 | HORIZONTAL VISIBILITY                                    |
| PRWE     | 020003 | PRESENT WEATHER                                          |
| PSW1     | 020004 | PAST WEATHER (1)                                         |
| PSW2     | 020005 | PAST WEATHER (2)                                         |
| CLAM     | 020011 | CLOUD AMOUNT                                             |
| CLTP     | 020012 | CLOUD TYPE                                               |
| HOCB     | 020013 | HEIGHT OF BASE OF CLOUD                                  |
|          |        |                                                          |
| SPP01    | 021193 | SPECTRAL PEAK POWER 0TH MOMENT                           |
|          |        |                                                          |
| DOSW     | 022003 | DIRECTION OF SWELL WAVES                                 |
| POWV     | 022011 | PERIOD OF WAVES                                          |
| POWW     | 022012 | PERIOD OF WIND WAVES                                     |
| POSW     | 022013 | PERIOD OF SWELL WAVES                                    |
| HOWV     | 022021 | HEIGHT OF WAVES                                          |
| HOWW     | 022022 | HEIGHT OF WIND WAVES                                     |
| HOSW     | 022023 | HEIGHT OF SWELL WAVES                                    |
| SST1     | 022043 | SEA TEMPERATURE                                          |
|          |        |                                                          |
| NPHL     | 025032 | WIND PROFILER MODE INFORMATION                           |
| NPSM     | 025033 | WIND PROFILER SUBMODE INFORMATION                        |
| NPQC     | 025034 | WIND PROFILER QUALITY CONTROL TEST RESULTS               |
|          |        |                                                          |
| QCEVR    | 033024 | STATION ELEVATION QUALITY MARK (FOR MOBIL STATIONS)      |
| QMGP     | 033192 | ON29 QUALITY MARK FOR GEOPOTENTIAL                       |
| QMAT     | 033193 | ON29 QUALITY MARK FOR TEMPERATURE                        |
| QMDD     | 033194 | ON29 QUALITY MARK FOR DEWPOINT DEPRESSION                |
| QMWN     | 033195 | ON29 QUALITY MARK FOR WIND                               |
| QMCA     | 033197 | ON29 QUALITY MARK FOR CLOUD AMOUNT                       |
| UARDC    | 033202 | RADIOSONDE REPORT DIAGNOSTIC CODE                        |
| QMPR     | 033207 | ON29 QUALITY MARK FOR PRESSURE                           |
| CORN     | 033215 | CORRECTED REPORT INDICATOR                               |
| QMST     | 033218 | ON29 QUALITY MARK FOR SEA SURFACE TEMPERATURE            |
|          |        |                                                          |
| BULTIM   | 058006 | BULLETIN TIME (DDHHMM)                                   |
| BBB      | 058007 | WMO BBB INDICATOR                                        |
| RRSTG    | 058008 | RAW REPORT STRING                                        |
|          |        |                                                          |
|------------------------------------------------------------------------------|
| MNEMONIC | SEQUENCE                                                          |
|----------|-------------------------------------------------------------------|
|          |                                                                   |
| NC001003 | MRPSC0  BID    {RCPTIM} RPSEC1  WNDSQ1  XS10    XS20     TMPSQ1   |
| NC001003 | PRSSQ1  MPCPSQ                                                    |
| NC001003 | &lt;PPWSQ1&gt;   &lt;WAVSQ1&gt;   &lt;WAVSQ2&gt;  {WAVSQ3}    WNDSQ3   {CTWNDS}     |
| NC001003 | {RAWRPT}                                                          |
|          |                                                                   |
| NC002001 | UARTM  {RCPTIM}  {BID}  UASID  {UARID}                            |
| NC002001 | {UARLV}  &lt;UASDG&gt;  {UARDCS}  {RAWRPT}                              |
| NC002001 | {UACLD}  &lt;UAADF&gt;                                                  |
| NC002001 | WMOB  WMOS  WMOR                                                  |
|          |                                                                   |
| NC002007 | WMOB     WMOS     CLAT     CLON     SELV     YEAR     MNTH        |
| NC002007 | DAYS     HOUR     MINU     TSIG     TPMI     WSPD     WDIR        |
| NC002007 | PMSL     TMDBST   REQV     REHU                                   |
| NC002007 | 201127   HINC     201000   NPSM                                   |
| NC002007 | 201132   HINC     201000   "OBSEQ"36                              |
| NC002007 | 201132   HINC     201000   "OBSEQ"7                               |
|          |                                                                   |
| TBLAEX1  | UASID  UARTM  {PRGPTMDP}                                          |
|          |                                                                   |
| TBLAEX2  | UASID  UARTM  "PRGPTMDP"100                                       |
|          |                                                                   |
| TBLAEX3  | UASID  UARTM  {PRGPTMDP}  PRLC  UACLD  PRLC  HOVI                 |
|          |                                                                   |
| TBLAEX4  | UASID  UARTM                                                      |
| TBLAEX4  | PRLC  UACLD GEOP  TMDB  HOVI TMDP                                 |
| TBLAEX4  | PRLC  UACLD TMDB  TMDP  HOVI GEOP                                 |
| TBLAEX4  | PRLC  UACLD GEOP  TMDP  HOVI TMDB                                 |
| TBLAEX4  | PRLC  UACLD TMDB  GEOP  HOVI TMDP                                 |
|          |                                                                   |
| PRGPTMDP | PRLC  GEOP  TMDB  TMDP                                            |
|          |                                                                   |
| RPSEC1   | ITSO    TOST    INPC    HOVI                                      |
|          |                                                                   |
| TMPSQ1   | QMAT    TMDB    QMDD    TMDP    MSST   QMST  SST1                 |
| TMPSQ1   | &lt;TMPSQ2&gt;   &lt;TMPSQ3&gt;                                               |
|          |                                                                   |
| TMPSQ2   | MWBT    TMWB    REHU                                              |
|          |                                                                   |
| TMPSQ3   | .DTHMXTM   MXTM   .DTHMITM   MITM                                 |
|          |                                                                   |
| WNDSQ1   | SUWS    QMWN    WDIR    WSPD    &lt;WNDSQ2&gt;                          |
|          |                                                                   |
| WNDSQ2   | .DTMMXGS  MXGS                                                    |
|          |                                                                   |
| WNDSQ3   | YEAR    MNTH    DAYS    HOUR    MINU    PKWDDR    PKWDSP          |
|          |                                                                   |
| WAVSQ1   | POWV    HOWV                                                      |
|          |                                                                   |
| WAVSQ2   | POWW    HOWW                                                      |
|          |                                                                   |
| WAVSQ3   | DOSW    POSW    HOSW                                              |
|          |                                                                   |
| PRSSQ1   | QMPR  PRES  PMSL  CHPT  3HPC                                      |
|          |                                                                   |
| PCPSQ2   | TP01    TP03    TP12    TP24                                      |
|          |                                                                   |
| PCPSQ3   | .DTHTOPC   TOPC                                                   |
|          |                                                                   |
| PPWSQ1   | PRWE    PSW1    PSW2                                              |
|          |                                                                   |
| MRPSC0   | YEAR    MNTH    DAYS    HOUR    MINU    RPID   MRPIDS CLON        |
| MRPSC0   | CLAT    SELV    CORN                                              |
|          |                                                                   |
| MRPIDS   | &lt;ID1SQ&gt; &lt;ID2SQ&gt; &lt;ID3SQ&gt;                                           |
|          |                                                                   |
| ID1SQ    | SHPC8                                                             |
|          |                                                                   |
| ID2SQ    | BPID                                                              |
|          |                                                                   |
| ID3SQ    | SBPI                                                              |
|          |                                                                   |
| CTWNDS   | TPMI    WDRC    WDSC                                              |
|          |                                                                   |
| MPCPSQ   | TP06    &lt;PCPSQ2&gt;   &lt;PCPSQ3&gt;                                       |
|          |                                                                   |
| BID      | SEQNUM  BUHD  ICLI  BULTIM  BBB                                   |
|          |                                                                   |
| RAWRPT   | RRSTG                                                             |
|          |                                                                   |
| RCPTIM   | RCTS  RCYR  RCMO  RCDY  RCHR  RCMI                                |
|          |                                                                   |
| UASID    | RPID  CLAT  CLON  SELV                                            |
|          |                                                                   |
| UARID    | RATP  A4ME  CORN  UAPART                                          |
|          |                                                                   |
| UARTM    | YEAR  MNTH  DAYS  HOUR                                            |
|          |                                                                   |
| UARLV    | VSIG  QMPR  PRLC  QMGP  &lt;UAGP07&gt;  &lt;UAGP10&gt;                        |
| UARLV    | &lt;UATMP&gt;  &lt;UAWND&gt;  &lt;UAWSH&gt;                                         |
|          |                                                                   |
| UAGP07   | GP07                                                              |
|          |                                                                   |
| UAGP10   | GP10                                                              |
|          |                                                                   |
| UATMP    | QMAT  TMDB  QMDD  TMDP                                            |
|          |                                                                   |
| UAWND    | QMWN  WDIR  WSPD                                                  |
|          |                                                                   |
| UAWSH    | AWSB  AWSA                                                        |
|          |                                                                   |
| UACLD    | CLTP  QMCA  CLAM  HOCB                                            |
|          |                                                                   |
| UASDG    | QMST  SST1  SIRC  TTSS  UALNHR  UALNMN                            |
|          |                                                                   |
| UAADF    | MWDL  MWSL  MWDH  MWSH  STBS5  XMPRLC  XMGP10                     |
|          |                                                                   |
| UARDCS   | UARDC                                                             |
|          |                                                                   |
| OBSEQ    | NPHL     NPQC                                                     |
| OBSEQ    | 201116   ACAV   201000   UCMP    VCMP   201127   SDHS  201000     |
| OBSEQ    | 201116   ACAV   201000   SPP01   WCMP   SDVS                      |
|          |                                                                   |
|------------------------------------------------------------------------------|
| MNEMONIC | SCAL | REFERENCE   | BIT | UNITS                    |-------------|
|----------|------|-------------|-----|--------------------------|-------------|
|          |      |             |     |                          |-------------|
| WMOB     |    0 |           0 |   7 | NUMERIC                  |-------------|
| WMOS     |    0 |           0 |  10 | NUMERIC                  |-------------|
| WMOR     |    0 |           0 |   3 | CODE TABLE               |-------------|
| BPID     |    0 |           0 |  17 | NUMERIC                  |-------------|
| SBPI     |    0 |           0 |  64 | CCITT IA5                |-------------|
| ICLI     |    0 |           0 |  64 | CCITT IA5                |-------------|
| SHPC8    |    0 |           0 |  64 | CCITT IA5                |-------------|
| UAPART   |    0 |           0 |  32 | CCITT IA5                |-------------|
| BUHD     |    0 |           0 |  64 | CCITT IA5                |-------------|
| RSML     |    0 |           0 |  64 | CCITT IA5                |-------------|
| RPID     |    0 |           0 |  64 | CCITT IA5                |-------------|
|          |      |             |     |                          |-------------|
| TOST     |    0 |           0 |   2 | CODE TABLE               |-------------|
| A4ME     |    0 |           0 |   4 | CODE TABLE               |-------------|
| RATP     |    0 |           0 |   8 | CODE TABLE               |-------------|
| SIRC     |    0 |           0 |   4 | CODE TABLE               |-------------|
| TTSS     |    0 |           0 |   7 | CODE TABLE               |-------------|
| MSST     |    0 |           0 |   3 | CODE TABLE               |-------------|
| MWBT     |    0 |           0 |   3 | CODE TABLE               |-------------|
| ITSO     |    0 |           0 |   3 | CODE TABLE               |-------------|
| SEQNUM   |    0 |           0 |  32 | CCITT IA5                |-------------|
|          |      |             |     |                          |-------------|
| YEAR     |    0 |           0 |  12 | YEAR                     |-------------|
| MNTH     |    0 |           0 |   4 | MONTH                    |-------------|
| DAYS     |    0 |           0 |   6 | DAY                      |-------------|
| HOUR     |    0 |           0 |   5 | HOUR                     |-------------|
| MINU     |    0 |           0 |   6 | MINUTE                   |-------------|
| TPMI     |    0 |       -2048 |  12 | MINUTES                  |-------------|
| .DTH.... |    0 |           0 |   8 | HOURS                    |-------------|
| .DTM.... |    0 |           0 |   6 | MINUTES                  |-------------|
| RCYR     |    0 |           0 |  12 | YEAR                     |-------------|
| RCMO     |    0 |           0 |   4 | MONTH                    |-------------|
| RCDY     |    0 |           0 |   6 | DAY                      |-------------|
| RCHR     |    0 |           0 |   5 | HOUR                     |-------------|
| RCMI     |    0 |           0 |   6 | MINUTE                   |-------------|
| UALNHR   |    0 |           0 |   5 | HOUR                     |-------------|
| UALNMN   |    0 |           0 |   6 | MINUTE                   |-------------|
|          |      |             |     |                          |-------------|
| CLAT     |    2 |       -9000 |  15 | DEGREES                  |-------------|
|          |      |             |     |                          |-------------|
| CLON     |    2 |      -18000 |  16 | DEGREES                  |-------------|
|          |      |             |     |                          |-------------|
| SELV     |    0 |        -400 |  15 | METERS                   |-------------|
| PRLC     |   -1 |           0 |  14 | PASCALS                  |-------------|
| HEIT     |    0 |       -1000 |  17 | METERS                   |-------------|
| GP07     |    0 |      -10000 |  20 | (METERS/SECOND)**2       |-------------|
| HINC     |    0 |        -400 |  12 | METERS                   |-------------|
| XMPRLC   |   -1 |           0 |  14 | PASCALS                  |-------------|
|          |      |             |     |                          |-------------|
| VSIG     |    0 |           0 |   7 | FLAG TABLE               |-------------|
| TSIG     |    0 |           0 |   5 | CODE TABLE               |-------------|
| SUWS     |    0 |           0 |   3 | CODE TABLE               |-------------|
| RCTS     |    0 |           0 |   6 | CODE TABLE               |-------------|
| ACAV     |    0 |           0 |  16 | NUMERIC                  |-------------|
|          |      |             |     |                          |-------------|
| GEOP     |   -1 |        -400 |  17 | (METERS/SECOND)**2       |-------------|
| PRES     |   -1 |           0 |  14 | PASCALS                  |-------------|
| GP10     |    0 |      -10000 |  20 | (METERS/SECOND)**2       |-------------|
| PMSL     |   -1 |           0 |  14 | PASCALS                  |-------------|
| CHPT     |    0 |           0 |   4 | CODE TABLE               |-------------|
| 3HPC     |   -1 |        -500 |  10 | PASCALS                  |-------------|
| XMGP10   |    0 |      -10000 |  20 | (METERS/SECOND)**2       |-------------|
|          |      |             |     |                          |-------------|
| WDIR     |    0 |           0 |   9 | DEGREES TRUE             |-------------|
| WSPD     |    1 |           0 |  12 | METERS/SECOND            |-------------|
| UCMP     |    1 |       -4096 |  13 | METERS/SECOND            |-------------|
| VCMP     |    1 |       -4096 |  13 | METERS/SECOND            |-------------|
| WCMP     |    2 |       -4096 |  13 | METERS/SECOND            |-------------|
| MXGS     |    1 |           0 |  12 | METERS/SECOND            |-------------|
| MWDL     |    0 |           0 |   9 | DEGREES TRUE             |-------------|
| MWSL     |    1 |           0 |  12 | METERS/SECOND            |-------------|
| SDHS     |    1 |           0 |  12 | METERS/SECOND            |-------------|
| SDVS     |    1 |           0 |   8 | METERS/SECOND            |-------------|
| AWSB     |    1 |           0 |  12 | METERS/SECOND            |-------------|
| AWSA     |    1 |           0 |  12 | METERS/SECOND            |-------------|
| PKWDDR   |    0 |           0 |   9 | DEGREES TRUE             |-------------|
| PKWDSP   |    1 |           0 |  12 | METERS/SECOND            |-------------|
| XS10     |    1 |           0 |  12 | METERS/SECOND            |-------------|
| XS20     |    1 |           0 |  12 | METERS/SECOND            |-------------|
| MWDH     |    0 |           0 |   9 | DEGREES TRUE             |-------------|
| MWSH     |    1 |           0 |  12 | METERS/SECOND            |-------------|
| WDRC     |    0 |           0 |   9 | DEGREES TRUE             |-------------|
| WDSC     |    1 |           0 |  12 | METERS/SECOND            |-------------|
|          |      |             |     |                          |-------------|
| TMDBST   |    1 |           0 |  12 | DEGREES KELVIN           |-------------|
| TMDB     |    2 |           0 |  16 | DEGREES KELVIN           |-------------|
| TMWB     |    2 |           0 |  16 | DEGREES KELVIN           |-------------|
| TMDP     |    2 |           0 |  16 | DEGREES KELVIN           |-------------|
| MXTM     |    2 |           0 |  16 | DEGREES KELVIN           |-------------|
| MITM     |    2 |           0 |  16 | DEGREES KELVIN           |-------------|
|          |      |             |     |                          |-------------|
| REHU     |    0 |           0 |   7 | %                        |-------------|
| TOPC     |    1 |          -1 |  14 | KG/METER**2              |-------------|
| REQV     |    4 |           0 |  12 | KG/((METERS**2)*SECONDS) |-------------|
| TP01     |    1 |          -1 |  14 | KG/METER**2              |-------------|
| TP03     |    1 |          -1 |  14 | KG/METER**2              |-------------|
| TP06     |    1 |          -1 |  14 | KG/METER**2              |-------------|
| TP12     |    1 |          -1 |  14 | KG/METER**2              |-------------|
| TP24     |    1 |          -1 |  14 | KG/METER**2              |-------------|
| INPC     |    0 |           0 |   3 | CODE TABLE               |-------------|
| STBS5    |    0 |         -40 |   8 | NUMERIC                  |-------------|
|          |      |             |     |                          |-------------|
| HOVI     |   -1 |           0 |  13 | METERS                   |-------------|
| PRWE     |    0 |           0 |   9 | CODE TABLE               |-------------|
| PSW1     |    0 |           0 |   5 | CODE TABLE               |-------------|
| PSW2     |    0 |           0 |   5 | CODE TABLE               |-------------|
| CLAM     |    0 |           0 |   4 | CODE TABLE               |-------------|
| CLTP     |    0 |           0 |   6 | CODE TABLE               |-------------|
| HOCB     |   -1 |         -40 |  11 | METERS                   |-------------|
|          |      |             |     |                          |-------------|
| SPP01    |    0 |           0 |   8 | DB                       |-------------|
|          |      |             |     |                          |-------------|
| DOSW     |    0 |           0 |   9 | DEGREES TRUE             |-------------|
| POWV     |    0 |           0 |   6 | SECONDS                  |-------------|
| POWW     |    0 |           0 |   6 | SECONDS                  |-------------|
| POSW     |    0 |           0 |   6 | SECONDS                  |-------------|
| HOWV     |    1 |           0 |  10 | METERS                   |-------------|
| HOWW     |    1 |           0 |  10 | METERS                   |-------------|
| HOSW     |    1 |           0 |  10 | METERS                   |-------------|
| SST1     |    2 |           0 |  15 | DEGREES KELVIN           |-------------|
|          |      |             |     |                          |-------------|
| NPHL     |    0 |           0 |   2 | CODE TABLE               |-------------|
| NPSM     |    0 |           0 |   2 | CODE TABLE               |-------------|
| NPQC     |    0 |           0 |   4 | FLAG TABLE               |-------------|
|          |      |             |     |                          |-------------|
| QCEVR    |    0 |           0 |   4 | CODE TABLE               |-------------|
| QMGP     |    0 |           0 |   4 | CODE TABLE               |-------------|
| QMAT     |    0 |           0 |   4 | CODE TABLE               |-------------|
| QMDD     |    0 |           0 |   4 | CODE TABLE               |-------------|
| QMWN     |    0 |           0 |   4 | CODE TABLE               |-------------|
| QMCA     |    0 |           0 |   1 | CODE TABLE               |-------------|
| UARDC    |    0 |           0 |   8 | CODE TABLE               |-------------|
| QMPR     |    0 |           0 |   4 | CODE TABLE               |-------------|
| CORN     |    0 |           0 |   3 | CODE TABLE               |-------------|
| QMST     |    0 |           0 |   4 | CODE TABLE               |-------------|
|          |      |             |     |                          |-------------|
| BULTIM   |    0 |           0 |  64 | CCITT IA5                |-------------|
| BBB      |    0 |           0 |  64 | CCITT IA5                |-------------|
| RRSTG    |    0 |           0 |  64 | CCITT IA5                |-------------|
|          |      |             |     |                          |-------------|
`------------------------------------------------------------------------------'
</pre>

<br>

As noted during the discussion of subroutine openbf(), every BUFR file that is presented to the
BUFRLIB software, either for input (reading/decoding) or output (writing/encoding) purposes, must
have DX BUFR tables associated with it, unless the 'SEC3' decoding option is specified during the
call to openbf(). For all other cases, DX table information must be pre-defined and made available
to the software via call argument LUNDX during the call to openbf(). In the case of an existing
BUFR file, the DX tables information may be embedded within the first few BUFR messages of the
file itself. Otherwise, a separate ASCII text file containing the necessary DX tables information
must be supplied, such as the example shown above. It is extremely important that any such file
not only be syntactically correct but also complete, in the sense that all necessary mnemonics
must exist and be fully-defined.

<br>

First, let's define what we mean by a <i>mnemonic</i>. In short, a mnemonic is simply a
descriptive, alphanumeric name for a data value. In the context of the BUFRLIB software, there
are "Table A mnemonics", which refer to particular data subset (i.e. report ) types, "Table B
mnemonics", which refer directly to basic data values, and "Table D mnemonics", which are
sequences composed of one or more Table B (or other Table D) mnemonics and which are themselves
normally direct constituents of a particular Table A mnemonic. In other words, at the highest level,
we have a Table A mnemonic which completely describes a type of data subset (e.g. rawinsonde,
wind profiler, etc.), and this Table A mnemonic is defined as a sequence of one or more Table B or
Table D mnemonics, where each Table D mnemonic is likewise itself defined as a sequence of one or
more Table B or Table D mnemonics, and so on until the entire data subset can be equivalently
described as a sequence of one or more Table B mnemonics which, again, themselves correspond to
basic data values (e.g. pressure, temperature, humidity, etc.). In this way, the entire sequence
of data values that constitute a particular type of data subset is fully and unambiguously defined,
both for purposes of input (reading/decoding) or output (writing/encoding) of reports corresponding
to that particular type of data subset.

<br>

However, it's also important to understand what mnemonics are not. Specifically, mnemonics never
themselves appear within actual BUFR messages that are read or written by the BUFRLIB software;
rather, their only purpose in life is to make it easier for users to interact with the software by
providing descriptive names to represent individual data values, as opposed to having to keep track
of the corresponding FXY numbers (described below), which are much less intuitive but which
nevertheless are the prescribed method within the BUFR code form for referencing of individual data
values, and which therefore are what are actually read and written by the software.

<br>

Before continuing on let's recall
that a DX BUFR tables file consists of three distinct sections.  Each section contains one or more
lines of 80 characters in length, and where a "*" as the first character of a line indicates that
that entire line is a comment. In the [first section](#section1), all Table A, B and D mnemonics that are to be
used within the file are initially declared, assigned a unique FXY number, and given a short,
free-form text description. Then, in the [second section](#section2), all previously-declared Table A and
Table D mnemonics are actually defined as a sequence of one or more Table B (or other Table D)
mnemonics. Finally, in the [third section](#section3), all previously-declared Table B mnemonics are defined
in terms of their scale factor, reference value, bit width, and units.

<br>

Now, as we delve into the details of each of the three sections, we'll constantly refer back to
our [sample DX BUFR tables file](#bftab) in order to better illustrate the concepts that are discussed.
</div>

<div id="section1">
## Section 1

As previously mentioned, the first section of a BUFR tables file is where all Table A, B and D
mnemonics are initially declared, assigned a unique FXY number, and given a short free-form text
description. Mnemonics may contain any combination of uppercase letters and numbers (or, in certain
special cases, a "." character!), up to a maximum total of 8 characters in length. A mnemonic may be
declared only once, and each one must correspond to a unique FXY number, which itself consists of 6
characters, and where the first character (i.e. the "F" component) is an "A" if the mnemonic is
being declared as a Table A mnemonic, "3" if the mnemonic is being declared as a Table D mnemonic,
and "0" if the mnemonic is being declared as a Table B mnemonic. Otherwise, the remainder of the FXY
number must be all digits, with the next 2 characters (i.e. the "X" component) as a number
between 00 and 63, and the final 3 characters (i.e. the "Y" component) as a number between 001
and 255. Readers who are more familiar with BUFR will immediately recognize these F, X, and Y
values as those that are defined within the
[official documentation of the BUFR code form](@ref manual);
therefore, by international convention, a mnemonic should not be given an X value between 00 and 47
along with a Y value between 001 and 191 unless that mnemonic, when subsequently defined, corresponds
exactly to the BUFR descriptor having that same FXY number within the
[official WMO master BUFR tables](@ref wmomstab). For example, in our [sample DX BUFR tables file](#bftab),
mnemonic "WMOB" is declared with an FXY number of 001001; therefore, it has the exact same text
description (i.e. "WMO BLOCK NUMBER") and, when later defined within the last section of the file,
the exact same scale factor, reference value, bit width, and units as for FXY number 001001 within
the official WMO master BUFR tables. This concept should be somewhat intuitive, but it's obviously very
important when the BUFRLIB software is to be used to encode BUFR messages that may potentially be
read by other users in other organizations around the world.

<br>

In looking further at our [sample DX BUFR tables file](#bftab), we see that the lines within the first
section each contain a "|" character in columns 1, 12, 21, and 80. Mnemonics are declared, and are
left-justified, in columns 3-10, corresponding FXY numbers are assigned in columns 14-19, and the
corresponding text description begins in column 23. All of the Table A mnemonics are declared first,
followed by all of the Table D mnemonics, followed by all of the Table B mnemonics. Within each set,
it is generally a good idea for human-readability purposes to list the mnemonics in ascending order
with respect to their FXY number, although this is not required by the BUFRLIB software itself.
Likewise, human-readability can usually also be improved by the judicious use of one or more
separator lines containing the required "|" character in columns 1, 12, 21, and 80 but without
any actual mnemonic declaration; however, again, the use of such separator lines is not required
by the software. In fact, the software will simply continue reading lines of the file, one at a time,
and looking for new mnemonic declarations, until it reaches a line which does not contain a "|"
character in each of columns 1, 12, 21, and 80, at which point it then knows that the first section
of the tables file has ended.

<br>

We mentioned earlier that mnemonics only exist in order to facilitate user interaction with the
BUFRLIB software and that, therefore, mnemonics should be as intuitive as possible. We now need
to amend that statement slightly, because certain Table A mnemonics do have a special additional
function. Specifically, if a Table A mnemonic consists of 8 characters (i.e. the maximum) and if
characters 3 through 8 are all digits, then the mnemonic is also used by the software to set the
data category and local subcategory within Section 1 of each BUFR message when writing/encoding
data subsets corresponding to that mnemonic. In such cases, characters 3 through 5 define the
category, and characters 6 through 8 define the subcategory. Therefore, in referring again to our
[sample DX BUFR tables file](#bftab) where we've defined three different Table A mnemonics,
we've also indicated that, e.g. when we use the software to write/encode data subsets according
to the Table A mnemonic "NC002007" (i.e. wind profiler), we want all BUFR messages which contain
such data subsets to be encoded as category 2 and local subcategory 7 within Section 1 of the
message.

<br>

Incidentally, even if a Table A mnemonic doesn't meet the above criteria, BUFR message category
and local subcategory values will still be set by the software when writing/encoding BUFR data
subsets corresponding to that Table A mnemonic. However, in such cases, the category value will
be set to the "Y" component (i.e. last 3 digits) of the FXY number corresponding to the mnemonic,
and the subcategory value will simply be set to 0. Therefore, it is recommended to use the
previous, more-explicit approach when assigning a Table A mnemonic for a data subset to be output,
since this approach provides for greater control over the category and subcategory values that
will be encoded into Section 1 of the resultant BUFR message. We should also take this opportunity
to point out that, when the FXY number corresponding to a Table A mnemonic is actually encoded
into a BUFR message, a "3" is actually encoded in place of the "A" which is used in the DX tables
file. Put another way, the "A" that appears within the FXY number corresponding to each Table A
mnemonic within the tables file is only there so that such mnemonics can be easily distinguished
from Table D mnemonics by the software.
</div>

<div id="section2">
## Section 2

Now, let's move on to the second section of a DX BUFR tables file. As already stated, this section
is used to define, for each Table A and Table D mnemonic that was previously declared in the
[first section](#section1), the sequence of Table B (and possibly other Table D) mnemonics which
constitutes that mnemonic. The format for this section is a "|" character in columns 1, 12, and 80,
with the mnemonic that is being defined listed in columns 3-10 (left-justified), and the sequence
of constituent mnemonics beginning in column 14, each one separated from the others by one or more
blank characters. For longer sequences, multiple successive lines may be used in a continuation
fashion by repeating, within columns 3-10 of each continuation line, the mnemonic being defined.
For example, in our [sample DX BUFR tables file](#bftab), the Table D mnemonic MRPSC0 is defined as consisting
of the sequence YEAR MNTH DAYS HOUR MINU RPID MRPIDS CLON CLAT SELV CORN, where MRPIDS is itself
a Table D mnemonic which is therefore itself defined in a similar manner elsewhere within the
section. As was the case with the [first section](#section1), separator lines may be employed
within this section in order to improve human-readability, as long as they contain the "|" character
that is required to be in columns 1, 12, and 80 for all non-comment lines within this section,
and the BUFRLIB software will continue reading lines of the file as though they are part of the
second section until it encounters one that does not adhere to this format.

<br>

At this point, most readers who have taken at least a cursory glance at the
[sample DX BUFR tables file](#bftab) will have no doubt begun to wonder about all of the additional
punctuation characters and symbols included within the sequence definitions of the second section.
We'll now address those concerns by stating that these are replication indicators for the mnemonic(s)
in question:

| Symbol | Meaning |
|--------|---------|
| &lt; &gt;  | The enclosed mnemonic is replicated using 1-bit delayed replication (either 0 or 1 replications) |
| { }  | The enclosed mnemonic is replicated using 8-bit delayed replication (between 0 and 255 replications) |
| ( )  | The enclosed mnemonic is replicated using 16-bit delayed replication (between 0 and 65535 replications) |
| " "n | The enclosed mnemonic is replicated using regular (non-delayed) replication, with a fixed replication factor of n |

Examples of most of these cases are shown within the [sample DX BUFR tables file](#bftab), and, through successive
application, can lead to the definition of some rather interesting data structures. For example,
the Table A mnemonic NC002001, which defines the layout of a data subset of the type "RAWINSONDE - FIXED LAND",
consists of the following sequence of Table B and Table D mnemonics:

1. UARTM, followed by
2. between 0 and 255 replications of RCPTIM, followed by
3. between 0 and 255 replications of BID, followed by
4. UASID, followed by
5. between 0 and 255 replications of UARID, followed by
6. between 0 and 255 replications of UARLV, followed by
7. either 0 or 1 replications of UASDG, followed by
8. between 0 and 255 replications of UARDCS, followed by
9. between 0 and 255 replications of RAWRPT, followed by
10. between 0 and 255 replications of UACLD, followed by
11. either 0 or 1 replications of UAADF, followed by
12. WMOB, followed by
13. WMOS, followed by
14. WMOR

where, e.g., the constituent Table D mnemonic UARLV itself consists of the following sequence:

1. VSIG, followed by
2. QMPR, followed by
3. PRLC, followed by
4. QMGP, followed by
5. either 0 or 1 replications of UAGP07, followed by
6. either 0 or 1 replications of UAGP10, followed by
7. either 0 or 1 replications of UATMP, followed by
8. either 0 or 1 replications of UAWND, followed by
9. either 0 or 1 replications of UAWSH

and where, in turn, UAGP07, UAGP10, UATMP, etc. are also Table D mnemonics which can themselves be
further resolved. So we can even nest certain replication sequences inside of other replication sequences,
and, further, via the judicious use of the &lt; &gt; indicator, even turn on/off entire sequences of data values
simply and efficiently. An example of this is the UAWSH (i.e. "RADIOSONDE WIND SHEAR DATA") sequence,
whose constituent data values are only ever present in a rawinsonde report when a level of maximum wind is
being reported (and, even then, not always!). In this case, enclosing the entire sequence within a &lt; &gt; 
indicator allows the lack of such data within a report level to be noted by the use of a single bit set to "0"
(i.e. 0 replications), rather than having to otherwise store the appropriate "missing" value for every
constituent data value. Over the course of many data levels within many data subsets within a single BUFR
message, this can add up to significant encoding efficiency, and, in turn, the use of less required storage
space per BUFR message. So, in summary, the judicious use of replication can even lead to more efficient
data storage for certain types of data.

<br>

Looking back again at the [sample DX BUFR tables file](#bftab), notice how several of the Table D mnemonics
such as RCPTIM and BID are used within both the NC001003 and NC002001 data subset types. This brings up
a good point; namely, that by logically grouping certain Table B mnemonics together within carefully-constructed
Table D sequence mnemonics, such mnemonics can be easily and efficiently re-used within different Table A
mnemonic definitions within the same BUFR tables file. In fact, this would be a good time to also point out that,
when using the BUFRLIB software, Table D sequence mnemonics are the only types of mnemonics upon which any
type of replication may be directly performed. Thus, in particular, if we wish to effect the replication of
a single, particular Table B mnemonic, then we must do so by defining a Table D sequence mnemonic whose only
constituent is that one particular Table B mnemonic and then replicating the sequence mnemonic.
For a specific example of such a situation, take a look at the definition of RAWRPT within the sample file.

<br>

Before we end our discussion on the second section of our sample BUFR tables file, there are a few other
special situations that we need to explain in further detail:

<br>

First, notice how a 201YYY indicator precedes each occurrence of ACAV within the definition of the Table D
sequence mnemonic OBSEQ as well as each occurrence of HINC within the definition of the Table A mnemonic
NC002007. This indicator is called an <i>operator</i>, and readers more familiar with the details of BUFR
will no doubt recognize it from Table C of the
[official WMO master BUFR tables](@ref wmomstab).
In short, the effect of this operator is that, for each Table B mnemonic which follows it within the current
sequence, and continuing up until the point in the sequence where a corresponding 201000 operator is reached
(and which turns off the effect), ( YYY - 128 ) bits should be added to the bit width that is otherwise defined
for that Table B mnemonic within the [third section](#section3) of the DX BUFR tables file, so that the net
effect is to change the number of bits occupied by the data value corresponding to that mnemonic within the
overall data subset. Thus, for example, the sequence: 

<br>

201132  HINC  201000

<br>

indicates that ( 132 - 128 ) = 4 bits should be added to the data width that was defined for mnemonic HINC
within the [third section](#section3) of the DX BUFR tables file, and, therefore, that for this occurrence
of that mnemonic within the overall data subset, the corresponding data value will occupy ( 12 + 4 ) = 16 bits.

<br>

Other than 201YYY, the BUFRLIB software also supports the similar use of the 202YYY (change scale), 203YYY
(change reference value), 204YYY (add associated field), 205YYY (add character data), 206YYY (define data
width for local descriptor), 207YYY (increase scale, reference value and data width) and 208YYY (change data
width for CCITT IA5 descriptor) operators from BUFR Table C.

<br>
@anchor ufbsubs

Next, take a look at the subset definitions for Table A mnemonics TBLAEX1, TBLAEX2, TBLAEX3 and TBLAEX4
within the [sample DX BUFR tables file](#bftab):

<pre>
| TBLAEX1  | UASID  UARTM  {PRGPTMDP}                                          |
|          |                                                                   |
| TBLAEX2  | UASID  UARTM  "PRGPTMDP"100                                       |
|          |                                                                   |
| TBLAEX3  | UASID  UARTM  {PRGPTMDP}  PRLC  UACLD  PRLC  HOVI                 |
|          |                                                                   |
| TBLAEX4  | UASID  UARTM                                                      |
| TBLAEX4  | PRLC  UACLD GEOP  TMDB  HOVI TMDP                                 |
| TBLAEX4  | PRLC  UACLD TMDB  TMDP  HOVI GEOP                                 |
| TBLAEX4  | PRLC  UACLD GEOP  TMDP  HOVI TMDB                                 |
| TBLAEX4  | PRLC  UACLD TMDB  GEOP  HOVI TMDP                                 |
</pre>

Furthermore, let's presume we wanted to read all of the occurrences
of the Table B mnemonics PRLC, GEOP, TMDB and TMDP from data subsets encoded according to each of these
different subset definitions.  In the first three cases, these 
mnemonics are all contained within the Table D sequence PRGPTMDP as follows:

<pre>
| PRGPTMDP | PRLC  GEOP  TMDB  TMDP                                            |
</pre>

However, due to some subtle differences in how
the sequence is replicated in each case, different BUFRLIB subroutines need to be used to
retrieve all of the respective PRLC, GEOP, TMDB and TMDP values in each case.
For example, within the first subset definition TBLAEX1, we have delayed replication using the notation
{PRGPTMDP}, so we could use subroutine ufbint() with STR='PRLC GEOP TMDB TMDP' to retrieve all of the
replications of these mnemonics into our output USR array, where each row of USR would contain corresponding
values for PRLC, GEOP, TMDB and TMDP in the first four columns, and where the return value IRET would tell us
how many rows of USR were actually filled with such values (i.e. the total number of replications that were
read). 
Alternatively, we could use subroutine ufbseq() with STR='PRGPTMDP', which would accomplish the exact same
thing.  Or, if we only wanted to know the total number of replications without
actually reading out all of the respective PRLC, GEOP, TMDB and TMDP values, we could also call
subroutine ufbint() with STR='{PRGPTMDP}', and the corresponding array location in USR would contain the
same value that would have been returned in IRET during our earlier call to ufbint() with 
STR='PRLC GEOP TMDB TMDP', or during our earlier call to ufbseq() with STR='PRGPTMDP'.

<br>

The second subset definition TBLAEX2 is different, because here instead of delayed replication we have fixed
replication using the
notation "PRGPTMDP"100, so in this case we must instead use subroutine ufbrep() with STR='PRLC GEOP TMDB TMDP'
in order to read all of the respective PRLC, GEOP, TMDB and TMDP values into the first four columns of our
USR array.  However, since in this case the number of replications is fixed at 100, then the return value
IRET would always be set to 100, and if there were less than 100 actual rows of available data values, then
the remaining rows of USR up through row 100 would be filled out with "missing" values by the BUFRLIB software.

<br>

The third subset definition TBLAEX3 is a bit more interesting, because we once again have delayed replication
using the notation {PRGPTMDP} just like in TBLAEX1; however, in this case there are additional subsequent
occurrences of Table B mnemonic PRLC which appear outside of the delayed replication sequence.  So in this
case we again have to use subroutine ufbrep() with STR='PRLC GEOP TMDB TMDP' in order to read all of
the occurrences of PRLC from within the data subset, and the return value IRET will now be 2 larger than
in our earlier example for the TBLAEX1 subset definition, because now we have 2 extra rows in USR which
contain additional PRLC values in the first column.  If we had instead tried to use subroutine ufbint() with
the same STR value for this TBLAEX3 subset definition, then we wouldn't have been able to read those last 2
extra rows, and our USR output array instead would have looked exactly as it did in our earlier TBLAEX1 example.

<br>

The fourth and final subset definition TBLAEX4 is even more interesting, not to mention a bit more contrived.
Here we have exactly 4 replications of the Table B mnemonics PRLC, GEOP, TMDB and TMDP, but the
replication is done via explicitly listing all of the occurrences of these mnemonics within the subset
definition, and where the mnemonics appear in slightly different order within each replication.  Again, this
is a contrived example, but it will serve to better explain how subroutine ufbrep() actually works, and also
how it behaves slightly differently from ufbint() and ufbseq() as well as from yet another subroutine ufbstp().
First of all, in the case of subroutine ufbrep(), the first listed mnemonic in STR
is always treated as a "pivot", meaning that the second dimension of USR (i.e. the number of rows)
is always defined by subsequent occurrences of this pivot mnemonic within the overall subset definition, and
where any remaining mnemonics within STR are always independently searched for between each successive
occurrence of the pivot mnemonic.  So in the case of subroutine ufbrep() with STR='PRLC GEOP TMDB TMDP'
for TBLAEX4, there will
be 4 rows of values in the returned USR array as follows, since there were 4 total occurrences of the pivot
mnemonic PRLC:

<table border>
<tr>
  <th>USR(I,J)</th>
  <th>J=1</th>
  <th>J=2</th>
  <th>J=3</th>
  <th>J=4</th>
</tr>
<tr>
  <th>I=1</th>
  <td>1st PRLC value</td>
  <td>1st GEOP value</td>
  <td>1st TMDB value</td>
  <td>1st TMDP value</td>
</tr>
<tr>
  <th>I=2</th>
  <td>2nd PRLC value</td>
  <td>2nd GEOP value</td>
  <td>2nd TMDB value</td>
  <td>2nd TMDP value</td>
</tr>
<tr>
  <th>I=3</th>
  <td>3rd PRLC value</td>
  <td>3rd GEOP value</td>
  <td>3rd TMDB value</td>
  <td>3rd TMDP value</td>
</tr>
<tr>
  <th>I=4</th>
  <td>4th PRLC value</td>
  <td>4th GEOP value</td>
  <td>4th TMDB value</td>
  <td>4th TMDP value</td>
</tr>
</table>

In other words, ufbrep() searched independently for each of the mnemonics GEOP, TMDB and TMDP between each
occurrence of the pivot mnemonic PRLC, so the varying order of those mnemonics between each successive
occurrence of PRLC was immaterial, and all of the requested values were found and returned.  However,
contrast that with what the first 4 rows of USR would look like if we called
subroutine ufbstp() with the same STR='PRLC GEOP TMDB TMDP':

<table border>
<tr>
  <th>USR(I,J)</th>
  <th>J=1</th>
  <th>J=2</th>
  <th>J=3</th>
  <th>J=4</th>
</tr>
<tr>
  <th>I=1</th>
  <td>1st PRLC value</td>
  <td>1st GEOP value</td>
  <td>1st TMDB value</td>
  <td>1st TMDP value</td>
</tr>
<tr>
  <th>I=2</th>
  <td>2nd PRLC value</td>
  <td>2nd GEOP value</td>
  <td><b>"missing" value</b></td>
  <td><b>"missing" value</b></td>
</tr>
<tr>
  <th>I=3</th>
  <td>3rd PRLC value</td>
  <td>3rd GEOP value</td>
  <td>3rd TMDB value</td>
  <td><b>"missing" value</b></td>
</tr>
<tr>
  <th>I=4</th>
  <td>4th PRLC value</td>
  <td>4th GEOP value</td>
  <td><b>"missing" value</b></td>
  <td><b>"missing" value</b></td>
</tr>
</table>

As shown here, the order of the non-pivot mnemonics between each occurrence of the pivot mnemonic PRLC is
now very important when using subroutine ufbstp().  Specifically, ufbstp() only ever moves forward from each
occurrence of the pivot mnemonic, and for only one non-pivot mnemonic at a time in the same exact order in
which they appear in STR.  So in this case, in the
second row of the output USR array, it searched forward from PRLC for the first occurrence of GEOP, and then
only after it found that did it search for the next mnemonic in the string (i.e. TMDB), but only searching
forward from GEOP rather than going all the way back to the previous occurrence of the pivot mnemonic PRLC
and searching from there.  And since it couldn't find any occurrence of TMDB between the location of GEOP
within the second replication and the third occurrence of the pivot mnemonic PRLC which signaled
the start of the third repliation, then TMDB and TMDP both ended up as "missing" in the second row of the
returned USR array.
Similarly for the third replication, and starting from the third occurrence of the pivot mnemonic PRLC, it
finds GEOP and then begins searching from there for TMDB, which it eventually finds, but not until after it
has already stepped past TMDP. And since it can only move forward from the point where it found TMDB, then
it never finds TMDP for the third replication before it encounters the fourth and final occurrence of the
pivot mnemonic PRLC, which in turn is why TMDP is "missing" in the third row of the returned USR array.
Finally, for the fourth replication, and starting from the fourth occurrence of the pivot mnemonic PRLC, it
finds GEOP and then begins searching from there for TMDB, but by that point it has already stepped past TMDB,
so it never finds that nor the subsequent TMDP mnemonic, and therefore both of those values are "missing" in
the fourth row of USR as well.

<br>

Next, take a look at the definitions of the Table D sequence mnemonics TMPSQ3, WNDSQ2, and PCPSQ3; in
particular, notice that, within these definitions, there are references to several mnemonics such as .DTHMITM and
.DTHMXGS which were not previously-declared within the [first section](#section1) of the table. At first glance,
this seems to contradict everything that we previously said about the need to initially declare all mnemonics
within the first section; however, upon closer inspection, the reader will notice that there do exist, within
the first section, declarations for mnemonics .DTH.... and .DTH..... So, what exactly is going on here? The
answer is that each of these is a special mnemonic known as a <i>following-value mnemonic</i>, meaning that,
when it is used within a sequence definition, it implies a special relationship with the mnemonic that immediately
follows it within the sequence. In fact, this relationship is so special that, when a following-value mnemonic is
used within a sequence definition, the .... portion of the mnemonic is replaced with the mnemonic that immediately
follows it! For example, when .DTH.... is used within the definition of the Table D sequence mnemonic TMPSQ3, it
appears as .DTHMXTM and .DTHMITM because it appears immediately before, respectively, the mnemonics MXTM and MITM.
However, when it appears within the definition of PCPSQ3, it appears as .DTHTOPC since it immediately precedes
TOPC within that sequence. To be precise, a following-value mnemonic is declared with a "." as the first character,
followed by no more than 3 alphanumeric characters as an identifier, followed by 4 more "." characters which must
then be replaced with the mnemonic that immediately follows it whenever and wherever it is used within a sequence
definition. This is important, because the BUFRLIB software will actually check that the immediately-following
mnemonic matches the last 4 characters of the following-value mnemonic and will diagnose an error if it does not.

<br>

In general, the "following-value" attribute is useful because it allows the same mnemonic to be used repeatedly
within the same overall Table A data subset definition in a very intuitive fashion and yet, since each occurrence
retains its own unique identification (e.g. .DTHMXTM, .DTHTOPC, etc.), then each one can still be individually
accessed independent of the others via subroutine ufbint(). An alternative would be to declare a regular mnemonic
such as DTHRFV instead of .DTH.... within the first section of the tables file and then use that mnemonic in all
of the same places within the same Table A data subset definition, but then we'd have to use subroutine ufbrep()
to access all such values simultaneously, even if we weren't interested in all of them. And we'd also lose the
intuitiveness provided by having available, within the mnemonic itself, the name of the mnemonic to which the
corresponding value applies.
</div>

<div id="section3">
## Section 3

It's now time to move on to the third and final section of a DX BUFR tables file. As we mentioned earlier,
this section is used to define the scale factor, reference value, data width, and units for all of the Table B
mnemonics that were previously declared in the [first section](#section1). In particular, the reader may recall
that the units definition for each Table B mnemonic in turn determines how data values corresponding to that
mnemonic are read/written from/to the REAL*8 array USR within BUFRLIB subroutines such as
ufbint(), ufbrep() and ufbseq().

<br>

In looking again at our [sample DX BUFR tables file](#bftab), we see that the format for the third section of such a file
is to have our same old, familiar "|" delimiter in columns 1, 12, 19, 33, 39, 66, and 80 of each line.
These delimiters, in turn, form the columns for the mnemonic (listed exactly as it was previously within
the [first section](#section1)), the scale factor (right-justified from column 17), the reference value
(right-justified from column 31), the bit width (right-justified from column 37), and the units (left-justified
from column 41). As with the previous two sections, blank separator lines may be employed in order to improve
human-readability, and, for the same reason, it's also recommended to list the mnemonics in the same order in
which they were declared within the first section, although this is by no means a requirement of the software.
However, note that any mnemonic whose corresponding data values are to be treated as character data must have
its units listed as "CCITT IA5", which is basically just a formal synonym for ASCII.
</div>
