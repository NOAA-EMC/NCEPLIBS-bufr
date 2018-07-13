#!/bin/sh -vx

export CLASSPATH=/usr1/jator/bufrtables/workspace/BUFR_tables/bin:/usr/share/java/mysql-connector-java.jar

#  make TableA html files for all versions, including remaking all previous versions, since each
#  version contains links to all of the others
java DBextract/DBextract TableA 0 13 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableA 0 14 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableA 0 15 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableA 0 16 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableA 0 17 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableA 0 18 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableA 0 19 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableA 0 20 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableA 0 21 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableA 0 22 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableA 0 23 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableA 0 24 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableA 0 25 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableA 0 26 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableA 0 27 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableA 0 28 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableA 0 29 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableA 0 30 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html

#  make TableB html files for all versions, including remaking all previous versions, since each
#  version contains links to all of the others
java DBextract/DBextract TableB 0 13 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableB 0 14 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableB 0 15 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableB 0 16 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableB 0 17 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableB 0 18 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableB 0 19 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableB 0 20 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableB 0 21 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableB 0 22 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableB 0 23 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableB 0 24 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableB 0 25 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableB 0 26 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableB 0 27 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableB 0 28 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableB 0 29 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableB 0 30 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html

#  make TableD html files for all versions, including remaking all previous versions, since each
#  version contains links to all of the others
java DBextract/DBextract TableD 0 13 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableD 0 14 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableD 0 15 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableD 0 16 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableD 0 17 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableD 0 18 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableD 0 19 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableD 0 20 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableD 0 21 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableD 0 22 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableD 0 23 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableD 0 24 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableD 0 25 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableD 0 26 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableD 0 27 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableD 0 28 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableD 0 29 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract TableD 0 30 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html

#  make CodeFlag html files for all versions, including remaking all previous versions, since each
#  version contains links to all of the others
java DBextract/DBextract CodeFlag 0 13 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract CodeFlag 0 14 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract CodeFlag 0 15 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract CodeFlag 0 16 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract CodeFlag 0 17 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract CodeFlag 0 18 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract CodeFlag 0 19 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract CodeFlag 0 20 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract CodeFlag 0 21 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract CodeFlag 0 22 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract CodeFlag 0 23 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract CodeFlag 0 24 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract CodeFlag 0 25 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract CodeFlag 0 26 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract CodeFlag 0 27 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract CodeFlag 0 28 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract CodeFlag 0 29 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html
java DBextract/DBextract CodeFlag 0 30 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles html

#  make TableB, TableD and CodeFlag bufrlib files for the new latest version
java DBextract/DBextract TableB 0 30 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles bufrlib
java DBextract/DBextract TableD 0 30 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles bufrlib
java DBextract/DBextract CodeFlag 0 30 /usr1/jator/bufrtables/workspace/BUFR_tables/outfiles bufrlib
