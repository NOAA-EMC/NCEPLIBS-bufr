 gen_cfunction() {
 local setx_status=${-//[^x]/}
 [[ -n $setx_status ]] && set +x

   logFILE=$1
   oneLINE=$2
   libINFO=$3
   LIB=$(echo "${!libINFO}" | \
         grep "LIBRARY FILE NAME: " | \
         sed -e 's/LIBRARY FILE NAME: //')
   [[ $LIB == lib*.a ]] || {
     echo "??? gen_cfunction: unknown library name ($LIB) ???" >&2
     exit -1
   }
   libNAME=$(echo ${LIB:3} | \
     sed -e 's/^\(.*\)_v[[:digit:]]\+\.[[:digit:]]\+\.[[:digit:]]\+.*$/\1/')
   cFunFILE=$(basename ${logFILE} .txt).c
   oFunFILE=${cFunFILE/%.c/.o}
   brfFunct=${libNAME}_brief_info
   libFunct=${libNAME}_library_info
   logFunct=${libNAME}_building_log

   cat << EOI > $cFunFILE
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
 char *${brfFunct}(char *infoline);
 int ${libFunct}(void);
 int ${logFunct}(void);
 static void tprintf(const char *str);
 void ${brfFunct}_(char *oneline, size_t n) {
  char infoline[256];
  ${brfFunct}(infoline);
  strncpy(oneline, infoline, n);
  return;
 }
 void ${libFunct}_(void) {
  ${libFunct}();
  return;
 }
 void ${logFunct}_(void) {
  ${logFunct}();
  return;
 }
 char *${brfFunct}(char *infoline) {
 $(sed -e '1,$ s/^\(.*\) = '\''\(.*\)'\'',\?$/  const char \1[] = "\2";/g' <<< "${!oneLINE}")
  infoline[0] = '\0';
  strcat(infoline, nclibver);
  strcat(infoline, compiled);
  strcat(infoline, compiler);
  strcat(infoline, datetime);
  return infoline;
 }
 int ${libFunct}(void) {
$(sed -e 's/^/  tprintf("/g; 1,$ s/$/");/g' <<< "${!libINFO}")
  return 0;
 }
 int ${logFunct}(void) {
  printf("... building log ...\n");
$(sed -e 's/"/\\"/g; 1,$ s/%/%%/g; 1,$ s/^/  printf("/g; 1,$ s/$/\\n");/g' $logFILE)
  return 0;
 }
 static void tprintf(const char *str) {
  const char *boldon = "\e[1m";
  const char *boldoff = "\e[0m";
  char *str1 = strdup(str);
  char *temp = strstr(str1, ": ");  //Get the pointer to substr token
  if (temp) {
    temp += 2;
    char *str2 = strdup(temp);
    *temp = '\0';
    if (isatty(STDOUT_FILENO)) {
      printf("%s%s%s%s\n", boldon, str1, boldoff, str2);
      free(str2);
      free(str1);
      return;
    }
    free(str2);
  }
  free(str1);
  printf("%s\n", str);
  return;
 }
EOI

   rm $logFILE
   echo $cFunFILE

 [[ -n $setx_status ]] && set -x
 }
