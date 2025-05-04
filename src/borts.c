/** @file borts.c
 *  @brief Implement abort functions
 *  @author D. O'Connor @date 2025-05-04
 */
#include <setjmp.h>
#include <string.h>
#include "bufrlib.h"

/// \cond DO_NOT_DOCUMENT
/* Internal variables to hold jump buffer, error string, etc */
static int setjmp_enabled = 0;
static jmp_buf env;
char *setjmperr = NULL;
/// \endcond

/** Enable setjmp call from bort/bort2
 *
 * This copies the environment to a local buffer and sets a flag
 * so that later calls to bort/bort2 will call longjmp to allow
 * for more graceful error handling.
 *
 * @param buf - Set jump buffer
 *
 * @author D. O'Connor @date 2025-05-04
 */
void
enablesetjmp(jmp_buf *buf) {
  if (buf != NULL) {
    memcpy(&env, buf, sizeof(env));
    setjmp_enabled = 1;
  } else
    setjmp_enabled = 0;
}

/** Log an error message, then abort the program or call setjmp.
 *
 * Calls setjmp if enablesetjmp() has been called previously,
 * otherwise it exits the process.
 *
 * This subroutine is similar to subroutine bort2(), except that bort2() logs
 * two error messages instead of one.
 *
 * @param str - Error message
 * @param slen - Error message buffer length
 * @author J. Woollen @date 1998-07-08
 */
void
bort_(char *str, size_t slen) {
  /* If setjmp is enabled stash a copy of the passed in error
   * for retrieval by the setjmp caller, then longjmp
   */
  if (setjmp_enabled) {
    if (setjmperr != NULL)
      free(setjmperr);
    setjmperr = calloc(1, slen + 1);
    strncat(setjmperr, str, slen);
    longjmp(env, 8);
  }
  puts(" ");
  puts("***********BUFR ARCHIVE LIBRARY ABORT**************");
  puts(str);
  puts("***********BUFR ARCHIVE LIBRARY ABORT**************");
  puts(" ");
  exit(8);
}

/** Log two error messages, then abort the program or call setjmp.
 *
 * Calls setjmp if enablesetjmp() has been called previously,
 * otherwise it exits the process.
 *
 * This subroutine is similar to subroutine bort(), except that bort() logs
 * one error message instead of two.
 *
 * @param str1 - First error message
 * @param str2 - Second error message
 * @param slen1 - First error message buffer length
 * @param slen2 - Second error message buffer length
 *
 * @author J. Woollen @date 1998-07-08
 */
void
bort2_(char *str1, char *str2, size_t slen1, size_t slen2) {
  /* If setjmp is enabled stash a copy of the passed in error
   * for retrieval by the setjmp caller, then longjmp
   */
  if (setjmp_enabled) {
    if (setjmperr != NULL)
      free(setjmperr);
    setjmperr = calloc(1, slen1 + slen2 + 2);
    strncpy(setjmperr, str1, slen1);
    strcat(setjmperr, "\n");
    strncat(setjmperr, str2, slen2);
    longjmp(env, 8);
  }
  puts(" ");
  puts("***********BUFR ARCHIVE LIBRARY ABORT**************");
  puts(str1);
  puts(str2);
  puts("***********BUFR ARCHIVE LIBRARY ABORT**************");
  puts(" ");
  exit(8);
}
