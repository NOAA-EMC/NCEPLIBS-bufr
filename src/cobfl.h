/** @file
 *  @brief Define file pointers for reading or writing BUFR
 *  messages via a C language interface.
 *
 *  @author J. Ator
 *  @date 2005-11-29
 */

#ifdef IN_COBFL
    FILE *pbf[2];  /* each element will automatically initialize to NULL */
#else
    extern FILE *pbf[2];
#endif
