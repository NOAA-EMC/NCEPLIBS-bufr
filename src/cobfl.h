#ifdef IN_COBFL
    FILE *pbf[2];  /* each element will automatically initialize to NULL */
#else
    extern FILE *pbf[2];
#endif
