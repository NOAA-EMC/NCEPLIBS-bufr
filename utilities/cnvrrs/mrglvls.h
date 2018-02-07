/*
**  Enable access to FORTRAN MODULES MOD_R64, MOD_R65 and MOD_R66.
*/
#ifdef UNDERSCORE
#  define R64_BASE(var) mod_r64_mp_ ## var ## _
#  define R65_BASE(var) mod_r65_mp_ ## var ## _
#  define R66_BASE(var) mod_r66_mp_ ## var ## _
#  define MRG_BASE(var) mod_mrg_mp_ ## var ## _
#else
#  define R64_BASE(var) mod_r64_mp_ ## var ##
#  define R65_BASE(var) mod_r65_mp_ ## var ##
#  define R66_BASE(var) mod_r66_mp_ ## var ##
#  define MRG_BASE(var) mod_mrg_mp_ ## var ## 
#endif
extern f77int R64_BASE(nr64);
extern f77r8  R64_BASE(r64)[][5];
extern f77int R65_BASE(nr65);
extern f77r8  R65_BASE(r65)[][5];
extern f77int R66_BASE(nr66);
extern f77r8  R66_BASE(r66)[][8];
extern f77int MRG_BASE(nmrg);
extern f77r8  MRG_BASE(mrg)[][10];
