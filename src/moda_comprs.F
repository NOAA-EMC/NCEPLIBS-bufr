	MODULE MODA_COMPRS

#ifndef MXCSB_H
#define MXCSB_H
	  USE MODV_MXCSB
#endif

#ifndef MXCDV_H
#define MXCDV_H
	  USE MODV_MXCDV
#endif

#ifndef MXLCC_H
#define MXLCC_H
	  USE MODV_MXLCC
#endif

	  INTEGER                    :: NCOL
	  INTEGER(8)                 :: INCR
	  INTEGER(8)   , ALLOCATABLE :: MATX(:,:) 
	  CHARACTER*(:), ALLOCATABLE :: CATX(:,:)

	END MODULE
