# 1 "modv_MXMSGL.F"
	MODULE MODV_MXMSGL

C	  MXMSGL is the maximum length (in bytes) of a BUFR message that
C	  can be read or written by the BUFRLIB software.

# 15


# 19

	  PARAMETER ( MXMSGL = 600000 )

	  PARAMETER ( MXMSGLD4 = MXMSGL/4 )



	END MODULE
