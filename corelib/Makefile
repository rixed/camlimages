#*********************************************************************#
#                                                                     #
#                          Caml Images                                #
#                                                                     #
#            François Pessaux, projet Cristal, INRIA Rocquencourt     #
#            Pierre Weis, projet Cristal, INRIA Rocquencourt          #
#            Jun Furuse, projet Cristal, INRIA Rocquencourt           #
#                                                                     #
#  Copyright 1999-2004,                                               #
#  Institut National de Recherche en Informatique et en Automatique.  #
#  Distributed only by permission.                                    #
#                                                                     #
#*********************************************************************#

#(* $Id: Makefile,v 1.5 2004/10/01 10:26:45 weis Exp $ *)

include ../Makefile.config
include ../Makefile.build

####################################### drivers ##############################

LIBNAME		= $(LIBPREFIX)core
EXTCLIB		= 
EXTMLLIB	=
CFLAGS		=
COMPFLAGS	= 
MLMODULES	= camlimages mstring color region tmpfile bitmap \
		  genimage \
	  	  rgba32 rgb24 index8 index16 cmyk32 \
		  images oColor oImages reduce geometry colorhist \
		  blend
CMODULES	= 

MLOBJS		= $(addsuffix .cmo, $(MLMODULES))
COBJS		= $(addsuffix .o, $(CMODULES))
CMA		= $(LIBNAME).cma
CMXA		= $(LIBNAME).cmxa
CLIB		=
CDLL		=

#######

all: byt opt

byt: $(CMA) $(CLIB)

opt: $(CMXA) $(CLIB)

include ../Makefile.shared

