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

#(* $Id: Makefile,v 1.4 2004/10/01 10:26:45 weis Exp $ *)

include ../Makefile.config
include ../Makefile.build

####################################### drivers ##############################

LIBNAME		= $(LIBPREFIX)lablgtk
EXTCLIB		= 
EXTMLLIB	= 
CFLAGS		=
COMPFLAGS	= -I ../corelib -I $(LABLGTKDIR)
MLMODULES	= ximage oXimage
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

