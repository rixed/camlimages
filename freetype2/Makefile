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

#(* $Id: Makefile,v 1.6 2004/10/01 10:26:45 weis Exp $ *)

include ../Makefile.config
include ../Makefile.build

####################################### drivers ##############################

LIBNAME		= $(LIBPREFIX)freetype
EXTCLIB		= $(LIBFREETYPE)
EXTMLLIB	=
CFLAGS		:= $(INCLUDE_FREETYPE) $(CFLAGS)
COMPFLAGS	= -I ../corelib
MLMODULES	= ftlow freetype jis_table jis_unicode fttext oFreetype
CMODULES	= ftintf

MLOBJS		= $(addsuffix .cmo, $(MLMODULES))
COBJS		= $(addsuffix .o, $(CMODULES))
CMA		= $(LIBNAME).cma
CMXA		= $(LIBNAME).cmxa
CLIB		= lib$(LIBNAME).a
CDLL		= dll$(LIBNAME).so

#######

all: byt opt

byt: $(CMA) $(CLIB)

opt: $(CMXA) $(CLIB)

include ../Makefile.shared

