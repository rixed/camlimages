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

#(* $Id: Makefile,v 1.3 2004/10/01 10:26:45 weis Exp $ *)

include ../Makefile.config
include ../Makefile.build

####################################### drivers ##############################

LIBNAME		= $(LIBPREFIX)freetype
EXTCLIB		= $(LIBFREETYPE)
EXTMLLIB	=
CFLAGS		= $(INCLUDE_FREETYPE)
COMPFLAGS	= -I ../corelib
MLMODULES	= ftlow freetype jis_table jis_unicode fttext oFreetype
CMODULES	= ftintf

MLOBJS		= $(addsuffix .cmo, $(MLMODULES))
COBJS		= $(addsuffix .o, $(CMODULES))
CMA		= $(LIBNAME).cma
CMXA		= $(LIBNAME).cmxa
CLIB		= lib$(LIBNAME).a
CDLL		= dll$(LIBNAME).so

all: byt

opt: opt

#######

FREETYPE_SHARED_CODE= freetype.mli jis_table.ml \
			jis_unicode.ml jis_unicode.mli \
			fttext.ml fttext.mli oFreetype.ml

clean::
	rm -f $(FREETYPE_SHARED_CODE)

freetype_shared_code:  $(FREETYPE_SHARED_CODE)

freetype.mli:
	ln -s ../freetype2/freetype.mli .

jis_table.ml:
	ln -s ../freetype2/jis_table.ml .

jis_unicode.ml:
	ln -s ../freetype2/jis_unicode.ml .

jis_unicode.mli:
	ln -s ../freetype2/jis_unicode.mli .

fttext.ml:
	ln -s ../freetype2/fttext.ml .

fttext.mli:
	ln -s ../freetype2/fttext.mli .

oFreetype.ml:
	ln -s ../freetype2/oFreetype.ml .

byt: $(FREETYPE_SHARED_CODE) $(CMA) $(CLIB)

opt: $(FREETYPE_SHARED_CODE) $(CMXA) $(CLIB)

include ../Makefile.shared

