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

#(* $Id: Makefile,v 1.3 2004/09/24 14:27:31 weis Exp $ *)

include ../../Makefile.config
include ../../Makefile.examples

COMPFLAGS= $(COMPFLAGS_CAMLIMAGES)
LINKFLAGS= $(COMPFLAGS_CAMLIMAGES) $(LINKFLAGS_CAMLIMAGES)

all: byt opt

byt: freetypetest japanese

opt: libfreetype.a freetype.cmxa

TESTOBJS= freetypetest.cmo
JAPANOBJS= japanese.cmo

freetypetest: $(TESTOBJS)
	$(CAMLC) -custom -o $@ $(LINKFLAGS) $(TESTOBJS)

japanese: $(JAPANOBJS)
	$(CAMLC) -custom -o $@ $(LINKFLAGS) $(JAPANOBJS) 

clean::
	rm -f freetypetest japanese

clean::
	rm -f charmapids

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) -c $<

clean::
	rm -f *.o *.cm* *.a

depend: 
	$(CAMLDEP) -I test *.ml *.mli  > .depend

include .depend
