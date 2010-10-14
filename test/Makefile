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

#(* $Id: Makefile,v 1.6 2004/09/27 15:34:07 weis Exp $ *)

include ../Makefile.config
include Makefile.fortest

all: byt opt

LINKFLAGS= $(LINKFLAGS_CAMLIMAGES)

byt: test.ml
	$(CAMLC) $(COMPFLAGS_CAMLIMAGES) $(DLLPATHS) $(LINKFLAGS) \
	 -o test.byt test.ml

opt: test.ml
	$(CAMLOPT) $(COMPFLAGS_CAMLIMAGES) $(LINKFLAGS:.cma=.cmxa) \
	 -o test test.ml

clean::
	rm -f test.byt test *.cm* *.o out.image screen.bmp

depend: .depend

.depend:
	$(CAMLDEP) $(COMPFLAGS) */*.mli */*.ml > .depend

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly .c .o

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) -c $<

.mll.cmo:
	$(CAMLLEX) $<
	$(CAMLC) $(COMPFLAGS) -c $*.ml

.mll.cmx:
	$(CAMLLEX) $<
	$(CAMLOPT) $(COMPFLAGS) -c $*.ml

.mly.cmo:
	$(CAMLYACC) $<
	$(CAMLC) $(COMPFLAGS) -c $*.mli
	$(CAMLC) $(COMPFLAGS) -c $*.ml

.mly.cmx:
	$(CAMLYACC) $<
	$(CAMLOPT) $(COMPFLAGS) -c $*.mli
	$(CAMLOPT) $(COMPFLAGS) -c $*.ml

.mly.cmi:
	$(CAMLYACC) $<
	$(CAMLC) $(COMPFLAGS) -c $*.mli

.mll.ml:
	$(CAMLLEX) $<

.mly.ml:
	$(CAMLYACC) $<

.c.o:
	$(CAMLC) -ccopt "-o `echo $< | sed -e s/\.c$$/.o/`" -c $< 

include .depend
