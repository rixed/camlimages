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

#(* $Id: Makefile,v 1.7 2004/09/29 09:52:35 weis Exp $ *)

include ../../Makefile.examples

COMPFLAGS= $(COMPFLAGS_CAMLIMAGES)
LINKFLAGS= $(COMPFLAGS) $(LINKFLAGS_CAMLIMAGES)

all: byt opt

SRCS= normalize.ml

byt: normalize.byt

normalize.byt: $(SRCS:.ml=.cmo)
	$(CAMLC) -o normalize.byt $(DLLPATHS) $(LINKFLAGS) $(SRCS:.ml=.cmo)

opt: normalize

normalize: $(SRCS:.ml=.cmx)
	$(CAMLOPT) -o normalize $(LINKFLAGS:.cma=.cmxa) $(SRCS:.ml=.cmx)

clean::
	rm -f normalize normalize.byt

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

.mll.cmo:
	$(CAMLLEX) $<
	$(CAMLC) -c $(COMPFLAGS) $*.ml

.mll.cmx:
	$(CAMLLEX) $<
	$(CAMLOPT) -c $(COMPFLAGS) $*.ml

.mly.cmo:
	$(CAMLYACC) $<
	$(CAMLC) -c $(COMPFLAGS) $*.mli
	$(CAMLC) -c $(COMPFLAGS) $*.ml

.mly.cmx:
	$(CAMLYACC) $<
	$(CAMLOPT) -c $(COMPFLAGS) $*.mli
	$(CAMLOPT) -c $(COMPFLAGS) $*.ml

.mly.cmi:
	$(CAMLYACC) $<
	$(CAMLC) -c $(COMPFLAGS) $*.mli

.mll.ml:
	$(CAMLLEX) $<

.mly.ml:
	$(CAMLYACC) $<

clean::
	rm -f *.cm[iox] *~ .*~ *.o

depend:
	$(CAMLDEP) *.mli *.ml > .depend

include .depend
