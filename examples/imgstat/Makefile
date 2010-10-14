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

#(* $Id: Makefile,v 1.5 2004/09/29 09:52:34 weis Exp $ *)

include ../../Makefile.examples

COMPFLAGS= $(COMPFLAGS_CAMLIMAGES)
LINKFLAGS= $(COMPFLAGS) $(LINKFLAGS_CAMLIMAGES)

all: byt opt

SRCS= imgstat.ml

byt: imgstat.byt

imgstat.byt: $(SRCS:.ml=.cmo)
	$(CAMLC) -o imgstat.byt $(DLLPATHS) $(LINKFLAGS) $(SRCS:.ml=.cmo)

opt: imgstat

imgstat: $(SRCS:.ml=.cmx)
	$(CAMLOPT) -o imgstat $(LINKFLAGS:.cma=.cmxa) $(SRCS:.ml=.cmx)

clean::
	rm -f imgstat imgstat.byt

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
