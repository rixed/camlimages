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

#(* $Id: Makefile,v 1.31 2004/09/29 09:52:34 weis Exp $ *)

include ../../Makefile.examples

LIVDIR=/usr/local/lib/liv
BINDIR=/usr/local/bin

CAMLC_NOLABEL=ocamlc
CAMLOPT_NOLABEL=ocamlopt
CAMLC=$(CAMLC_LABEL) -w s
CAMLOPT= $(CAMLOPT_LABEL) -unsafe -w s

LABLGTK2OBJS= lablgtk.cma
LABLGTK2COMPFLAGS= -I $(LABLGTK2DIR)
LABLGTK2LINKFLAGS= $(LABLGTK2OBJS) -ccopt "-L$(LABLGTK2DIR)" -cclib "-llablgtk2 $(GTKLIBS)"

COMPFLAGS= $(COMPFLAGS_CAMLIMAGES) $(COMPFLAGS_LABLGTK2IMG) $(LABLGTK2COMPFLAGS)
LINKFLAGS= $(COMPFLAGS) $(LINKFLAGS_CAMLIMAGES) $(LABLGTK2LINKFLAGS) $(WITH_LABLGTK2IMAGE) $(WITH_UNIX)

all: byt opt

MODULES= deficon mylazy cache livmisc gui jWidget seq tout enhance display \
	 viewer edge \
	 pathfind iconcap livshtype icon livsh scandir \
	 jpf liv

SRCS= $(addsuffix .ml, $(MODULES))

byt: liv.byt

liv.byt: $(SRCS:.ml=.cmo)
	$(CAMLC) -o liv.byt $(DLLPATHS) $(LINKFLAGS) $(SRCS:.ml=.cmo)

opt: liv

liv: $(SRCS:.ml=.cmx)
	$(CAMLOPT) -o liv $(LINKFLAGS:.cma=.cmxa) $(SRCS:.ml=.cmx)

install: opt
	- mkdir $(LIVDIR)
	cp liv $(BINDIR)/liv
	cp *.xpm *.jpg $(LIVDIR)
	cp iconcap $(LIVDIR)

clean::
	rm -f liv.byt liv

liv.ml:
	if test -f liv-jpf.ml; then \
	  ln -s liv-jpf.ml liv.ml; \
	else \
	  ln -s liv-org.ml liv.ml; \
	fi

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

enhance.cmo: enhance.ml
	$(CAMLC_NOLABEL) -c $(COMPFLAGS) $<

enhance.cmx: enhance.ml
	$(CAMLOPT_NOLABEL) -c $(COMPFLAGS) $<

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
