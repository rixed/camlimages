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

#(* $Id: Makefile,v 1.12 2004/09/29 09:52:33 weis Exp $ *)

include ../../Makefile.examples

CAMLC=$(CAMLC_LABEL)
CAMLOPT=$(CAMLOPT_LABEL)

# include $(LABLGTKDIR)/config.make

LABLGTKOBJS= lablgtk.cma

COMPFLAGS= $(COMPFLAGS_CAMLIMAGES) $(LABLGTKCOMPFLAGS)
LINKFLAGS= $(COMPFLAGS) $(LINKFLAGS_CAMLIMAGES) $(LABLGTKLINKFLAGS) $(WITH_LABLGTKIMAGE) $(WITH_UNIX)

all: byt opt

SRCS= guienv.ml usleep.ml gifanim.ml

byt: gifanim.byt

gifanim.byt: $(SRCS:.ml=.cmo)
	$(CAMLC) -o gifanim.byt	$(DLLPATHS) $(LINKFLAGS) $(CSRCS:.c=.o) $(SRCS:.ml=.cmo)

opt: gifanim

gifanim: $(SRCS:.ml=.cmx)
	$(CAMLOPT) -o gifanim \
	$(LINKFLAGS:.cma=.cmxa) $(CSRCS:.c=.o) $(SRCS:.ml=.cmx)

clean::
	rm -f gifanim gifanim.byt

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly .c .o

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

.c.o:
	$(CAMLC) -c $< 

include .depend
