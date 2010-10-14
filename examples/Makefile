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

#(* $Id: Makefile,v 1.3 2004/10/01 15:57:58 weis Exp $ *)

SUBDIRS = crop normalize tiffps \
converter edgedetect imgstat monochrome resize ttfimg \
gifanim liv

all: byt opt

byt:
	for i in $(SUBDIRS); do \
	 cd $$i; $(MAKE) byt; cd ..; \
	done

opt:
	for i in $(SUBDIRS); do \
	 cd $$i; $(MAKE) opt; cd ..; \
	done

clean:
	for i in $(SUBDIRS); do \
	 cd $$i; $(MAKE) clean; cd ..; \
	done
