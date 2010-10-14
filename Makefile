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

#(* $Id: Makefile,v 1.78 2004/10/02 15:49:05 weis Exp $ *)

include Makefile.config
include Makefile.build

PACKAGE=camlimages
MAINVERSION=2
SUBVERSION=2
PATCHLEVEL=0
VERSION=$(MAINVERSION).$(SUBVERSION)
FULLVERSION=$(VERSION).$(PATCHLEVEL)
OLDVERSION=2.12
FULLOLDVERSION=2.12.0

PACKAGEVERSIONFILES=configure.in
DOCVERSIONFILES=doc/eng.htm

CVSRELEASETAG=$(PACKAGE)-$(MAINVERSION)_$(SUBVERSION)_$(PATCHLEVEL)
ANNOUNCEFILE=Announce-$(FULLVERSION)

EXAMPLES= converter crop edgedetect gifanim imgstat liv monochrome \
	   normalize resize tiffps ttfimg

all: byt opt

.PHONY: examples install

examples: examples.byt examples.opt

examples.byt:
	for i in $(EXAMPLES); do (cd "examples/$$i"; $(MAKE) byt); done

examples.opt:
	for i in $(EXAMPLES); do (cd "examples/$$i"; $(MAKE) opt); done

examples.clean:
	for i in $(EXAMPLES); do (cd "examples/$$i"; $(MAKE) -k clean); done

config:
	$(RM) config.cache
	./configure

clean::
	$(RM) *~
	for i in $(BUILDDIRS); do (cd $$i; $(MAKE) clean) || exit $$?; done
	cd test; $(MAKE) clean

veryclean: clean
	$(RM) config.cache config.log config.status config.h \
		$(COREDIR)/camlimages.ml
	$(MAKE) examples.clean

veryveryclean: veryclean
	$(RM) configure
	$(RM) autoconf.info */*~
	$(RM) Makefile.config Makefile.fortools

byt:
	for i in $(BUILDDIRS); do (cd $$i; $(MAKE) byt) || exit $$?; done

opt:
	for i in $(BUILDDIRS); do (cd $$i; $(MAKE) opt) || exit $$?; done

top:
	$(CAMLMKTOP) $(CUSTOM) -o customtop \
		$(COMPFLAGS_CAMLIMAGES) \
		$(WITH_UNIX) \
		$(WITH_CAMLIMAGES) \
		$(WITH_GRAPHICS) \
		$(WITH_GIF) \
		$(WITH_JPEG) \
		$(WITH_TIFF) \
		$(WITH_BMP) \
		$(WITH_PNG) \
		$(WITH_XPM) \
		$(WITH_FREETYPE)

installopt: install

install: all
	mkdir -p $(LIBDIR)
	if test -w $(CAMLDIR)/ld.conf \
        && test `grep -s -c '^$(LIBDIR)$$' $(CAMLDIR)/ld.conf` = 0; then \
	    echo $(LIBDIR) >> $(CAMLDIR)/ld.conf; \
	fi
	for i in $(BUILDDIRS); do (cd $$i; $(MAKE) install) || exit $$?; done
	$(CP) Makefile.config $(LIBDIR)
	$(CP) config.h $(LIBDIR)
	$(RANLIB) $(LIBDIR)/*.a

depend:
	for i in $(BUILDDIRS); do (cd $$i; $(MAKE) depend) || exit $$?; done

# Automatic handling of versionning
version:
	for i in $(PACKAGEVERSIONFILES); do \
	echo $$i; \
	$(MV) $$i $$i~; \
	sed -e '/CAMLIMAGES_VERSION/s/$(OLDVERSION)/$(VERSION)/' $$i~ > $$i; \
	done
	for i in  $(DOCVERSIONFILES); do \
	echo $$i; \
	$(MV) $$i $$i~; \
	sed -e '/Version/s/$(OLDVERSION)/$(VERSION)/' $$i~ | \
	sed -e '/ftp.inria.fr/s/$(FULLOLDVERSION)/$(FULLVERSION)/' > $$i; \
	done

distribution: all documentation
	$(MAKE) -f Makefile.distrib distribute

documentation:
	cd doc; $(MAKE) all

release:
	cvs commit -m 'Release $(VERSION)'
	cvs rtag -R $(CVSRELEASETAG) bazar-ocaml/$(PACKAGE)

unrelease:
	$(RM) ./release
	cvs rtag -R -d $(CVSRELEASETAG) bazar-ocaml/$(PACKAGE)

announce:
	mail -n -s "New release $(VERSION) of $(PACKAGE)" \
		caml-announce@inria.fr < $(ANNOUNCEFILE)

package_distribution: release distribution announce
