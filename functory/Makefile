##########################################################################
#                                                                        #
#  Functory: a distributed computing library for Ocaml                   #
#  Copyright (C) 2010 Jean-Christophe Filliatre and Kalyan Krishnamani   #
#                                                                        #
#  This software is free software; you can redistribute it and/or        #
#  modify it under the terms of the GNU Library General Public           #
#  License version 2.1, with the special exception on linking            #
#  described in file LICENSE.                                            #
#                                                                        #
#  This software is distributed in the hope that it will be useful,      #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  #
#                                                                        #
##########################################################################

# where to install the binaries
prefix=/usr/local
exec_prefix=${prefix}
BINDIR=${exec_prefix}/bin

# where to install the man page
MANDIR=${prefix}/share/man

# other variables set by ./configure
OCAMLC   = ocamlc.opt
OCAMLOPT = ocamlopt.opt -g -annot
OCAMLDEP = ocamldep.opt
OCAMLLEX = ocamllex.opt
OCAMLYACC= ocamlyacc
OCAMLLIB = /usr/lib/ocaml
OCAMLBEST= opt
OCAMLVERSION = 4.02.3
OCAMLWEB = true
OCAMLWIN32 = no
EXE = 
LIBEXT = .a
OBJEXT = .o

INCLUDES = 
BFLAGS = -dtypes -g $(INCLUDES)
OFLAGS = -dtypes $(INCLUDES) -for-pack Functory

# main target
#############

NAME = functory

CMA  = functory.cma
CMXA = functory.cmxa

TESTDIRS = n-queens sums sort sort-nfs
TESTS := $(patsubst %, tests/%/a.out, $(TESTDIRS))

all: $(OCAMLBEST) 
byte: $(CMA) 
opt: $(CMA) $(CMXA) 

# bytecode and native-code compilation
######################################

CMO = control.cmo sequential.cmo map_fold.cmo \
      cores.cmo binary.cmo protocol.cmo network.cmo 
CMX = $(CMO:.cmo=.cmx)

GENERATED = version.ml

$(CMA): functory.cmo
	$(OCAMLC) -a $(BFLAGS) -o $@ $^

$(CMXA): functory.cmx
	$(OCAMLOPT) -a $(OFLAGS) -o $@ $^

functory.cmo: functory.cmi
functory.cmo: $(CMO)
	$(OCAMLC) $(BFLAGS) -pack -o $@ $(CMO)

functory.cmx: functory.cmi
functory.cmx: $(CMX)
	$(OCAMLOPT) $(INCLUDES) -pack -o $@ $(CMX)

NTCMO = network_test.cmo
NTCMX = $(NTCMO:.cmo=.cmx)

network_test.byte: $(CMA) $(NTCMO)
	$(OCAMLC) $(INCLUDES) -o $@ unix.cma $^

network_test.cmx: functory.cmi
network_test.opt: $(CMXA) $(NTCMX)
	$(OCAMLOPT) $(INCLUDES) -o $@ unix.cmxa $(CMXA) $(NTCMX)

WTCMO = worker_test.cmo
WTCMX = $(WTCMO:.cmo=.cmx)

worker_test.byte: $(CMA) $(WTCMO)
	$(OCAMLC) $(INCLUDES) -o $@ unix.cma $^

worker_test.cmx: functory.cmi
worker_test.opt: $(CMXA) $(WTCMX)
	$(OCAMLOPT) $(INCLUDES) -o $@ unix.cmxa $(CMXA) $(WTCMX)

MTCMO = master_test.cmo
MTCMX = $(MTCMO:.cmo=.cmx)

master_test.byte: $(CMA) $(MTCMO)
	$(OCAMLC) $(BFLAGS) -o $@ unix.cma $^

master_test.opt: $(CMXA) $(MTCMX)
	$(OCAMLOPT) $(INCLUDES) -o $@ unix.cmxa $^

TCMO = test.cmo
TCMX = $(TCMO:.cmo=.cmx)

test.byte: $(CMA) $(TCMO)
	$(OCAMLC) $(BFLAGS) -o $@ unix.cma $^

test.opt: $(CMXA) $(TCMX)
	$(OCAMLOPT) $(INCLUDES) -o $@ unix.cmxa $^

VERSION=0.5

version.ml: Makefile
	echo "let version = \""$(VERSION)"\"" > version.ml
	echo "let date = \""`date`"\"" >> version.ml

# tests
#######

tests: $(TESTS)

test: test.opt 
	WORKER=yes xterm -e "./test.opt" &
	sleep 1 && ./test.opt

test-network: network_test.opt 
	sh ./test-network.sh

test2: master_test.opt worker_test.opt
	./worker_test.opt &
	sleep 1 && ./master_test.opt && killall -9 worker_test.opt

install-test: tests/fault/a.out
	cp tests/fault/a.out ~

install-queens: tests/n-queens/queens64.opt
	cp tests/n-queens/queens64.opt ~

install-data-test: tests/data-test/a.out
	cp tests/data-test/a.out ~/data.opt

install-mandelbrot: tests/mandelbrot/a.out
	cp tests/mandelbrot/a.out ~/mandelbrot.opt

install-provers: tests/provers/a.out
	cp tests/provers/a.out ~/disco-provers.opt

tests/%/a.out: tests/%/main.ml functory.cmxa
	$(OCAMLOPT) graphics.cmxa unix.cmxa functory.cmxa tests/$*/main.ml -o $@

tests/%.gtk2: tests/%.ml functory.cmxa
	$(OCAMLOPT) -I +lablgtk2 lablgtk.cmxa lablgnomecanvas.cmxa unix.cmxa functory.cmxa tests/$*.ml -o $@

tests/%.opt: tests/%.ml functory.cmxa
	$(OCAMLOPT) graphics.cmxa unix.cmxa functory.cmxa tests/$*.ml -o $@

tests/pasco/%: tests/pasco/poly.ml tests/pasco/dpoly.ml tests/pasco/%.ml functory.cmxa
	$(OCAMLOPT) -I +creal -I tests/pasco/ creal.cmxa unix.cmxa functory.cmxa tests/pasco/poly.ml tests/pasco/dpoly.ml tests/pasco/$*.ml -o $@

install-pasco: tests/pasco/matmul
	cp tests/pasco/matmul ~

tests/n-queens/queens64.opt: tests/n-queens/main64.ml functory.cmxa
	$(OCAMLOPT) unix.cmxa functory.cmxa tests/n-queens/main64.ml -o $@

wc:
	ocamlwc -p *.ml* tests/*/*.ml*

# installation
##############

INSTALL_LIBDIR=$(DESTDIR)$(OCAMLLIB)/functory

install: install-$(OCAMLBEST)

install-byte: 
	mkdir -p $(INSTALL_LIBDIR)
	cp -f functory.mli functory.cmo functory.cmi $(CMA) $(INSTALL_LIBDIR)

install-opt: 
	mkdir -p $(INSTALL_LIBDIR)
	cp -f functory.mli functory.cmo functory.cmi $(CMA) $(INSTALL_LIBDIR)
	cp -f functory$(LIBEXT) functory.cmx $(CMXA) $(INSTALL_LIBDIR)

ocamlfind-install:
	ocamlfind install functory META $(BUILD)functory.cmi $(CMA) $(CMXA) $(BUILD)functory.a

ocamlfind-remove:
	ocamlfind remove functory

# export
########

EXPORTDIR=$(NAME)-$(VERSION)
TAR=$(EXPORTDIR).tar

WWW = $$HOME/WWW/$(NAME)

FILES = *.mli *.ml tests/*/*.ml \
	Makefile.in configure configure.in   \
	.depend README.txt LICENSE CHANGES META

export: source export-www export-doc
	cp README.txt LICENSE CHANGES $(WWW)

source: 
	mkdir -p export/$(EXPORTDIR)
	cp --parents $(FILES) export/$(EXPORTDIR)
	cd export ; tar cf $(TAR) $(EXPORTDIR) ; gzip -f --best $(TAR)
	cp export/$(TAR).gz $(WWW)/download/

export-doc: functory.mli
	mkdir -p $(WWW)/doc/
	ocamldoc -html -hide Functory -d  $(WWW)/doc/ functory.mli

WWW_FILES=*.html *.xml Media Scripts About_files Mini-Tutorial_files

export-www:
	cd www; cp -rf $(WWW_FILES) $(WWW)

BINARY = $(EXPORTDIR)-$(OSTYPE)
BINARYTAR=$(BINARY).tar

BINARYFILES = README INSTALL COPYING GPL $(NAME).1

binary: $(NAME).$(OCAMLBEST) $(DOCFILES)
	mkdir -p export/$(BINARY)
	cp $(BINARYFILES) $(DOCFILES) export/$(BINARY)
	cp $(NAME).$(OCAMLBEST) export/$(BINARY)/$(NAME)
	(cd export; tar czf $(BINARY).tar.gz $(BINARY))
	cp export/$(BINARY).tar.gz $(FTP)

# generic rules
###############

.SUFFIXES: .mli .ml .cmi .cmo .cmx .mll .mly .tex .dvi .ps .html

.mli.cmi:
	$(OCAMLC) -c $(BFLAGS) $<

.ml.cmo:
	$(OCAMLC) -c $(BFLAGS) $<

.ml.o:
	$(OCAMLOPT) -c $(OFLAGS) $<

.ml.cmx:
	$(OCAMLOPT) -c $(OFLAGS) $<

.mll.ml:
	$(OCAMLLEX) $<

.mly.ml:
	$(OCAMLYACC) -v $<

.mly.mli:
	$(OCAMLYACC) -v $<

.tex.dvi:
	latex $< && latex $<

.dvi.ps:
	dvips $< -o $@ 

.tex.html:
	hevea $<

# headers
#########

headers:
	headache -c misc/headache_config.txt -h misc/header.txt \
	 *.in README.txt *.mli *.ml tests/*/*.ml
	./config.status

# Emacs tags
############

tags:
	find . -name "*.ml*" | sort -r | xargs \
	etags "--regex=/let[ \t]+\([^ \t]+\)/\1/" \
	      "--regex=/let[ \t]+rec[ \t]+\([^ \t]+\)/\1/" \
	      "--regex=/and[ \t]+\([^ \t]+\)/\1/" \
	      "--regex=/type[ \t]+\([^ \t]+\)/\1/" \
              "--regex=/exception[ \t]+\([^ \t]+\)/\1/" \
	      "--regex=/val[ \t]+\([^ \t]+\)/\1/" \
	      "--regex=/module[ \t]+\([^ \t]+\)/\1/"

# Makefile is rebuilt whenever Makefile.in or configure.in is modified
######################################################################

Makefile: Makefile.in config.status
	./config.status

config.status: configure
	./config.status --recheck

configure: configure.in
	autoconf 

# clean
#######

clean::
	rm -f *.cm[iox] *.o *~ *.annot
	rm -f $(GENERATED) parser.output
	rm -f $(NAME).byte $(NAME).opt
	rm -f master_test.byte master_test.opt
	rm -f worker_test.byte worker_test.opt
	rm -f network_test.byte network_test.opt
	rm -f *.aux *.log $(NAME).tex $(NAME).dvi $(NAME).ps

dist-clean distclean:: clean
	rm -f Makefile config.cache config.log config.status

# depend
########

.depend depend:: $(GENERATED)
	rm -f .depend
	$(OCAMLDEP) $(INCLUDES) *.ml *.mli > .depend

include .depend
