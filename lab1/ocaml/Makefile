SUBDIRS = util,parse,trans,type,codegen,top
LIBS = str
MAIN = go

OCAMLBUILD = ocamlbuild
OCAMLBUILDFLAGS = 

DEBUG = $(MAIN).d.byte
NATIVE = $(MAIN).native
TOPLEVEL = $(MAIN).top

all: debug

l1c: bin/l1c
bin/l1c: native
	install $(NATIVE) $@

debug: $(DEBUG)
native: $(NATIVE)
top: $(TOPLEVEL) 

OCAMLOPTS = $(OCAMLBUILDFLAGS) -Is $(SUBDIRS) -libs $(LIBS)
OCAMLBRUN = $(OCAMLBUILD) $(OCAMLOPTS)

%.native: always
	$(OCAMLBRUN) $@
%.d.byte: always
	$(OCAMLBRUN) -cflags -annot $@
top: always
	$(OCAMLBRUN) $@

clean:
	$(OCAMLBUILD) -Is $(SUBDIRS) -clean
	@echo			# $(OCAMLBUILD) -clean needs a newline
	rm -f bin/l1c

always:

.PHONY: l1c debug native top clean always
