# Makefile
# Author: Ben Hamme (bhamme)
# Author: Adam Mihalcin (amihalci)
# 15-411 Fall 2012
#
# This Makefile has all the necessary information to compile the compiler
# on both AFS and Autolab.  It automatically detects whether or not
# make is running on Autolab, and chooses the source paths accordingly.
#
# This Makefile is divided into four sections: general variables, file
# variables, build rules, and clean rules.  Feel free to change whatever
# you like, but if you stick to this structure you get a lot of
# benefits for free.
#
# General Variables - shouldn't have to touch these unless you install
#                     Mono and/or F#
# File Variables - add dependencies (DLLs without path name, F# files
#                  with path name relative to Makefile's directory)
#   - You can also add a new DLL by adding a new variable here, and
#     then updating the build rules with a new DLL in both the bin and
#     debug sections of the build rules.
# Build Rules - leave alone unless you add a new DLL, or write code that
#               depends on a F# PowerPack DLL or other DLL outside the
#               directory where this Makefile is located.  See
#               Lab1.Parse.dll for an example of referring to an
#               external DLL - the $(REF_FSPP) variable expands to
#               "-r FSharp.PowerPack.dll".
# Clean Rules - you shouldn't have to touch these
#
#
# ----------------------------------------------------------------------
# A note from amihalci:
#
# The AFS path uses Mono in the amihalci public directory.
# I intend to keep this public directory available indefinitely, but CMU
# may eventually close down my Andrew account, or there may be a
# sufficient benefit in a later version of Mono or F# to justify moving to
# a new version.  In that case, you will have to install F# yourself,
# since AFS doesn't include Mono and F# by default.
#
# If you choose to install F# yourself, you will have to install three
# things: Mono (the Linux version of the .NET framework), the F# compiler,
# and the F# PowerPack (parsing utilities and some nice collections,
# among other useful modules).  The InstallingFSharp.pdf file in this
# directory has instructions for installing all three of these.  Also,
# you will need to change the AFS_TOOLDIR variable below, and the
# directory references in bin/l1c and debug/l1c to match the directory
# where you installed all the tools.  If you have any questions, try
# emailing amihalci@andrew.cmu.edu, and hopefully CMU will still be
# forwarding my mail to a place where I read your question (remember,
# you only need to install F# in the first place if my public directory
# becomes unavailable, or a new version of Mono or F# is released with
# much better features).
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
# General Variables
#
# You shouldn't need to change any variable except AFS_TOOLDIR if you
# install Mono and F# yourself, and otherwise shouldn't need to touch
# this file.
# ----------------------------------------------------------------------

MONO = mono
MCS = dmcs
FSC = fsharpc
REF_FSPP = -r FSharp.PowerPack.dll
REF_FSPP_PS = -r FSharp.PowerPack.Parallel.Seq.dll

AFS_TOOLDIR = /afs/andrew/usr/amihalci/public
NOT_AUTOLAB = $(if $(wildcard $(AFS_TOOLDIR)),TRUE)
ifneq ($(strip $(NOT_AUTOLAB)),)
	TOOLDIR = $(AFS_TOOLDIR)
	INCLUDEFLAGS = -I $(TOOLDIR)/powerpack/bin
	FSLEX = $(MONO) $(TOOLDIR)/FSharpPowerPack-2.0.0.0/bin/fslex.exe
	FSYACC = $(MONO) $(TOOLDIR)/FSharpPowerPack-2.0.0.0/bin/fsyacc.exe
else
	TOOLDIR = /usr/local/lib/fsharp-r54075
	INCLUDEFLAGS =
	FSLEX = $(MONO) $(TOOLDIR)/bin/fslex.exe
	FSYACC = $(MONO) $(TOOLDIR)/bin/fsyacc.exe
endif

FSFLAGS = $(INCLUDEFLAGS) --nologo --warn:4 --warnaserror+
DEBUGFLAGS = $(FSFLAGS) -g -d:DEBUG --checked+

.PHONY: all test
all: l1c

test: l1c
	./driver.pl -q ../tests/*.l?

.PHONY: l1c
l1c: bin/Lab1.exe


# ----------------------------------------------------------------------
# File Variables
# 
# These are the dependencies for each output file.  A file's dependencies
# have the same name as the file, but with all letters converted to
# uppercase and all dots replaced with underscores.  The DLL dependencies
# have no path, but the source-code dependencies have the relative
# path included.  The reason the DLL dependencies have no path
# included is that each output file is built twice: bin/l1c executes
# bin/Lab1.exe, which is built to depend on the DLLs in the bin directory,
# while debug/l1c executes debug/Lab1.exe, which is built to depend on
# the DLLs in the debug directory.  The actual make targets perform the
# necessary path joining to turn any pathless DLL dependency into a
# dependency with the correct path.
# ----------------------------------------------------------------------

LAB1_EXE = Lab1.CodeGen.dll Lab1.Parse.dll Lab1.Trans.dll Lab1.Type.dll \
			Lab1.Util.dll top/Top.fs

LAB1_CODEGEN_DLL = Lab1.Trans.dll Lab1.Util.dll \
			codegen/InterRep.fs codegen/Codegen.fs

LAB1_PARSE_DLL = Lab1.Util.dll parse/Ast.fs parse/Parser.fsi \
			parse/Parser.fs parse/Lex.fs parse/FileParser.fs

LAB1_TRANS_DLL = Lab1.Parse.dll Lab1.Type.dll Lab1.Util.dll \
			trans/Temp.fs trans/Tree.fs trans/Trans.fs

LAB1_TYPE_DLL = Lab1.Parse.dll Lab1.Util.dll type/TypeChecker.fs

LAB1_UTIL_DLL = Mono.Options.dll util/Mark.fs util/ErrorMsg.fs \
			util/Flags.fs util/Order.fs util/Symbol.fs


# ----------------------------------------------------------------------
# Build Rules
#
# The targets beginning with bin are used when the bin/l1c script is
# executed, and the targets beginning with bin are used when the debug/l1c
# script is executed.  Look in the general variables section for the
# DEBUGFLAGS variable, which contains all the flags used in normal
# computation plus "-g -d:DEBUG --checked+".  The -g flag includes
# debugging symbols in the final binary, and the -d:DEBUG flag defines the
# symbol DEBUG to be used in any conditional compilation directives
# you desire.  The --checked+ flag turns on checked math i.e. an
# exception is thrown on any overflow.  This can come in handy to
# make sure that overflow is not causing bugs in your compiler, since
# your compiler will fail with an exception on overflow, but you probably
# don't want checked math when optimizing integer expressions that
# may overflow.  We recommend creating one DLL for optimization modules,
# and adding the --checked- flag *after* the DEBUGFLAGS variable in the
# debug/LabX.Optimize.dll target.  The --checked- will override the
# earlier --checked+, which is probably correct for your optimization
# library because F# follows the same integer arithmetic rules as C0.
# ----------------------------------------------------------------------

bin/Lab1.exe: $(patsubst %.dll, bin/%.dll, $(LAB1_EXE))
	$(FSC) $(FSFLAGS) -o $@ $(patsubst %.dll,-r %.dll,$^)

bin/Lab1.CodeGen.dll: $(patsubst %.dll, bin/%.dll, $(LAB1_CODEGEN_DLL))
	$(FSC) $(FSFLAGS) -a -o $@ $(REF_FSPP) $(patsubst %.dll,-r %.dll,$^)

bin/Lab1.Parse.dll: $(patsubst %.dll, bin/%.dll, $(LAB1_PARSE_DLL))
	$(FSC) $(FSFLAGS) -a -o $@ $(REF_FSPP) $(patsubst %.dll,-r %.dll,$^)

bin/Lab1.Trans.dll: $(patsubst %.dll, bin/%.dll, $(LAB1_TRANS_DLL))
	$(FSC) $(FSFLAGS) -a -o $@ $(patsubst %.dll,-r %.dll,$^)

bin/Lab1.Type.dll: $(patsubst %.dll, bin/%.dll, $(LAB1_TYPE_DLL))
	$(FSC) $(FSFLAGS) -a -o $@ $(REF_FSPP_PS) $(patsubst %.dll,-r %.dll,$^)

bin/Lab1.Util.dll: $(patsubst %.dll, bin/%.dll, $(LAB1_UTIL_DLL))
	$(FSC) $(FSFLAGS) -a -o $@ $(patsubst %.dll,-r %.dll,$^)

bin/Mono.Options.dll: external/Options.cs
	$(MCS) -d:LINQ -t:library -out:$@ $^
	chmod -x $@


.PHONY: debug
debug: debug/Lab1.exe

debug/Lab1.exe: $(patsubst %.dll, debug/%.dll, $(LAB1_EXE))
	$(FSC) $(DEBUGFLAGS) -o $@ $(patsubst %.dll,-r %.dll,$^)

debug/Lab1.CodeGen.dll: $(patsubst %.dll, debug/%.dll, $(LAB1_CODEGEN_DLL))
	$(FSC) $(DEBUGFLAGS) -a -o $@ $(REF_FSPP) $(patsubst %.dll,-r %.dll,$^)

debug/Lab1.Parse.dll: $(patsubst %.dll, debug/%.dll, $(LAB1_PARSE_DLL))
	$(FSC) $(DEBUGFLAGS) -a -o $@ $(REF_FSPP) $(patsubst %.dll,-r %.dll,$^)

debug/Lab1.Trans.dll: $(patsubst %.dll, debug/%.dll, $(LAB1_TRANS_DLL))
	$(FSC) $(DEBUGFLAGS) -a -o $@ $(patsubst %.dll,-r %.dll,$^)

debug/Lab1.Type.dll: $(patsubst %.dll, debug/%.dll, $(LAB1_TYPE_DLL))
	$(FSC) $(DEBUGFLAGS) -a -o $@ $(REF_FSPP_PS) $(patsubst %.dll,-r %.dll,$^)

debug/Lab1.Util.dll: $(patsubst %.dll, debug/%.dll, $(LAB1_UTIL_DLL))
	$(FSC) $(DEBUGFLAGS) -a -o $@ $(patsubst %.dll,-r %.dll,$^)

debug/Mono.Options.dll: external/Options.cs
	$(MCS) -debug -d:LINQ -t:library -out:$@ $^
	chmod -x $@

parse/Lex.fs: parse/Lex.fsl
	$(FSLEX) --unicode $^

# See http://stackoverflow.com/a/3077254 for the GNU Make magic here
parse/%.fs parse/%.fsi: parse/%.fsy
	$(FSYACC) --module Lab1.Parse.Parser $^


# ----------------------------------------------------------------------
# Clean Rules
# ----------------------------------------------------------------------

.PHONY: cleanbin cleandebug clean veryclean

cleanbin:
	rm -f bin/*.dll bin/*.exe bin/*.mdb

cleandebug:
	rm -f debug/*.dll debug/*.exe debug/*.mdb

clean: cleanbin cleandebug
	rm -f parse/Lex.fs parse/Parser.fsi parse/Parser.fs
	rm -f parse/Parser.fsyacc.output

veryclean: clean
	rm -f driver.pl *.pm
	rm -f l4rt.c l4rt.h0

