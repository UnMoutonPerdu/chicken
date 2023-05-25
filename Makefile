CAMLC=$(BINDIR)ocamlc
CAMLDEP=$(BINDIR)ocamldep
CAMLLEX=$(BINDIR)ocamllex
CAMLYACC=$(BINDIR)ocamlyacc
# COMPFLAGS=-w A-4-6-9 -warn-error A -g
COMPFLAGS=-g

TEST_FILES = hello.ck

COMPILE_EXEC = ckc
COMPILE_SRCS = ckast.ml printByteCode.ml compile.ml mainCompile.ml
COMPILE_GENERATED = cklex.ml ckparse.ml ckparse.mli
COMPILE_MLIS = vmBytecode.mli printByteCode.mli compile.mli
COMPILE_OBJS = $(COMPILE_GENERATED:.ml=.cmo) $(COMPILE_SRCS:.ml=.cmo)

VM_EXEC = ckrun
VM_SRCS = printByteCode.ml mem.ml vmExec.ml mainRun.ml
VM_GENERATED =
VM_MLIS = vmBytecode.mli printByteCode.mli mem.mli
VM_OBJS = $(VM_GENERATED:.ml=.cmo) $(VM_SRCS:.ml=.cmo)

# Building the world
all: $(VM_EXEC) $(COMPILE_EXEC)

$(VM_EXEC): $(COMMON_OBJS) $(VM_OBJS)
	$(CAMLC) $(COMPFLAGS) $(COMMON_OBJS) $(VM_OBJS) -o $(VM_EXEC)

$(COMPILE_EXEC): $(COMPILE_OBJS)
	$(CAMLC) $(COMPFLAGS) $(COMPILE_OBJS) -o $(COMPILE_EXEC)


.PHONY: tests
tests: $(COMPILE_EXEC) $(VM_EXEC) $(TEST_FILES)
	@for i in $(TEST_FILES); do \
		echo "PROCESSING: $$i" ; \
		./$(COMPILE_EXEC) $$i > /dev/null ; \
	  err=$$?; \
	  case $$err in \
	 		0);; \
	 	  *) echo "*** !!! BROKEN COMPILATION: $$i" ; \
	  esac; \
		./$(VM_EXEC) a.out ;\
	  err=$$?; \
	  case $$err in \
	 		0);; \
	 	  *) echo "*** !!! BROKEN EXECUTION: $$i" ; \
	  esac; \
	done

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx
.SUFFIXES: .mll .mly

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.mll.ml:
	$(CAMLLEX) $<

.mly.ml:
	$(CAMLYACC) $<

# Clean up
clean:
	rm -f tarball-enonce.tgz tarball-solution.tgz
	rm -f *.cm[io] *.cmx *~ .*~ *.o
	rm -f $(COMPILE_GENERATED) $(VM_GENERATED)
	rm -f $(LOOP_EXEC) $(COMPILE_EXEC) $(VM_EXEC)
	rm -f tarball-enonce.tgz tarball-solution.tgz

# Dependencies
depend: $(COMPILE_SRCS) $(COMPILE_GENERATED) $(COMPILE_MLIS) \
				$(VM_SRCS) $(VM_GENERATED) $(VM_MLIS)
	$(CAMLDEP) $? > .depend

include .depend


tarball-enonce:
	rm -f tarball-enonce.tgz
	tar cvzhf tarball-enonce.tgz \
		std.pdf $(TEST_FILES) Makefile printByteCode.ml printByteCode.mli ckast.ml \
		cklex.mll ckparse.mly mainCompile.ml vmBytecode.mli \
		compile.mli mainRun.ml vmExec.ml mem.ml mem.mli compile-eleves.ml *.ck

tarball-solution:
	rm -f tarball-solution.tgz
	tar cvzhf tarball-solution.tgz \
		ctd.pdf $(TEST_FILES) Makefile printByteCode.ml printByteCode.mli ckast.ml \
		cklex.mll ckparse.mly mainCompile.ml vmBytecode.mli \
		compile.mli mainRun.ml vmExec.ml mem.ml mem.mli compile.ml *.ck
