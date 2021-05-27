#Compilador Tupa
#CC=ftn 

#Compilador 
FC = mpif90

# Opcoes de compilacao
FFLAGS = -c -O3 -g -w

# Opcoes de otimizacao
FFLAGSOPT = -O3 -g -w

VPATH = src
MODDIR = mod
BUILDDIR = build

# Arquivos objeto do annMPCA
SRCMPCA := $(BUILDDIR)/foul.o \
$(BUILDDIR)/uniformR8.o \
$(BUILDDIR)/newTypes.o \
$(BUILDDIR)/normalR8.o \
$(BUILDDIR)/annTraining.o \
$(BUILDDIR)/mpcaFunctions.o \
$(BUILDDIR)/mpca.o

# Arquivos objeto do annTest
SRCMLP := $(BUILDDIR)/foul.o \
$(BUILDDIR)/newTypes.o \
$(BUILDDIR)/annGeneralization.o \
$(BUILDDIR)/main_generalization.o

# Arquivos objeto do annActivation
SRCACTIVATION := $(BUILDDIR)/foul.o \
$(BUILDDIR)/newTypes.o \
$(BUILDDIR)/annActivation.o \
$(BUILDDIR)/main_activation.o


all: 	$(BUILDDIR)/foul.o \
	$(BUILDDIR)/newTypes.o \
	$(BUILDDIR)/uniformR8.o \
	$(BUILDDIR)/normalR8.o \
	$(BUILDDIR)/annTraining.o \
	$(BUILDDIR)/mpcaFunctions.o \
	$(BUILDDIR)/mpca.o \
	$(BUILDDIR)/annActivation.o \
	$(BUILDDIR)/annGeneralization.o \
	$(BUILDDIR)/main_generalization.o \
	$(BUILDDIR)/main_activation.o \
	annMPCA \
	annTest \
	annActivation

annMPCA:
	$(FC) $(FFLAGSOPT) -o annMPCA $(SRCMPCA)

annTest:
	$(FC) $(FFLAGSOPT) -o annTest $(SRCMLP)

annActivation:
	$(FC) $(FFLAGSOPT) -o annActivation $(SRCACTIVATION)

$(BUILDDIR)/%.o: $(VPATH)/%.f90
	@mkdir -p $(@D)
	$(FC) $(FFLAGS) $< -o $@

clean:	removemod
	rm -rf *.*~ Makefile~ build/*.o *.mod annActivation annTest annMPCA

removemod:
	rm -f build/*.o *.mod
