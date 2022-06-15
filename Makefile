FC = gfortran
FLAGS = -cpp -O2 -Wall -Wpedantic

SRCDIR = src
OBJDIR = obj
MODDIR = mod

SOURCES = mod_error.f90                   \
          mod_utility.f90                 \
          mod_graph.f90                   \
          mod_input.f90                   \
          mod_output.f90                  \
          main.f90

SRC = $(addprefix $(SRCDIR)/, $(SOURCES))
OBJ = $(addprefix $(OBJDIR)/, $(SOURCES:%.f90=%.o))
EXE = pathgen.x

# main compilation options --------------------------------
.PHONY: default
default: $(EXE)

.PHONY: fresh
fresh: clean $(EXE)

# utility -------------------------------------------------
.PHONY: clean
clean:
	@printf "Cleaning... "
	@rm -f $(OBJDIR)/*.o $(MODDIR)/*.mod
	@printf "DONE\n"

# core ----------------------------------------------------
$(EXE): $(OBJ)
	$(FC) $(FLAGS) -J$(MODDIR) -o $(EXE) $(OBJ)

$(OBJ): | $(OBJDIR)
$(OBJ): | $(MODDIR)
$(OBJ): $(OBJDIR)/%.o: $(SRCDIR)/%.f90
	$(FC) $(FLAGS) -J$(MODDIR) -c -o $@ $<

$(OBJDIR):
	mkdir -p $(OBJDIR)

$(MODDIR):
	mkdir -p $(MODDIR)

