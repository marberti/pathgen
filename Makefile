FC = gfortran

FLAGS = -cpp -O2 -Wall -Wpedantic

SRC = mod_error.f90   \
      mod_utility.f90 \
      mod_graph.f90   \
      mod_input.f90   \
      mod_output.f90  \
      main.f90

OBJ = $(SRC:%.f90=%.o)
EXE = pathgen.x

.PHONY: default
default: clean $(EXE)

.PHONY: quickbuild
quickbuild: $(EXE)

.PHONY: clean
clean:
	@printf "Cleaning... "
	@rm -f *.o *.mod
	@printf "DONE\n"

$(EXE): $(OBJ)
	$(FC) $(FLAGS) $(OBJ) -o $(EXE)

$(OBJ): %.o: %.f90
	$(FC) $(FLAGS) -c $<

