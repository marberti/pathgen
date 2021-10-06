FC = gfortran

FLAGS = -cpp -Wall -Wpedantic

SRC = $(wildcard *.f90)
OBJ = $(SRC:%.f90=%.o)
EXE = pathgen.x

.PHONY: default
default: $(EXE)

.PHONY: clean
clean:
	@printf "Cleaning... "
	@rm -f *.o *.mod
	@printf "DONE\n"

$(EXE): $(OBJ)
	$(FC) $(FLAGS) $(OBJ) -o $(EXE)

$(OBJ): %.o: %.f90
	$(FC) $(FLAGS) -c $<

