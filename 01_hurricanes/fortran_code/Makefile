# Makefile to compile

F90 = gfortran
F90FLAGS = -cpp -O3 -g -fbounds-check
#F90FLAGS = -cpp -O3 -g -fopenmp

F90CFLAGS = -c
F90LFLAGS =

F90_COMPILE  = $(F90) $(F90FLAGS) $(F90CFLAGS)
F90_LOAD     = $(F90) $(F90FLAGS) $(F90LFLAGS)

.SUFFIXES:
.SUFFIXES: .f90 .o .mod


.f90.o:
	$(F90_COMPILE) $*.f90
.f90.mod:
	$(F90_COMPILE) $*.f90

EXECUTABLES = runProgram
OBJS = main.o \
       globals.o \
       timing.o \
       param_parser.o \
       utilities.o fileio.o

all: $(EXECUTABLES) $(SLEXE)

$(EXECUTABLES): $(OBJS)
	$(F90_LOAD) $(OBJS) -o $(EXECUTABLES)

$(SLEXE): $(OBJS)
	$(F90_LOAD) $(SOBJS) -o $(SLEXE)

main.o: globals.o timing.o param_parser.o utilities.o \
	fileio.o
param_parser.o: globals.o timing.o utilities.o
fileio.o: globals.o utilities.o
utilities.o: globals.o
globals.o: Makefile

clean:
	rm -f *.o *.mod *.d $(EXECUTABLES)

check:
	./$(EXECUTABLES) sample.param
