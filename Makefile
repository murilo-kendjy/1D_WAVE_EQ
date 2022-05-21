
F90 = gfortran
FC =  gfortran

# F90FLAGS = -O3 -Wall -Waliasing -pedantic -Warray-bounds -Wsurprising -Wunderflow -fcheck=all -std=f2008
FFLAGS = -O3 -Wall -Waliasing -pedantic -Warray-bounds -Wsurprising -Wunderflow -fcheck=all -std=f2008

# Link options
LDFLAGS = -s

.SUFFIXES: $(SUFFIXES) .f95 .o

%.o: %.f95
	$(FC) $(FFLAGS) -c $<

# Includes created dependencies from makedepf90
include .depend
    
# Clean
clean:
	rm -f $(PROG) $(FOBJ) *.mod .depend *.o
    
# Generating the dependencies makedepf90
depend .depend:
	makedepf90 -free -o wave *.f95 > .depend
