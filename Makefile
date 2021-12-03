#############################################################
#    To calculate spectral functions of a single hole 
#    in the t-J model using the self-consistent Born approximation
#
#   Author: 	Weiguo Yin
#   Date:	20 January 2003
#   Modified:	23 October 2006
#############################################################

OBJ = main.o backbone.o random.o cputime.o ReadIn.o

TARGET = fb

FC  = gfortran
FFLAGS = -O3
LIBS = -lm

all:	$(OBJ)
	$(FC) $(FFLAGS) $(OBJ) -o $(TARGET) $(LIBS)

clean:
	rm -f *.o $(ALL)

new: 	clean all
