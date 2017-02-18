output.out:main.o gausscme.o inverse.o
	ifort -o output.out main.o gausscme.o inverse.o
main.o:main.f90
	ifort -g -c main.f90 -o main.o
gausscme.o:gausscme.f90
	ifort -g -c gausscme.f90 -o gausscme.o
inverse.o:inverse.f90
	ifort -g -c inverse.f90 -o inverse.o

clean:
	rm output.out *.o
