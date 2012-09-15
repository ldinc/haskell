CC=ghc

OBJDIR=../obj/

EXECUTABLE=out

MODULESDIR=./Modules/

MODULES=MyMath.hs

OBJSDIR=./obj/

OBJS=$(MODULES:.hs=.o)

TARGET=main.hs

all:
	$(CC) -c $(MODULESDIR)$(MODULES) 
	mv $(MODULESDIR)$(OBJS) $(OBJSDIR)

p2:$(OBJ)
	$(CC) $(MODULESDIR)$(MODULES) ./2/$(TARGET) -o $(EXECUTABLE)2

p3:
	$(CC) $(MODULESDIR)$(MODULES) ./3/$(TARGET) -o $(EXECUTABLE)3

p4:
	$(CC) $(MODULESDIR)$(MODULES) ./4/$(TARGET) -o $(EXECUTABLE)4

clean:
	find . -name '*.hi' -delete
	find . -name '*.hs.*' -delete
	find . -name '*.o' -delete
	find . -name '*.hs~' -delete
	rm -f out*
