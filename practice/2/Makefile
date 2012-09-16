CC=ghc

OBJDIR=../obj/

EXECUTABLE=out

MODULESDIR=./Modules/

MODULE=MyMath.hs MyAlgorithm.hs

MODULES=$(addprefix $(MODULESDIR), $(MODULE))

TARGET=main.hs

all:p2 p3 p4 p5

p2:
	$(CC) $(MODULES) ./2/$(TARGET) -o $(EXECUTABLE)2

p3:
	$(CC) $(MODULES) ./3/$(TARGET) -o $(EXECUTABLE)3

p4:
	$(CC) $(MODULES) ./4/$(TARGET) -o $(EXECUTABLE)4

p5:
	$(CC) $(MODULES) ./5/$(TARGET) -o $(EXECUTABLE)5	


clean:
	find . -name '*.hi' -delete
	find . -name '*.hs.*' -delete
	find . -name '*.o' -delete
	find . -name '*.hs~' -delete
	rm -f out*
