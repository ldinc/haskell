CC=ghc

OBJDIR=../obj/

EXECUTABLE=out

MODULESDIR=./Modules/

MODULE=MyMath.hs MyAlgorithm.hs MyStack.hs

MODULES=$(addprefix $(MODULESDIR), $(MODULE))

TARGET=main.hs

all:p2 p3 p4 p5 p6 p7 p18 p19

p2:
	$(CC) $(MODULES) ./2/$(TARGET) -o $(EXECUTABLE)2

p3:
	$(CC) $(MODULES) ./3/$(TARGET) -o $(EXECUTABLE)3

p4:
	$(CC) $(MODULES) ./4/$(TARGET) -o $(EXECUTABLE)4

p5:
	$(CC) $(MODULES) ./5/$(TARGET) -o $(EXECUTABLE)5	

p6:
	$(CC) $(MODULES) ./6/$(TARGET) -o $(EXECUTABLE)6

p7:
	$(CC) $(MODULES) ./7/$(TARGET) -o $(EXECUTABLE)7

p18:
	$(CC) $(MODULES) ./18/$(TARGET) -o $(EXECUTABLE)18

p19:
	$(CC) $(MODULES) ./19/$(TARGET) -o $(EXECUTABLE)19
	$(CC) $(MODULES) ./19/main2.hs -o $(EXECUTABLE)19f

clean:
	find . -name '*.hi' -delete
	find . -name '*.hs.*' -delete
	find . -name '*.o' -delete
	find . -name '*.hs~' -delete
	rm -f out*
