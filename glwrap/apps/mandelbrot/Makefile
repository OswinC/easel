TARGET = mandelbrot
LIBS = -lGL -lGLEW -lglut -lm
CC = gcc
CFLAGS = -g -Wall

.PHONY: all clean cleanall

all: $(TARGET)

OBJECTS = $(patsubst %.c, %.o, $(wildcard *.c))
HEADERS = $(wildcard *.h)

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

.PRECIOUS: $(TARGET) $(OBJECTS)

$(TARGET): $(OBJECTS)
	$(CC) $(OBJECTS) -Wall $(LIBS) -o $@

clean:
	-rm -f *.o

cleanall: clean
	-rm -f $(TARGET)
