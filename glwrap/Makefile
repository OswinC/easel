OUT_DIR = _build
APP_DIR = apps

SRC = $(wildcard *.c)
CAPPS = $(wildcard $(APP_DIR)/*.c)
LLAPPS = $(wildcard $(APP_DIR)/*.ll)
APPS = $(CAPPS:.c=) $(LLAPPS:.ll=)
OBJ = $(SRC:.c=.o)
LIBNAME = glwrap

TARGETLIB = $(OUT_DIR)/lib$(LIBNAME).a
TARGETAPPS = $(foreach app, $(notdir $(APPS)), $(OUT_DIR)/$(app))

GLFLAGS = -L$(OUT_DIR) -l$(LIBNAME) -lGL -lGLEW -lglut -lm

.PHONY: all dir

TARGET = $(TARGETLIB) $(TARGETAPPS)

all: dir $(TARGET)

dir: ${OUT_DIR}

${OUT_DIR}:
	mkdir -p $@ 

$(TARGETLIB): $(OUT_DIR)/$(OBJ)
	$(AR) rcs $@ $^
	cp $@ ..

$(OUT_DIR)/%.o: %.c
	$(CC) -c -o $@ $^

$(OUT_DIR)/%: $(APP_DIR)/$(notdir %.c) $(TARGETLIB)
	gcc -o $@ $< $(GLFLAGS)

$(OUT_DIR)/%: $(APP_DIR)/$(notdir %.ll) $(TARGETLIB)
	llc -o $@.s $<
	gcc -o $@ $@.s $(GLFLAGS)
