EMACS_ROOT	= $(HOME)/src/emacs/
EMACS_SRC	= $(EMACS_ROOT)/src/
EMACS		= $(EMACS_ROOT)/src/emacs

# Assuming Unix (change to *.dll on Windows)
all: taglib-core.so

taglib-core.o: taglib-core.c
	gcc -Wall -fPIC -I$(EMACS_SRC) $(shell pkg-config --cflags taglib_c) -c $<

taglib-core.so: taglib-core.o
	gcc -shared $(shell pkg-config --libs taglib_c) -o $@ $<

# Launch a fresh Emacs and test it manually
test-interactive:
	$(EMACS) --no-window-system -Q -L . -l taglib-core
