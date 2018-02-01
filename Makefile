EMACS ?= emacs

ELS = lisp/chunyang-edit-minibuffer.el
ELCS = $(ELS:.el=.elc)

all: $(ELCS)

%.elc: %.el
	$(EMACS) --batch -f batch-byte-compile $<

clean:
	rm -f $(ELCS)
