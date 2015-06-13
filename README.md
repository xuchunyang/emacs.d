# emacs.d

The `init.el` contains all package configuration and Emacs settings (with
[use-package](https://github.com/jwiegley/use-package)).  All 3rd party packages
come from MELPA or GNU ELPA.

## Setup

```console
$ git https://github.com/xuchunyang/emacs.d.git ~/.emacs.d
```

### OS X support

```console
$ sudo port install coreutils
```

### Spell checking

```console
$ sudo port install aspell aspell-dict-en
```

### Fuzzy find

```console
$ sudo port install fuzzy-find
```

### Mail

```console
$ sudo port install mu +emacs
$ sudo port install offlineimap msmtp
```

## Notable packages
- **Package management**: [use-package](https://github.com/jwiegley/use-package)
- **Version control**: [Magit](https://github.com/magit/magit) and
  [git-gutter](https://github.com/syohex/emacs-git-gutter)
- **Color theme**: [Zenburn](https://github.com/bbatsov/zenburn-emacs)
- **Mode line**: [Anzu](https://github.com/syohex/emacs-anzu) and
  [powerline](https://github.com/unic0rn/powerline)
- **Minibuffer**: [helm](https://github.com/emacs-helm/helm)
- **Visual guides**:
  [Page Break Lines](https://github.com/purcell/page-break-lines) and
  [Rainbow Delimiters](https://github.com/jlr/rainbow-delimiters)
- **Undo and killing**: [Undo Tree](http://www.dr-qubit.org/emacs.php#undo-tree) and
  [Easy Kill](https://github.com/leoliu/easy-kill)
- **Syntax checking**: [Flycheck](http://flycheck.readthedocs.org)
- **Auto-completion**: [Company Mode](http://company-mode.github.io)
- **Symbols**: [Highlight Symbol](https://github.com/nschum/highlight-symbol.el)
- **Project navigation**: [helm-ls-git](https://github.com/emacs-helm/helm-ls-git)
- **Lisp**: [Paredit](http://mumble.net/~campbell/emacs/paredit.html)
- **Emacs Lisp**: [Macrostep](https://github.com/joddie/macrostep)
- **Social Network**: [SX](https://github.com/vermiculus/sx.el)
