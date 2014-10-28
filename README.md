dotfiles
--------

These are my configuration files.  The spotlight right now is on my
`.emacs.d`, which is undergoing its first complete rewrite since I
started using Emacs late 2011.  I'm updating this file to say one
thing before I pass out for lack of sleep: use elisp for
customizations that are cross-platform and use `custom.el` for
customizations that are local.  Some things can only be done with
customize.  (Saving Ibuffer groups is such a thing, for example; see
`M-x ibuffer-save-filters` (which calls `ibuffer-maybe-save-stuff`).
