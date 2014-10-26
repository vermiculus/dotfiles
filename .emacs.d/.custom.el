(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(*-text-mono-type "Source Code Pro")
 '(*-text-sans-type "Avenir Book")
 '(*-text-serif-type "Baskerville Old")
 '(TeX-command-list
   (quote
    (("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "texexec --once --texutil %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file")
     ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command")
     ("Arara" "arara %t" TeX-run-TeX nil t))))
 '(TeX-parse-self t)
 '(TeX-view-program-list
   (quote
    (("System Start (Windows)" "start %o")
     ("System Start (OS X)" "open %o"))))
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(auto-mode-alist
   (quote
    (("\\.ly" . LilyPond-mode)
     ("\\.[Cc][Ss][Vv]\\'" . csv-mode)
     ("\\.ya?ml$" . yaml-mode)
     ("\\.cson\\'" . coffee-mode)
     ("Cakefile\\'" . coffee-mode)
     ("\\.iced\\'" . coffee-mode)
     ("\\.coffee\\'" . coffee-mode)
     ("\\.fish\\'" . fish-mode)
     ("\\.drv\\'" . latex-mode)
     ("/\\(?:COMMIT\\|NOTES\\|TAG\\|PULLREQ\\)_EDITMSG\\'" . git-commit-mode)
     ("/MERGE_MSG\\'" . git-commit-mode)
     ("/git-rebase-todo\\'" . git-rebase-mode)
     ("\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'" nil epa-file)
     ("\\.elc\\'" . emacs-lisp-byte-code-mode)
     ("\\.dz\\'" nil jka-compr)
     ("\\.xz\\'" nil jka-compr)
     ("\\.lzma\\'" nil jka-compr)
     ("\\.lz\\'" nil jka-compr)
     ("\\.g?z\\'" nil jka-compr)
     ("\\.bz2\\'" nil jka-compr)
     ("\\.Z\\'" nil jka-compr)
     ("\\.vr[hi]?\\'" . vera-mode)
     ("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . ruby-mode)
     ("\\.re?st\\'" . rst-mode)
     ("\\.py\\'" . python-mode)
     ("\\.awk\\'" . awk-mode)
     ("\\.\\(u?lpc\\|pike\\|pmod\\(\\.in\\)?\\)\\'" . pike-mode)
     ("\\.idl\\'" . idl-mode)
     ("\\.java\\'" . java-mode)
     ("\\.m\\'" . objc-mode)
     ("\\.ii\\'" . c++-mode)
     ("\\.i\\'" . c-mode)
     ("\\.lex\\'" . c-mode)
     ("\\.y\\(acc\\)?\\'" . c-mode)
     ("\\.[ch]\\'" . c-mode)
     ("\\.\\(CC?\\|HH?\\)\\'" . c++-mode)
     ("\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\'" . c++-mode)
     ("\\.\\(cc\\|hh\\)\\'" . c++-mode)
     ("\\.\\(bat\\|cmd\\)\\'" . bat-mode)
     ("\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" . html-mode)
     ("\\.svgz?\\'" . image-mode)
     ("\\.svgz?\\'" . xml-mode)
     ("\\.x[bp]m\\'" . image-mode)
     ("\\.x[bp]m\\'" . c-mode)
     ("\\.p[bpgn]m\\'" . image-mode)
     ("\\.tiff?\\'" . image-mode)
     ("\\.gif\\'" . image-mode)
     ("\\.png\\'" . image-mode)
     ("\\.jpe?g\\'" . image-mode)
     ("\\.te?xt\\'" . text-mode)
     ("\\.[tT]e[xX]\\'" . tex-mode)
     ("\\.ins\\'" . tex-mode)
     ("\\.ltx\\'" . latex-mode)
     ("\\.dtx\\'" . doctex-mode)
     ("\\.org\\'" . org-mode)
     ("\\.el\\'" . emacs-lisp-mode)
     ("Project\\.ede\\'" . emacs-lisp-mode)
     ("\\.\\(scm\\|stk\\|ss\\|sch\\)\\'" . scheme-mode)
     ("\\.l\\'" . lisp-mode)
     ("\\.li?sp\\'" . lisp-mode)
     ("\\.[fF]\\'" . fortran-mode)
     ("\\.for\\'" . fortran-mode)
     ("\\.p\\'" . pascal-mode)
     ("\\.pas\\'" . pascal-mode)
     ("\\.\\(dpr\\|DPR\\)\\'" . delphi-mode)
     ("\\.ad[abs]\\'" . ada-mode)
     ("\\.ad[bs].dg\\'" . ada-mode)
     ("\\.\\([pP]\\([Llm]\\|erl\\|od\\)\\|al\\)\\'" . perl-mode)
     ("Imakefile\\'" . makefile-imake-mode)
     ("Makeppfile\\(?:\\.mk\\)?\\'" . makefile-makepp-mode)
     ("\\.makepp\\'" . makefile-makepp-mode)
     ("\\.mk\\'" . makefile-bsdmake-mode)
     ("\\.make\\'" . makefile-bsdmake-mode)
     ("GNUmakefile\\'" . makefile-gmake-mode)
     ("[Mm]akefile\\'" . makefile-bsdmake-mode)
     ("\\.am\\'" . makefile-automake-mode)
     ("\\.texinfo\\'" . texinfo-mode)
     ("\\.te?xi\\'" . texinfo-mode)
     ("\\.[sS]\\'" . asm-mode)
     ("\\.asm\\'" . asm-mode)
     ("\\.css\\'" . css-mode)
     ("\\.mixal\\'" . mixal-mode)
     ("\\.gcov\\'" . compilation-mode)
     ("/\\.[a-z0-9-]*gdbinit" . gdb-script-mode)
     ("-gdb\\.gdb" . gdb-script-mode)
     ("[cC]hange\\.?[lL]og?\\'" . change-log-mode)
     ("[cC]hange[lL]og[-.][0-9]+\\'" . change-log-mode)
     ("\\$CHANGE_LOG\\$\\.TXT" . change-log-mode)
     ("\\.scm\\.[0-9]*\\'" . scheme-mode)
     ("\\.[ck]?sh\\'\\|\\.shar\\'\\|/\\.z?profile\\'" . sh-mode)
     ("\\.bash\\'" . sh-mode)
     ("\\(/\\|\\`\\)\\.\\(bash_\\(profile\\|history\\|log\\(in\\|out\\)\\)\\|z?log\\(in\\|out\\)\\)\\'" . sh-mode)
     ("\\(/\\|\\`\\)\\.\\(shrc\\|[kz]shrc\\|bashrc\\|t?cshrc\\|esrc\\)\\'" . sh-mode)
     ("\\(/\\|\\`\\)\\.\\([kz]shenv\\|xinitrc\\|startxrc\\|xsession\\)\\'" . sh-mode)
     ("\\.m?spec\\'" . sh-mode)
     ("\\.m[mes]\\'" . nroff-mode)
     ("\\.man\\'" . nroff-mode)
     ("\\.sty\\'" . latex-mode)
     ("\\.cl[so]\\'" . latex-mode)
     ("\\.bbl\\'" . latex-mode)
     ("\\.bib\\'" . bibtex-mode)
     ("\\.bst\\'" . bibtex-style-mode)
     ("\\.sql\\'" . sql-mode)
     ("\\.m[4c]\\'" . m4-mode)
     ("\\.mf\\'" . metafont-mode)
     ("\\.mp\\'" . metapost-mode)
     ("\\.vhdl?\\'" . vhdl-mode)
     ("\\.article\\'" . text-mode)
     ("\\.letter\\'" . text-mode)
     ("\\.i?tcl\\'" . tcl-mode)
     ("\\.exp\\'" . tcl-mode)
     ("\\.itk\\'" . tcl-mode)
     ("\\.icn\\'" . icon-mode)
     ("\\.sim\\'" . simula-mode)
     ("\\.mss\\'" . scribe-mode)
     ("\\.f9[05]\\'" . f90-mode)
     ("\\.f0[38]\\'" . f90-mode)
     ("\\.indent\\.pro\\'" . fundamental-mode)
     ("\\.\\(pro\\|PRO\\)\\'" . idlwave-mode)
     ("\\.srt\\'" . srecode-template-mode)
     ("\\.prolog\\'" . prolog-mode)
     ("\\.tar\\'" . tar-mode)
     ("\\.\\(arc\\|zip\\|lzh\\|lha\\|zoo\\|[jew]ar\\|xpi\\|rar\\|7z\\|ARC\\|ZIP\\|LZH\\|LHA\\|ZOO\\|[JEW]AR\\|XPI\\|RAR\\|7Z\\)\\'" . archive-mode)
     ("\\.oxt\\'" . archive-mode)
     ("\\.\\(deb\\|[oi]pk\\)\\'" . archive-mode)
     ("\\`/tmp/Re" . text-mode)
     ("/Message[0-9]*\\'" . text-mode)
     ("\\`/tmp/fol/" . text-mode)
     ("\\.oak\\'" . scheme-mode)
     ("\\.sgml?\\'" . sgml-mode)
     ("\\.x[ms]l\\'" . xml-mode)
     ("\\.dbk\\'" . xml-mode)
     ("\\.dtd\\'" . sgml-mode)
     ("\\.ds\\(ss\\)?l\\'" . dsssl-mode)
     ("\\.js\\'" . javascript-mode)
     ("\\.json\\'" . javascript-mode)
     ("\\.[ds]?vh?\\'" . verilog-mode)
     ("\\.by\\'" . bovine-grammar-mode)
     ("\\.wy\\'" . wisent-grammar-mode)
     ("[]>:/\\]\\..*\\(emacs\\|gnus\\|viper\\)\\'" . emacs-lisp-mode)
     ("\\`\\..*emacs\\'" . emacs-lisp-mode)
     ("[:/]_emacs\\'" . emacs-lisp-mode)
     ("/crontab\\.X*[0-9]+\\'" . shell-script-mode)
     ("\\.ml\\'" . lisp-mode)
     ("\\.ld[si]?\\'" . ld-script-mode)
     ("ld\\.?script\\'" . ld-script-mode)
     ("\\.xs\\'" . c-mode)
     ("\\.x[abdsru]?[cnw]?\\'" . ld-script-mode)
     ("\\.zone\\'" . dns-mode)
     ("\\.soa\\'" . dns-mode)
     ("\\.asd\\'" . lisp-mode)
     ("\\.\\(asn\\|mib\\|smi\\)\\'" . snmp-mode)
     ("\\.\\(as\\|mi\\|sm\\)2\\'" . snmpv2-mode)
     ("\\.\\(diffs?\\|patch\\|rej\\)\\'" . diff-mode)
     ("\\.\\(dif\\|pat\\)\\'" . diff-mode)
     ("\\.[eE]?[pP][sS]\\'" . ps-mode)
     ("\\.\\(?:PDF\\|DVI\\|OD[FGPST]\\|DOCX?\\|XLSX?\\|PPTX?\\|pdf\\|djvu\\|dvi\\|od[fgpst]\\|docx?\\|xlsx?\\|pptx?\\)\\'" . doc-view-mode-maybe)
     ("configure\\.\\(ac\\|in\\)\\'" . autoconf-mode)
     ("\\.s\\(v\\|iv\\|ieve\\)\\'" . sieve-mode)
     ("BROWSE\\'" . ebrowse-tree-mode)
     ("\\.ebrowse\\'" . ebrowse-tree-mode)
     ("#\\*mail\\*" . mail-mode)
     ("\\.g\\'" . antlr-mode)
     ("\\.mod\\'" . m2-mode)
     ("\\.ses\\'" . ses-mode)
     ("\\.docbook\\'" . sgml-mode)
     ("\\.com\\'" . dcl-mode)
     ("/config\\.\\(?:bat\\|log\\)\\'" . fundamental-mode)
     ("\\.\\(?:[iI][nN][iI]\\|[lL][sS][tT]\\|[rR][eE][gG]\\|[sS][yY][sS]\\)\\'" . conf-mode)
     ("\\.\\(?:desktop\\|la\\)\\'" . conf-unix-mode)
     ("\\.ppd\\'" . conf-ppd-mode)
     ("java.+\\.conf\\'" . conf-javaprop-mode)
     ("\\.properties\\(?:\\.[a-zA-Z0-9._-]+\\)?\\'" . conf-javaprop-mode)
     ("\\`/etc/\\(?:DIR_COLORS\\|ethers\\|.?fstab\\|.*hosts\\|lesskey\\|login\\.?de\\(?:fs\\|vperm\\)\\|magic\\|mtab\\|pam\\.d/.*\\|permissions\\(?:\\.d/.+\\)?\\|protocols\\|rpc\\|services\\)\\'" . conf-space-mode)
     ("\\`/etc/\\(?:acpid?/.+\\|aliases\\(?:\\.d/.+\\)?\\|default/.+\\|group-?\\|hosts\\..+\\|inittab\\|ksysguarddrc\\|opera6rc\\|passwd-?\\|shadow-?\\|sysconfig/.+\\)\\'" . conf-mode)
     ("[cC]hange[lL]og[-.][-0-9a-z]+\\'" . change-log-mode)
     ("/\\.?\\(?:gnokiirc\\|kde.*rc\\|mime\\.types\\|wgetrc\\)\\'" . conf-mode)
     ("/\\.\\(?:enigma\\|gltron\\|gtk\\|hxplayer\\|net\\|neverball\\|qt/.+\\|realplayer\\|scummvm\\|sversion\\|sylpheed/.+\\|xmp\\)rc\\'" . conf-mode)
     ("/\\.\\(?:gdbtkinit\\|grip\\|orbital/.+txt\\|rhosts\\|tuxracer/options\\)\\'" . conf-mode)
     ("/\\.?X\\(?:default\\|resource\\|re\\)s\\>" . conf-xdefaults-mode)
     ("/X11.+app-defaults/" . conf-xdefaults-mode)
     ("/X11.+locale/.+/Compose\\'" . conf-colon-mode)
     ("/X11.+locale/compose\\.dir\\'" . conf-javaprop-mode)
     ("\\.~?[0-9]+\\.[0-9][-.0-9]*~?\\'" nil t)
     ("\\.\\(?:orig\\|in\\|[bB][aA][kK]\\)\\'" nil t)
     ("[/.]c\\(?:on\\)?f\\(?:i?g\\)?\\(?:\\.[a-zA-Z0-9._-]+\\)?\\'" . conf-mode-maybe)
     ("\\.[1-9]\\'" . nroff-mode)
     ("\\.tgz\\'" . tar-mode)
     ("\\.tbz2?\\'" . tar-mode)
     ("\\.txz\\'" . tar-mode))) t)
 '(before-save-hook (quote (time-stamp *-maybe-delete-trailing-whitespace)))
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(custom-enabled-themes (quote (monokai)))
 '(custom-file (concat user-emacs-directory ".custom.el"))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "57f8801351e8b7677923c9fe547f7e19f38c99b80d68c34da6fa9b94dc6d3297" default)))
 '(debug-on-error t)
 '(default-input-method "TeX")
 '(desktop-save-mode t)
 '(dired-listing-switches "-alh")
 '(display-time-mode t)
 '(doc-view-mode-hook (quote (auto-revert-mode)) t)
 '(emacs-lisp-mode-hook
   (quote
    (eldoc-mode imenu-add-menubar-index checkdoc-minor-mode show-paren-mode)))
 '(exec-path
   (quote
    ("/usr/local/bin" "/usr/texbin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_9" "/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_9" "/Applications/Emacs.app/Contents/MacOS/libexec" "/Applications/Emacs.app/Contents/MacOS/bin")))
 '(find-file-hook
   (quote
    (auto-insert yas-global-mode-check-buffers
                 #[nil "\302\301!\210\303\304!8\211\207"
                       [buffer-file-name auto-revert-tail-pos make-local-variable 7 file-attributes]
                       3]
                 global-undo-tree-mode-check-buffers undo-tree-load-history-hook global-font-lock-mode-check-buffers epa-file-find-file-hook vc-find-file-hook)))
 '(flx-ido-mode t)
 '(ido-enable-dot-prefix t)
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(inferior-lisp-program "clisp" t)
 '(inhibit-startup-screen t)
 '(insert-directory-program "gls" t)
 '(ispell-program-name
   (if *--windows-p "t:/#_Programs/Aspell/bin/aspell.exe" "/usr/local/bin/ispell"))
 '(lisp-mode-hook (quote (show-paren-mode slime-lisp-mode-hook)))
 '(m4-font-lock-keywords
   (quote
    (("\\(\\b\\(m4_\\)?dnl\\b\\).*$" . font-lock-comment-face)
     ("\\$#" . font-lock-variable-name-face)
     ("\\$\\@" . font-lock-variable-name-face)
     ("\\$\\*" . font-lock-variable-name-face)
     ("\\b\\(builtin\\|change\\(com\\|quote\\|word\\)\\|d\\(e\\(bug\\(file\\|mode\\)\\|cr\\|f\\(ine\\|n\\)\\)\\|iv\\(ert\\|num\\)\\|nl\\|umpdef\\)\\|e\\(rrprint\\|syscmd\\|val\\)\\|f\\(ile\\|ormat\\)\\|gnu\\|i\\(f\\(def\\|else\\)\\|n\\(c\\(lude\\|r\\)\\|d\\(ex\\|ir\\)\\)\\)\\|l\\(en\\|ine\\)\\|m\\(4\\(exit\\|wrap\\)\\|aketemp\\)\\|p\\(atsubst\\|opdef\\|ushdef\\)\\|regexp\\|s\\(hift\\|include\\|ubstr\\|ys\\(cmd\\|val\\)\\)\\|tra\\(ceo\\(ff\\|n\\)\\|nslit\\)\\|un\\(d\\(efine\\|ivert\\)\\|ix\\)\\)\\b" . font-lock-keyword-face)
     ("\\b\\(m4_\\(builtin\\|change\\(com\\|quote\\|word\\)\\|d\\(e\\(bug\\(file\\|mode\\)\\|cr\\|f\\(ine\\|n\\)\\)\\|iv\\(ert\\|num\\)\\|nl\\|umpdef\\)\\|e\\(rrprint\\|syscmd\\|val\\)\\|f\\(ile\\|ormat\\)\\|i\\(f\\(def\\|else\\)\\|n\\(c\\(lude\\|r\\)\\|d\\(ex\\|ir\\)\\)\\)\\|l\\(en\\|ine\\)\\|m\\(4\\(_undefine\\|exit\\|wrap\\)\\|aketemp\\)\\|p\\(atsubst\\|opdef\\|ushdef\\)\\|regexp\\|s\\(hift\\|include\\|ubstr\\|ys\\(cmd\\|val\\)\\)\\|tra\\(ceo\\(ff\\|n\\)\\|nslit\\)\\|undivert\\)\\)\\b" . font-lock-keyword-face))) t)
 '(menu-bar-mode (not *--windows-p))
 '(org-agenda-files "~/.emacs.d/.agenda_files")
 '(org-agenda-include-diary t)
 '(org-babel-load-languages
   (quote
    ((perl . t)
     (ruby . t)
     (python . t)
     (sh . t)
     (R . t)
     (emacs-lisp . t))))
 '(org-capture-templates
   (quote
    (("n" "Note" entry
      (file "notes.org")
      "* %U %?")
     ("t" "Task" entry
      (file "unfiled-tasks.org")
      "* TODO %?")
     ("j" "Journal Entry" entry
      (file+datetree "journal.org")
      "* %u %?"))))
 '(org-completion-use-ido t)
 '(org-edit-src-content-indentation 0)
 '(org-id-link-to-org-use-id t)
 '(org-load-hook (quote (org-agenda-to-appt appt-activate)))
 '(org-log-done (quote note))
 '(org-mobile-directory (concat "/Users/sean/Dropbox/Apps/MobileOrg" "4"))
 '(org-outline-path-complete-in-steps nil)
 '(org-structure-template-alist
   (quote
    (("n" "#+name: ?
#+begin_src 

#+end_src" "")
     ("nt" "#+name: ?
#+begin_src elisp :tangle \"\"

#+end_src" "")
     ("s" "#+begin_src ?

#+end_src" "<src lang=\"?\">

</src>")
     ("es" "#+begin_src elisp
?
#+end_src" "")
     ("esf" "#+begin_src elisp :tangle %file
?
#+end_src" "")
     ("e" "#+begin_example
?
#+end_example" "<example>
?
</example>")
     ("q" "#+begin_quote
?
#+end_quote" "<quote>
?
</quote>")
     ("v" "#+begin_verse
?
#+end_verse" "<verse>
?
</verse>")
     ("V" "#+begin_verbatim
?
#+end_verbatim" "<verbatim>
?
</verbatim>")
     ("c" "#+begin_center
?
#+end_center" "<center>
?
</center>")
     ("l" "#+begin_latex
?
#+end_latex" "<literal style=\"latex\">
?
</literal>")
     ("L" "#+LaTeX: " "<literal style=\"latex\">?</literal>")
     ("h" "#+begin_html
?
#+end_html" "<literal style=\"html\">
?
</literal>")
     ("H" "#+HTML: " "<literal style=\"html\">?</literal>")
     ("a" "#+begin_ascii
?
#+end_ascii" "")
     ("A" "#+ASCII: " "")
     ("i" "#+INDEX: ?" "#+INDEX: ?")
     ("I" "#+INCLUDE: %file ?" "<include file=%file markup=\"?\">"))))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.melpa.org/packages/"))))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(twittering-use-master-password t t)
 '(user-emacs-directory (concat (if *--windows-p "T:" "~") "/.emacs.d/") t)
 '(visible-bell t)
 '(yas-global-mode t nil (yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
