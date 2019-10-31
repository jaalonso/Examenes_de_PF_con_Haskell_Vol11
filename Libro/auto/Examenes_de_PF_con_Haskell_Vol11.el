(TeX-add-style-hook
 "Examenes_de_PF_con_Haskell_Vol11"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("book" "a4paper" "12pt" "twoside")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("babel" "spanish") ("hyperref" "colorlinks=true" "urlcolor=blue" "pdfauthor={José A. Alonso <jalonso@us.es>}" "pdftitle={Examenes de Informatica de 1 de Matematicas}" "pdfstartview=FitH" "bookmarks=false")))
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (add-to-list 'LaTeX-verbatim-environments-local "VerbatimOut")
   (add-to-list 'LaTeX-verbatim-environments-local "SaveVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "LVerbatim*")
   (add-to-list 'LaTeX-verbatim-environments-local "LVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "BVerbatim*")
   (add-to-list 'LaTeX-verbatim-environments-local "BVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "Verbatim*")
   (add-to-list 'LaTeX-verbatim-environments-local "Verbatim")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "Verb")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "Verb")
   (TeX-run-style-hooks
    "latex2e"
    "definiciones"
    "Licencia/licenciaCC"
    "Apendice/resumen_Haskell"
    "Apendice/metodo_de_Polya"
    "book"
    "bk12"
    "fontspec"
    "xltxtra"
    "babel"
    "fancyvrb"
    "a4wide"
    "minted"
    "hyperref"
    "tocstyle"
    "fancyhdr")
   (LaTeX-add-environments
    "enumerate"
    "itemize")
   (LaTeX-add-bibliographies
    "Examenes_de_PF_con_Haskell_Vol10"))
 :latex)

