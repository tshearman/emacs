(TeX-add-style-hook
 "research"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("amsart" "a4paper" "10pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem")))
   (TeX-run-style-hooks
    "latex2e"
    "amsart"
    "amsart10"
    "inputenc"
    "fontenc"
    "graphicx"
    "grffile"
    "longtable"
    "wrapfig"
    "rotating"
    "ulem"
    "amsmath"
    "textcomp"
    "amssymb"
    "capt-of"
    "enumitem"
    "amsthm"
    "etoolbox"))
 :latex)

