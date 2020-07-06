(TeX-add-style-hook
 "journal"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("amsbook" "a4paper" "10pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem")))
   (TeX-run-style-hooks
    "latex2e"
    "amsbook"
    "amsbook10"
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
    "etoolbox")
   (LaTeX-add-labels
    "sec:org55ba464"
    "sec:orgf7fc827"
    "sec:org1de6a74"
    "sec:orgbf49dc7"))
 :latex)

