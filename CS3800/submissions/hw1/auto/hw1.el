(TeX-add-style-hook
 "hw1"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "margin=1.0in") ("xcolor" "usenames" "dvipsnames") ("hyperref" "pdftex") ("algorithm2e" "boxruled" "vlined" "nofillcomment")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "algpseudocode"
    "amsmath"
    "amssymb"
    "amsthm"
    "enumerate"
    "enumitem"
    "framed"
    "verbatim"
    "geometry"
    "microtype"
    "kpfonts"
    "palatino"
    "xcolor"
    "hyperref"
    "algorithm2e")
   (TeX-add-symbols
    '("ind" 1)
    '("set" 1)
    '("card" 1)
    '("var" 2)
    '("pr" 2)
    '("ex" 2)
    '("ab" 1)
    '("bracket" 1)
    '("paren" 1)
    "yourname"
    "poly"
    "polylog"
    "zo"
    "pmo"
    "getsr"
    "negl"
    "eps"
    "eqand"
    "sslash"
    "N"
    "R"
    "Z"
    "cA"
    "cB"
    "cC"
    "cD"
    "cE"
    "cF"
    "cL"
    "cM"
    "cO"
    "cP"
    "cQ"
    "cR"
    "cS"
    "cU"
    "cV"
    "cW"
    "cX"
    "cY"
    "cZ"
    "instructor"
    "hwnum"
    "hwdue"
    "solution"
    "comments")
   (LaTeX-add-amsthm-newtheorems
    "thm"
    "lem"
    "fact"
    "clm"
    "rem"
    "coro"
    "prop"
    "conj"
    "defn"
    "case"
    "prob"
    "sol")
   (LaTeX-add-amsthm-newtheoremstyles
    "case")
   (LaTeX-add-xcolor-definecolors
    "DarkGreen"
    "DarkRed"
    "DarkBlue"
    "DarkPurple"
    "cit"))
 :latex)

