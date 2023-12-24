(TeX-add-style-hook
 "final"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "margin=1.0in") ("algorithm2e" "boxruled" "vlined" "nofillcomment")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "forest"
    "tikz"
    "algpseudocode"
    "amsmath"
    "amssymb"
    "amsthm"
    "tcolorbox"
    "enumerate"
    "enumitem"
    "framed"
    "verbatim"
    "geometry"
    "microtype"
    "kpfonts"
    "palatino"
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
    "pipe"
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
    "case"))
 :latex)

