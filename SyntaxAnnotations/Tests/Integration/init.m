(* Mathematica Init File *)

Get["SyntaxAnnotations`"]
Get["SyntaxAnnotations`Tests`Integration`Utilities`"]

SetOptions[AnnotateSyntax,
	"BoxRules" -> {
		SyntaxBox[boxes_, types__] :>
			SyntaxBox[boxes, Sequence @@ NormalizeAnnotationTypes[types]]
	}
]
