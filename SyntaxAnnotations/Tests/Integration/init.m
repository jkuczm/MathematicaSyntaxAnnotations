(* Mathematica Init File *)

Get["SyntaxAnnotations`"]
Get["SyntaxAnnotations`Tests`Integration`Utilities`"]

SetOptions[AnnotateSyntax, "Annotation" -> (SyntaxBox[#1, Sequence @@ #2]&)]
