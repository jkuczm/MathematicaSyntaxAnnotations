(* ::Package:: *)


BeginPackage["SyntaxAnnotations`Tests`Integration`Utilities`"]


Unprotect["`*"]
ClearAll["`*"]


(* ::Section:: *)
(*Usage messages*)


syntaxExpr::usage =
"\
syntaxExpr[expr, types] \
converted to boxes gives SyntaxBox with expr converted to boxes and given \
annotation types."


(* ::Section:: *)
(*Implementation*)
	

Begin["`Private`"]


Needs["SyntaxAnnotations`"]


(* ::Subsection:: *)
(*syntaxExpr*)


MakeBoxes[syntaxExpr[expr_, types___], StandardForm] ^:=
	SyntaxBox[MakeBoxes[expr], types]


(* ::Section:: *)
(*Package Epilogue*)


End[]


Protect["`*"]


EndPackage[]
