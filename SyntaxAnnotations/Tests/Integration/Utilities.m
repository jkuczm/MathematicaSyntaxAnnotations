(* ::Package:: *)


BeginPackage["SyntaxAnnotations`Tests`Integration`Utilities`"]


Unprotect["`*"]
ClearAll["`*"]


(* ::Section:: *)
(*Usage messages*)


SyntaxBox::usage =
"\
SyntaxBox[expr, type1, type2, ...] \
represents boxes that in an expression perform sytnax roles of given types."


SyntaxExpr::usage =
"\
SyntaxExpr[expr, type1, type2, ...] \
converted to boxes gives SyntaxBox with expr converted to boxes and given \
annotation types."


(* ::Section:: *)
(*Implementation*)
	

Begin["`Private`"]


(* ::Subsection:: *)
(*syntaxExpr*)


MakeBoxes[SyntaxExpr[expr_, types___], StandardForm] ^:=
	SyntaxBox[MakeBoxes[expr], types]


(* ::Section:: *)
(*Package Epilogue*)


End[]


Protect["`*"]


EndPackage[]
