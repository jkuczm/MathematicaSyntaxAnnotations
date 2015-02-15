(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["SyntaxAnnotations`Tests`Integration`PatternsNested`", {"MUnit`"}]


Get["SyntaxAnnotations`Tests`Integration`init`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Rule*)


Test[
	AnnotateSyntax @ MakeBoxes[(a_ -> a_) -> a]
	,
	MakeBoxes[
		(
			syntaxExpr[a_, "PatternVariable"] ->
				syntaxExpr[a_, "PatternVariable"]
		) ->
			syntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "(a_ -> a_) -> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a -> (a_ -> a_)]
	,
	MakeBoxes[
		 syntaxExpr[a, "UndefinedSymbol"] ->
		 	(syntaxExpr[a_, "PatternVariable"] -> a_)
	]
	,
	TestID -> "a -> (a_ -> a_)"
]


(* ::Subsection:: *)
(*Set*)


Test[
	AnnotateSyntax @ MakeBoxes[(a_ = a_) = a]
	,
	MakeBoxes[
		(
			syntaxExpr[a_, "PatternVariable"] =
				syntaxExpr[a_, "PatternVariable"]
		) =
			syntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "(a_ = a_) = a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a = (a_ = a_)]
	,
	MakeBoxes[
		 syntaxExpr[a, "UndefinedSymbol"] =
		 	(syntaxExpr[a_, "PatternVariable"] = a_)
	]
	,
	TestID -> "a = (a_ = a_)"
]


(* ::Subsection:: *)
(*UpSet*)


Test[
	AnnotateSyntax @ MakeBoxes[(a_ ^= a_) ^= a]
	,
	MakeBoxes[
		(
			syntaxExpr[a_, "PatternVariable"] ^=
				syntaxExpr[a_, "PatternVariable"]
		) ^=
			syntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "(a_ ^= a_) = a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a ^= (a_ ^= a_)]
	,
	MakeBoxes[
		 syntaxExpr[a, "UndefinedSymbol"] ^=
		 	(syntaxExpr[a_, "PatternVariable"] ^= a_)
	]
	,
	TestID -> "a ^= (a_ ^= a_)"
]


(* ::Subsection:: *)
(*TagSet*)


Test[
	AnnotateSyntax @ MakeBoxes[(a_ /: a_ = a_) /: a = a]
	,
	MakeBoxes[
		(
			syntaxExpr[a_, "PatternVariable"] /:
				syntaxExpr[a_, "PatternVariable"] =
					syntaxExpr[a_, "PatternVariable"]
		) /:
			syntaxExpr[a, "UndefinedSymbol"] = syntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "(a_ /: a_ = a_) /: a = a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a /: (a_ /: a_ = a_) = a]
	,
	MakeBoxes[
		syntaxExpr[a, "UndefinedSymbol"] /: (
			syntaxExpr[a_, "PatternVariable"] /:
				syntaxExpr[a_, "PatternVariable"] =
					syntaxExpr[a_, "PatternVariable"]
		) = syntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a /: (a_ /: a_ = a_) = a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a /: a = (a_ /: a_ = a_)]
	,
	MakeBoxes[
		syntaxExpr[a, "UndefinedSymbol"] /:
			syntaxExpr[a, "UndefinedSymbol"] = (
				syntaxExpr[a_, "PatternVariable"] /:
					syntaxExpr[a_, "PatternVariable"] = a_
			)
	]
	,
	TestID -> "a /: a = (a_ /: a_ = a_)"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
