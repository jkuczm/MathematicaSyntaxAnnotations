(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["SyntaxAnnotations`Tests`Integration`Patterns`", {"MUnit`"}]


Get["SyntaxAnnotations`Tests`Integration`init`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Rule*)


Test[
	AnnotateSyntax @ MakeBoxes[a -> a]
	,
	MakeBoxes[
		syntaxExpr[a, "UndefinedSymbol"] -> syntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a -> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ -> a]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] -> syntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a_ -> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a__ -> a]
	,
	MakeBoxes[
		syntaxExpr[a__, "PatternVariable"] -> syntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a__ -> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a___ -> a]
	,
	MakeBoxes[
		syntaxExpr[a___, "PatternVariable"] -> syntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a___ -> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[_a -> a]
	,
	MakeBoxes[_a -> syntaxExpr[a, "UndefinedSymbol"]]
	,
	TestID -> "_a -> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[__a -> a]
	,
	MakeBoxes[__a -> syntaxExpr[a, "UndefinedSymbol"]]
	,
	TestID -> "__a -> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[___a -> a]
	,
	MakeBoxes[___a -> syntaxExpr[a, "UndefinedSymbol"]]
	,
	TestID -> "___a -> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_b -> a b]
	,
	MakeBoxes[
		syntaxExpr[a_b, "PatternVariable"] ->
			syntaxExpr[a, "UndefinedSymbol"] syntaxExpr[b, "UndefinedSymbol"]
	]
	,
	TestID -> "a_b -> a b"
]


Test[
	AnnotateSyntax @ MakeBoxes[a -> a_]
	,
	MakeBoxes[syntaxExpr[a, "UndefinedSymbol"] -> a_]
	,
	TestID -> "a -> a_"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ \[Rule] a]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] -> syntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a_ \\[Rule] a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ b_ -> a b]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] syntaxExpr[b_, "PatternVariable"] ->
			syntaxExpr[a, "UndefinedSymbol"] syntaxExpr[b, "UndefinedSymbol"]
	]
	,
	TestID -> "a_ b_ -> a b"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ a -> a]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] syntaxExpr[a, "UndefinedSymbol"] ->
			syntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a_ a -> a"
]


(* ::Subsection:: *)
(*Set*)


Test[
	AnnotateSyntax @ MakeBoxes[a = a]
	,
	MakeBoxes[
		syntaxExpr[a, "UndefinedSymbol"] = syntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a = a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ = a]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] = syntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a_ = a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a = a_]
	,
	MakeBoxes[syntaxExpr[a, "UndefinedSymbol"] = a_]
	,
	TestID -> "a = a_"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ b_ = a b]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] syntaxExpr[b_, "PatternVariable"] =
			syntaxExpr[a, "UndefinedSymbol"] syntaxExpr[b, "UndefinedSymbol"]
	]
	,
	TestID -> "a_ b_ = a b"
]


(* ::Subsection:: *)
(*UpSet*)


Test[
	AnnotateSyntax @ MakeBoxes[a ^= a]
	,
	MakeBoxes[
		syntaxExpr[a, "UndefinedSymbol"] ^= syntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a ^= a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ ^= a]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] ^= syntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a_ ^= a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a ^= a_]
	,
	MakeBoxes[syntaxExpr[a, "UndefinedSymbol"] ^= a_]
	,
	TestID -> "a ^= a_"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ b_ ^= a b]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] syntaxExpr[b_, "PatternVariable"] ^=
			syntaxExpr[a, "UndefinedSymbol"] syntaxExpr[b, "UndefinedSymbol"]
	]
	,
	TestID -> "a_ b_ ^= a b"
]


(* ::Subsection:: *)
(*TagSet*)


Test[
	AnnotateSyntax @ MakeBoxes[a /: a = a]
	,
	MakeBoxes[
		syntaxExpr[a, "UndefinedSymbol"] /: syntaxExpr[a, "UndefinedSymbol"] =
			syntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a /: a = a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ /: a = a]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] /: syntaxExpr[a, "UndefinedSymbol"] =
			syntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a_ /: a = a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a /: a_ = a]
	,
	MakeBoxes[
		syntaxExpr[a, "UndefinedSymbol"] /: syntaxExpr[a_, "PatternVariable"] =
			syntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a /: a_ = a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a /: a = a_]
	,
	MakeBoxes[
		syntaxExpr[a, "UndefinedSymbol"] /: syntaxExpr[a, "UndefinedSymbol"] =
			a_
	]
	,
	TestID -> "a /: a = a_"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ b_ /: a b = a b]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] syntaxExpr[b_, "PatternVariable"] /:
			syntaxExpr[a, "UndefinedSymbol"] syntaxExpr[b, "UndefinedSymbol"] =
				syntaxExpr[a, "UndefinedSymbol"] *
				syntaxExpr[b, "UndefinedSymbol"]
	]
	,
	TestID -> "a_ b_ /: a b = a b"
]


Test[
	AnnotateSyntax @ MakeBoxes[a b /: a_ b_ = a b]
	,
	MakeBoxes[
		syntaxExpr[a, "UndefinedSymbol"] syntaxExpr[b, "UndefinedSymbol"] /:
			syntaxExpr[a_, "PatternVariable"] *
			syntaxExpr[b_, "PatternVariable"] =
				syntaxExpr[a, "UndefinedSymbol"] *
				syntaxExpr[b, "UndefinedSymbol"]
	]
	,
	TestID -> "a b /: a_ b_ = a b"
]


Test[
	AnnotateSyntax @ MakeBoxes[a b /: a b = a_ b_]
	,
	MakeBoxes[
		syntaxExpr[a, "UndefinedSymbol"] syntaxExpr[b, "UndefinedSymbol"] /:
			syntaxExpr[a, "UndefinedSymbol"] syntaxExpr[b, "UndefinedSymbol"] =
				a_ b_
	]
	,
	TestID -> "a b /: a b = a_ b_"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
