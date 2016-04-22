(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["SyntaxAnnotations`Tests`Integration`PatternsDelayed`",
	{"MUnit`"}
]


Get["SyntaxAnnotations`Tests`Integration`init`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*RuleDelayed*)


Test[
	AnnotateSyntax @ MakeBoxes[a :> a]
	,
	MakeBoxes[
		SyntaxExpr[a, "UndefinedSymbol"] :> SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a :> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ :> a]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] :>
			SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	]
	,
	TestID -> "a_ :> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a__ :> a]
	,
	MakeBoxes[
		SyntaxExpr[a__, "PatternVariable"] :>
			SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	]
	,
	TestID -> "a__ :> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a___ :> a]
	,
	MakeBoxes[
		SyntaxExpr[a___, "PatternVariable"] :>
			SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	]
	,
	TestID -> "a___ :> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[_a :> a]
	,
	MakeBoxes[_a :> SyntaxExpr[a, "UndefinedSymbol"]]
	,
	TestID -> "_a :> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[__a :> a]
	,
	MakeBoxes[__a :> SyntaxExpr[a, "UndefinedSymbol"]]
	,
	TestID -> "__a :> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[___a :> a]
	,
	MakeBoxes[___a :> SyntaxExpr[a, "UndefinedSymbol"]]
	,
	TestID -> "___a :> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_b :> a b]
	,
	MakeBoxes[
		SyntaxExpr[a_b, "PatternVariable"] :>
			SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
			SyntaxExpr[b, "UndefinedSymbol"]
	]
	,
	TestID -> "a_b :> a b"
]


Test[
	AnnotateSyntax @ MakeBoxes[a :> a_]
	,
	MakeBoxes[SyntaxExpr[a, "UndefinedSymbol"] :> a_]
	,
	TestID -> "a :> a_"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ :> a_]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] :>
			SyntaxExpr[a_, "LocalScopeConflict"]
	]
	,
	TestID -> "a_ :> a_"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ :> a__]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] :>
			SyntaxExpr[a__, "LocalScopeConflict"]
	]
	,
	TestID -> "a_ :> a__"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ :> a___]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] :>
			SyntaxExpr[a___, "LocalScopeConflict"]
	]
	,
	TestID -> "a_ :> a___"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ :> _a]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] :>
			SyntaxExpr[_a, "PatternVariable"]
	]
	,
	TestID -> "a_ :> _a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ :> __a]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] :>
			SyntaxExpr[__a, "PatternVariable"]
	]
	,
	TestID -> "a_ :> __a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ :> ___a]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] :>
			SyntaxExpr[___a, "PatternVariable"]
	]
	,
	TestID -> "a_ :> ___a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ \[RuleDelayed] a]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] :>
			SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	]
	,
	TestID -> "a_ \\[RuleDelayed] a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ :> a_b]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] :>
			SyntaxExpr[a_b, "LocalScopeConflict"]
	]
	,
	TestID -> "a_ :> a_b"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ :> b_a]
	,
	MakeBoxes[SyntaxExpr[a_, "PatternVariable"] :> b_a]
	,
	TestID -> "a_ :> b_a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ b_ :> a b]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] SyntaxExpr[b_, "PatternVariable"] :>
			SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
			SyntaxExpr[b, "PatternVariable", "UndefinedSymbol"]
	]
	,
	TestID -> "a_ b_ :> a b"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ a :> a]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] SyntaxExpr[a, "UndefinedSymbol"] :>
			SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	]
	,
	TestID -> "a_ a :> a"
]


(* ::Subsection:: *)
(*SetDelayed*)


Test[
	AnnotateSyntax @ MakeBoxes[a := a]
	,
	MakeBoxes[
		SyntaxExpr[a, "UndefinedSymbol"] := SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a := a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ := a]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] :=
			SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	]
	,
	TestID -> "a_ := a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a := a_]
	,
	MakeBoxes[SyntaxExpr[a, "UndefinedSymbol"] := a_]
	,
	TestID -> "a := a_"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ b_ := a b]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] SyntaxExpr[b_, "PatternVariable"] :=
			SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
			SyntaxExpr[b, "PatternVariable", "UndefinedSymbol"]
	]
	,
	TestID -> "a_ b_ := a b"
]


(* ::Subsection:: *)
(*UpSetDelayed*)


Test[
	AnnotateSyntax @ MakeBoxes[a ^:= a]
	,
	MakeBoxes[
		SyntaxExpr[a, "UndefinedSymbol"] ^:= SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a ^:= a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ ^:= a]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] ^:=
			SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	]
	,
	TestID -> "a_ ^:= a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a ^:= a_]
	,
	MakeBoxes[SyntaxExpr[a, "UndefinedSymbol"] ^:= a_]
	,
	TestID -> "a ^:= a_"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ b_ ^:= a b]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] SyntaxExpr[b_, "PatternVariable"] ^:=
			SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
			SyntaxExpr[b, "PatternVariable", "UndefinedSymbol"]
	]
	,
	TestID -> "a_ b_ ^:= a b"
]


(* ::Subsection:: *)
(*TagSetDelayed*)


Test[
	AnnotateSyntax @ MakeBoxes[a /: a := a]
	,
	MakeBoxes[
		SyntaxExpr[a, "UndefinedSymbol"] /: SyntaxExpr[a, "UndefinedSymbol"] :=
			SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a /: a := a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ /: a := a]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] /:
			SyntaxExpr[a, "UndefinedSymbol"] :=
				SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	]
	,
	TestID -> "a_ /: a := a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a /: a_ := a]
	,
	MakeBoxes[
		SyntaxExpr[a, "UndefinedSymbol"] /:
			SyntaxExpr[a_, "PatternVariable"] :=
				SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	]
	,
	TestID -> "a /: a_ := a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a /: a := a_]
	,
	MakeBoxes[
		SyntaxExpr[a, "UndefinedSymbol"] /:
			SyntaxExpr[a, "UndefinedSymbol"] := a_
	]
	,
	TestID -> "a /: a := a_"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ /: a_ := a]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] /:
			SyntaxExpr[a_, "PatternVariable"] :=
				SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	]
	,
	TestID -> "a_ /: a_ := a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ /: a := a_]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] /:
			SyntaxExpr[a, "UndefinedSymbol"] :=
				SyntaxExpr[a_, "LocalScopeConflict"]
	]
	,
	TestID -> "a_ /: a := a_"
]


Test[
	AnnotateSyntax @ MakeBoxes[a /: a_ := a_]
	,
	MakeBoxes[
		SyntaxExpr[a, "UndefinedSymbol"] /:
			SyntaxExpr[a_, "PatternVariable"] :=
				SyntaxExpr[a_, "LocalScopeConflict"]
	]
	,
	TestID -> "a /: a_ := a_"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ /: a_ := a_]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] /:
			SyntaxExpr[a_, "PatternVariable"] :=
				SyntaxExpr[a_, "LocalScopeConflict"]
	]
	,
	TestID -> "a_ /: a_ := a_"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ b_ /: a b := a b]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] SyntaxExpr[b_, "PatternVariable"] /:
			SyntaxExpr[a, "UndefinedSymbol"] *
			SyntaxExpr[b, "UndefinedSymbol"] :=
				SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
				SyntaxExpr[b, "PatternVariable", "UndefinedSymbol"]
	]
	,
	TestID -> "a_ b_ /: a b := a b"
]


Test[
	AnnotateSyntax @ MakeBoxes[a b /: a_ b_ := a b]
	,
	MakeBoxes[
		SyntaxExpr[a, "UndefinedSymbol"] SyntaxExpr[b, "UndefinedSymbol"] /:
			SyntaxExpr[a_, "PatternVariable"] *
			SyntaxExpr[b_, "PatternVariable"] :=
				SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
				SyntaxExpr[b, "PatternVariable", "UndefinedSymbol"]
	]
	,
	TestID -> "a b /: a_ b_ := a b"
]


Test[
	AnnotateSyntax @ MakeBoxes[a b /: a b := a_ b_]
	,
	MakeBoxes[
		SyntaxExpr[a, "UndefinedSymbol"] SyntaxExpr[b, "UndefinedSymbol"] /:
			SyntaxExpr[a, "UndefinedSymbol"] *
			SyntaxExpr[b, "UndefinedSymbol"] :=
				a_ b_
	]
	,
	TestID -> "a b /: a b := a_ b_"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
