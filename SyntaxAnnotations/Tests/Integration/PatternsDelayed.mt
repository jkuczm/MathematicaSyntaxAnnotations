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
		syntaxExpr[a, "UndefinedSymbol"] :> syntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a :> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ :> a]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] :>
			syntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	]
	,
	TestID -> "a_ :> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a__ :> a]
	,
	MakeBoxes[
		syntaxExpr[a__, "PatternVariable"] :>
			syntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	]
	,
	TestID -> "a__ :> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a___ :> a]
	,
	MakeBoxes[
		syntaxExpr[a___, "PatternVariable"] :>
			syntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	]
	,
	TestID -> "a___ :> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[_a :> a]
	,
	MakeBoxes[_a :> syntaxExpr[a, "UndefinedSymbol"]]
	,
	TestID -> "_a :> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[__a :> a]
	,
	MakeBoxes[__a :> syntaxExpr[a, "UndefinedSymbol"]]
	,
	TestID -> "__a :> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[___a :> a]
	,
	MakeBoxes[___a :> syntaxExpr[a, "UndefinedSymbol"]]
	,
	TestID -> "___a :> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_b :> a b]
	,
	MakeBoxes[
		syntaxExpr[a_b, "PatternVariable"] :>
			syntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
			syntaxExpr[b, "UndefinedSymbol"]
	]
	,
	TestID -> "a_b :> a b"
]


Test[
	AnnotateSyntax @ MakeBoxes[a :> a_]
	,
	MakeBoxes[syntaxExpr[a, "UndefinedSymbol"] :> a_]
	,
	TestID -> "a :> a_"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ :> a_]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] :>
			syntaxExpr[a_, "LocalScopeConflict"]
	]
	,
	TestID -> "a_ :> a_"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ :> a__]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] :>
			syntaxExpr[a__, "LocalScopeConflict"]
	]
	,
	TestID -> "a_ :> a__"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ :> a___]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] :>
			syntaxExpr[a___, "LocalScopeConflict"]
	]
	,
	TestID -> "a_ :> a___"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ :> _a]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] :>
			syntaxExpr[_a, "PatternVariable"]
	]
	,
	TestID -> "a_ :> _a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ :> __a]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] :>
			syntaxExpr[__a, "PatternVariable"]
	]
	,
	TestID -> "a_ :> __a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ :> ___a]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] :>
			syntaxExpr[___a, "PatternVariable"]
	]
	,
	TestID -> "a_ :> ___a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ \[RuleDelayed] a]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] :>
			syntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	]
	,
	TestID -> "a_ \\[RuleDelayed] a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ :> a_b]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] :>
			syntaxExpr[a_b, "LocalScopeConflict"]
	]
	,
	TestID -> "a_ :> a_b"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ :> b_a]
	,
	MakeBoxes[syntaxExpr[a_, "PatternVariable"] :> b_a]
	,
	TestID -> "a_ :> b_a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ b_ :> a b]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] syntaxExpr[b_, "PatternVariable"] :>
			syntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
			syntaxExpr[b, "PatternVariable", "UndefinedSymbol"]
	]
	,
	TestID -> "a_ b_ :> a b"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ a :> a]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] syntaxExpr[a, "UndefinedSymbol"] :>
			syntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
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
		syntaxExpr[a, "UndefinedSymbol"] := syntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a := a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ := a]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] :=
			syntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	]
	,
	TestID -> "a_ := a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a := a_]
	,
	MakeBoxes[syntaxExpr[a, "UndefinedSymbol"] := a_]
	,
	TestID -> "a := a_"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ b_ := a b]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] syntaxExpr[b_, "PatternVariable"] :=
			syntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
			syntaxExpr[b, "PatternVariable", "UndefinedSymbol"]
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
		syntaxExpr[a, "UndefinedSymbol"] ^:= syntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a ^:= a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ ^:= a]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] ^:=
			syntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	]
	,
	TestID -> "a_ ^:= a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a ^:= a_]
	,
	MakeBoxes[syntaxExpr[a, "UndefinedSymbol"] ^:= a_]
	,
	TestID -> "a ^:= a_"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ b_ ^:= a b]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] syntaxExpr[b_, "PatternVariable"] ^:=
			syntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
			syntaxExpr[b, "PatternVariable", "UndefinedSymbol"]
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
		syntaxExpr[a, "UndefinedSymbol"] /: syntaxExpr[a, "UndefinedSymbol"] :=
			syntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a /: a := a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ /: a := a]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] /:
			syntaxExpr[a, "UndefinedSymbol"] :=
				syntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	]
	,
	TestID -> "a_ /: a := a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a /: a_ := a]
	,
	MakeBoxes[
		syntaxExpr[a, "UndefinedSymbol"] /:
			syntaxExpr[a_, "PatternVariable"] :=
				syntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	]
	,
	TestID -> "a /: a_ := a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a /: a := a_]
	,
	MakeBoxes[
		syntaxExpr[a, "UndefinedSymbol"] /:
			syntaxExpr[a, "UndefinedSymbol"] := a_
	]
	,
	TestID -> "a /: a := a_"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ /: a_ := a]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] /:
			syntaxExpr[a_, "PatternVariable"] :=
				syntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	]
	,
	TestID -> "a_ /: a_ := a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ /: a := a_]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] /:
			syntaxExpr[a, "UndefinedSymbol"] :=
				syntaxExpr[a_, "LocalScopeConflict"]
	]
	,
	TestID -> "a_ /: a := a_"
]


Test[
	AnnotateSyntax @ MakeBoxes[a /: a_ := a_]
	,
	MakeBoxes[
		syntaxExpr[a, "UndefinedSymbol"] /:
			syntaxExpr[a_, "PatternVariable"] :=
				syntaxExpr[a_, "LocalScopeConflict"]
	]
	,
	TestID -> "a /: a_ := a_"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ /: a_ := a_]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] /:
			syntaxExpr[a_, "PatternVariable"] :=
				syntaxExpr[a_, "LocalScopeConflict"]
	]
	,
	TestID -> "a_ /: a_ := a_"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ b_ /: a b := a b]
	,
	MakeBoxes[
		syntaxExpr[a_, "PatternVariable"] syntaxExpr[b_, "PatternVariable"] /:
			syntaxExpr[a, "UndefinedSymbol"] *
			syntaxExpr[b, "UndefinedSymbol"] :=
				syntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
				syntaxExpr[b, "PatternVariable", "UndefinedSymbol"]
	]
	,
	TestID -> "a_ b_ /: a b := a b"
]


Test[
	AnnotateSyntax @ MakeBoxes[a b /: a_ b_ := a b]
	,
	MakeBoxes[
		syntaxExpr[a, "UndefinedSymbol"] syntaxExpr[b, "UndefinedSymbol"] /:
			syntaxExpr[a_, "PatternVariable"] *
			syntaxExpr[b_, "PatternVariable"] :=
				syntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
				syntaxExpr[b, "PatternVariable", "UndefinedSymbol"]
	]
	,
	TestID -> "a b /: a_ b_ := a b"
]


Test[
	AnnotateSyntax @ MakeBoxes[a b /: a b := a_ b_]
	,
	MakeBoxes[
		syntaxExpr[a, "UndefinedSymbol"] syntaxExpr[b, "UndefinedSymbol"] /:
			syntaxExpr[a, "UndefinedSymbol"] *
			syntaxExpr[b, "UndefinedSymbol"] :=
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
