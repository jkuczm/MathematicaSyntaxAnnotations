(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["SyntaxAnnotations`Tests`Integration`PatternsDelayedNested`",
	{"MUnit`"}
]


Get["SyntaxAnnotations`Tests`Integration`init`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*RuleDelayed*)


Test[
	AnnotateSyntax @ MakeBoxes[a_ :> (b_ :> a b)]
	,
	MakeBoxes[
		 syntaxExpr[a_, "PatternVariable"] :> (
		 	syntaxExpr[b_, "PatternVariable"] :>
		 		syntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
		 		syntaxExpr[b, "PatternVariable", "UndefinedSymbol"]
		 )
	]
	,
	TestID -> "a_ :> (b_ :> a b)"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ :> (a_ :> a)]
	,
	MakeBoxes[
		 syntaxExpr[a_, "PatternVariable"] :> (
		 	syntaxExpr[a_, "LocalScopeConflict", "PatternVariable"] :>
		 		syntaxExpr[a,
		 			"LocalScopeConflict", "PatternVariable", "UndefinedSymbol"
		 		]
		 )
	]
	,
	TestID -> "a_ :> (a_ :> a)"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ :> (a :> a_)]
	,
	MakeBoxes[
		 syntaxExpr[a_, "PatternVariable"] :> (
		 	syntaxExpr[a, "PatternVariable", "UndefinedSymbol"] :>
		 		syntaxExpr[a_, "LocalScopeConflict"]
		 )
	]
	,
	TestID -> "a_ :> (a :> a_)"
]


(* ::Subsection:: *)
(*SetDelayed*)


Test[
	AnnotateSyntax @ MakeBoxes[a_ := (b_ := a b)]
	,
	MakeBoxes[
		 syntaxExpr[a_, "PatternVariable"] := (
		 	syntaxExpr[b_, "PatternVariable"] :=
		 		syntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
		 		syntaxExpr[b, "PatternVariable", "UndefinedSymbol"]
		 )
	]
	,
	TestID -> "a_ := (b_ := a b)"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ := (a_ := a)]
	,
	MakeBoxes[
		 syntaxExpr[a_, "PatternVariable"] := (
		 	syntaxExpr[a_, "LocalScopeConflict", "PatternVariable"] :=
		 		syntaxExpr[a,
		 			"LocalScopeConflict", "PatternVariable", "UndefinedSymbol"
		 		]
		 )
	]
	,
	TestID -> "a_ := (a_ := a)"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ := (a := a_)]
	,
	MakeBoxes[
		 syntaxExpr[a_, "PatternVariable"] := (
		 	syntaxExpr[a, "PatternVariable", "UndefinedSymbol"] :=
		 		syntaxExpr[a_, "LocalScopeConflict"]
		 )
	]
	,
	TestID -> "a_ := (a := a_)"
]


(* ::Subsection:: *)
(*UpSetDelayed*)


Test[
	AnnotateSyntax @ MakeBoxes[a_ ^:= (b_ ^:= a b)]
	,
	MakeBoxes[
		 syntaxExpr[a_, "PatternVariable"] ^:= (
		 	syntaxExpr[b_, "PatternVariable"] ^:=
		 		syntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
		 		syntaxExpr[b, "PatternVariable", "UndefinedSymbol"]
		 )
	]
	,
	TestID -> "a_ ^:= (b_ ^:= a b)"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ ^:= (a_ ^:= a)]
	,
	MakeBoxes[
		 syntaxExpr[a_, "PatternVariable"] ^:= (
		 	syntaxExpr[a_, "LocalScopeConflict", "PatternVariable"] ^:=
		 		syntaxExpr[a,
		 			"LocalScopeConflict", "PatternVariable", "UndefinedSymbol"
		 		]
		 )
	]
	,
	TestID -> "a_ ^:= (a_ ^:= a)"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ ^:= (a ^:= a_)]
	,
	MakeBoxes[
		 syntaxExpr[a_, "PatternVariable"] ^:= (
		 	syntaxExpr[a, "PatternVariable", "UndefinedSymbol"] ^:=
		 		syntaxExpr[a_, "LocalScopeConflict"]
		 )
	]
	,
	TestID -> "a_ ^:= (a ^:= a_)"
]


(* ::Subsection:: *)
(*TagSetDelayed*)


Test[
	AnnotateSyntax @ MakeBoxes[a_ /: a := (b_ := a b)]
	,
	MakeBoxes[
		 syntaxExpr[a_, "PatternVariable"] /:
		 	syntaxExpr[a, "UndefinedSymbol"] := (
		 		syntaxExpr[b_, "PatternVariable"] :=
		 			syntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
		 			syntaxExpr[b, "PatternVariable", "UndefinedSymbol"]
		 	)
	]
	,
	TestID -> "a_ /: a := (b_ := a b)"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ /: a := (a_ := a)]
	,
	MakeBoxes[
		 syntaxExpr[a_, "PatternVariable"] /:
		 	syntaxExpr[a, "UndefinedSymbol"] := (
		 		syntaxExpr[a_, "LocalScopeConflict", "PatternVariable"] :=
		 			syntaxExpr[a,
		 				"LocalScopeConflict", "PatternVariable",
		 				"UndefinedSymbol"
		 			]
		 	)
	]
	,
	TestID -> "a_ /: a := (a_ := a)"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ /: a := (a := a_)]
	,
	MakeBoxes[
		 syntaxExpr[a_, "PatternVariable"] /:
		 	syntaxExpr[a, "UndefinedSymbol"] := (
		 		syntaxExpr[a, "PatternVariable", "UndefinedSymbol"] :=
		 			syntaxExpr[a_, "LocalScopeConflict"]
		 	)
	]
	,
	TestID -> "a_ /: a := (a := a_)"
]


Test[
	AnnotateSyntax @ MakeBoxes[a /: a_ := (a_ := a)]
	,
	MakeBoxes[
		 syntaxExpr[a, "UndefinedSymbol"] /:
		 	syntaxExpr[a_, "PatternVariable"] := (
		 		syntaxExpr[a_, "LocalScopeConflict", "PatternVariable"] :=
		 			syntaxExpr[a,
		 				"LocalScopeConflict", "PatternVariable",
		 				"UndefinedSymbol"
		 			]
		 	)
	]
	,
	TestID -> "a /: a_ := (a_ := a)"
]


Test[
	AnnotateSyntax @ MakeBoxes[a /: a_ := (a := a_)]
	,
	MakeBoxes[
		 syntaxExpr[a, "UndefinedSymbol"] /:
		 	syntaxExpr[a_, "PatternVariable"] := (
		 		syntaxExpr[a, "PatternVariable", "UndefinedSymbol"] :=
		 			syntaxExpr[a_, "LocalScopeConflict"]
		 	)
	]
	,
	TestID -> "a /: a_ := (a := a_)"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
