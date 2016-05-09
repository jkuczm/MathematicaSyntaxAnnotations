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


(* ::Subsubsection:: *)
(*RuleDelayed*)


Test[
	AnnotateSyntax @ MakeBoxes[a_ :> (b_ :> a b)]
	,
	MakeBoxes[
		 SyntaxExpr[a_, "PatternVariable"] :> (
		 	SyntaxExpr[b_, "PatternVariable"] :>
		 		SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
		 		SyntaxExpr[b, "PatternVariable", "UndefinedSymbol"]
		 )
	]
	,
	TestID -> "a_ :> (b_ :> a b)"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ :> (a_ :> a)]
	,
	MakeBoxes[
		 SyntaxExpr[a_, "PatternVariable"] :> (
		 	SyntaxExpr[a_, "LocalScopeConflict", "PatternVariable"] :>
		 		SyntaxExpr[a,
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
		 SyntaxExpr[a_, "PatternVariable"] :> (
		 	SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] :>
		 		SyntaxExpr[a_, "LocalScopeConflict"]
		 )
	]
	,
	TestID -> "a_ :> (a :> a_)"
]


Test[
	AnnotateSyntax @ MakeBoxes[(a_ :> a_) :> a a_]
	,
	MakeBoxes[
		 (
		 	SyntaxExpr[a_, "PatternVariable"] :>
		 		SyntaxExpr[a_, "PatternVariable", "LocalScopeConflict"]
		 ) :>
		 	SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
		 	SyntaxExpr[a_, "LocalScopeConflict"]
	]
	,
	TestID -> "(a_ :> a_) :> a a_"
]


(* ::Subsubsection:: *)
(*SetDelayed*)


Test[
	AnnotateSyntax @ MakeBoxes[(a_ := a_) :> a a_]
	,
	MakeBoxes[
		 (
		 	SyntaxExpr[a_, "PatternVariable"] :=
		 		SyntaxExpr[a_, "PatternVariable", "LocalScopeConflict"]
		 ) :>
		 	SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
		 	SyntaxExpr[a_, "LocalScopeConflict"]
	]
	,
	TestID -> "(a_ := a_) :> a a_"
]


(* ::Subsection:: *)
(*SetDelayed*)


(* ::Subsubsection:: *)
(*SetDelayed*)


Test[
	AnnotateSyntax @ MakeBoxes[a_ := (b_ := a b)]
	,
	MakeBoxes[
		 SyntaxExpr[a_, "PatternVariable"] := (
		 	SyntaxExpr[b_, "PatternVariable"] :=
		 		SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
		 		SyntaxExpr[b, "PatternVariable", "UndefinedSymbol"]
		 )
	]
	,
	TestID -> "a_ := (b_ := a b)"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ := (a_ := a)]
	,
	MakeBoxes[
		 SyntaxExpr[a_, "PatternVariable"] := (
		 	SyntaxExpr[a_, "LocalScopeConflict", "PatternVariable"] :=
		 		SyntaxExpr[a,
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
		 SyntaxExpr[a_, "PatternVariable"] := (
		 	SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] :=
		 		SyntaxExpr[a_, "LocalScopeConflict"]
		 )
	]
	,
	TestID -> "a_ := (a := a_)"
]


Test[
	AnnotateSyntax @ MakeBoxes[(a_ := a_) := a a_]
	,
	MakeBoxes[
		 (
		 	SyntaxExpr[a_, "LocalScopeConflict", "PatternVariable"] :=
		 		SyntaxExpr[a_, "LocalScopeConflict"]
		 ) :=
		 	SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
		 	SyntaxExpr[a_, "LocalScopeConflict"]
	]
	,
	TestID -> "(a_ := a_) := a a_"
]


(* ::Subsubsection:: *)
(*RuleDelayed*)


Test[
	AnnotateSyntax @ MakeBoxes[(a_ :> a_) := a a_]
	,
	MakeBoxes[
		 (
		 	SyntaxExpr[a_, "PatternVariable"] :>
		 		SyntaxExpr[a_, "PatternVariable"]
		 ) :=
		 	SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
		 	SyntaxExpr[a_, "LocalScopeConflict"]
	]
	,
	TestID -> "(a_ :> a_) := a a_"
]


(* ::Subsection:: *)
(*UpSetDelayed*)


(* ::Subsubsection:: *)
(*UpSetDelayed*)


Test[
	AnnotateSyntax @ MakeBoxes[a_ ^:= (b_ ^:= a b)]
	,
	MakeBoxes[
		 SyntaxExpr[a_, "PatternVariable"] ^:= (
		 	SyntaxExpr[b_, "PatternVariable"] ^:=
		 		SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
		 		SyntaxExpr[b, "PatternVariable", "UndefinedSymbol"]
		 )
	]
	,
	TestID -> "a_ ^:= (b_ ^:= a b)"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ ^:= (a_ ^:= a)]
	,
	MakeBoxes[
		 SyntaxExpr[a_, "PatternVariable"] ^:= (
		 	SyntaxExpr[a_, "LocalScopeConflict", "PatternVariable"] ^:=
		 		SyntaxExpr[a,
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
		 SyntaxExpr[a_, "PatternVariable"] ^:= (
		 	SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] ^:=
		 		SyntaxExpr[a_, "LocalScopeConflict"]
		 )
	]
	,
	TestID -> "a_ ^:= (a ^:= a_)"
]


(* ::Subsection:: *)
(*TagSetDelayed*)


(* ::Subsubsection:: *)
(*TagSetDelayed*)


Test[
	AnnotateSyntax @ MakeBoxes[a_ /: a := (b_ := a b)]
	,
	MakeBoxes[
		 SyntaxExpr[a_, "PatternVariable"] /:
		 	SyntaxExpr[a, "UndefinedSymbol"] := (
		 		SyntaxExpr[b_, "PatternVariable"] :=
		 			SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
		 			SyntaxExpr[b, "PatternVariable", "UndefinedSymbol"]
		 	)
	]
	,
	TestID -> "a_ /: a := (b_ := a b)"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ /: a := (a_ := a)]
	,
	MakeBoxes[
		 SyntaxExpr[a_, "PatternVariable"] /:
		 	SyntaxExpr[a, "UndefinedSymbol"] := (
		 		SyntaxExpr[a_, "LocalScopeConflict", "PatternVariable"] :=
		 			SyntaxExpr[a,
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
		 SyntaxExpr[a_, "PatternVariable"] /:
		 	SyntaxExpr[a, "UndefinedSymbol"] := (
		 		SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] :=
		 			SyntaxExpr[a_, "LocalScopeConflict"]
		 	)
	]
	,
	TestID -> "a_ /: a := (a := a_)"
]


Test[
	AnnotateSyntax @ MakeBoxes[a /: a_ := (a_ := a)]
	,
	MakeBoxes[
		 SyntaxExpr[a, "UndefinedSymbol"] /:
		 	SyntaxExpr[a_, "PatternVariable"] := (
		 		SyntaxExpr[a_, "LocalScopeConflict", "PatternVariable"] :=
		 			SyntaxExpr[a,
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
		 SyntaxExpr[a, "UndefinedSymbol"] /:
		 	SyntaxExpr[a_, "PatternVariable"] := (
		 		SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] :=
		 			SyntaxExpr[a_, "LocalScopeConflict"]
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
