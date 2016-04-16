(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["SyntaxAnnotations`Tests`Integration`MixedNested`", {"MUnit`"}]


Get["SyntaxAnnotations`Tests`Integration`init`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Scoping in Functions*)


Test[
	Solve[With[{b}, a b], a] // MakeBoxes // AnnotateSyntax
	,
	Solve[
		With[{syntaxExpr[b, "LocalVariable", "UndefinedSymbol"]},
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			syntaxExpr[b, "LocalVariable", "UndefinedSymbol"]
		]
		,
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Solve[With[{b}, a b], a]"
]


Test[
	Table[Module[{a}, a], {a, 0, 1}] // MakeBoxes // AnnotateSyntax
	,
	Table[
		Module[
			{
				syntaxExpr[a,
					"LocalVariable", "FunctionLocalVariable", "UndefinedSymbol"
				]
			}
			,
			syntaxExpr[a,
				"LocalVariable", "FunctionLocalVariable", "UndefinedSymbol"
			]
		]
		,
		{syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"], 0, 1}
	] // MakeBoxes
	,
	TestID -> "Table[Module[{a}, a], {a, 0, 1}]"
]


(* ::Subsection:: *)
(*Functions in Scoping*)


Test[
	Block[{a}, Plot[a b, {b, 0, 1}]] // MakeBoxes // AnnotateSyntax
	,
	Block[
		{syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]}
		,
		Plot[
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			syntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"]
			,
			{syntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"], 0, 1}
		]
	] // MakeBoxes
	,
	TestID -> "Block[{a}, Plot[a b, {b, 0, 1}]]"
]


Test[
	With[{a}, Limit[a, a -> a]] // MakeBoxes // AnnotateSyntax
	,
	With[
		{syntaxExpr[a, "LocalVariable", "UndefinedSymbol"]}
		,
		Limit[
			syntaxExpr[a,
				"FunctionLocalVariable", "LocalVariable", "UndefinedSymbol"
			]
			,
			syntaxExpr[a,
				"FunctionLocalVariable", "LocalVariable", "UndefinedSymbol"
			] ->
				syntaxExpr[a,
					"FunctionLocalVariable", "LocalVariable", "UndefinedSymbol"
				]
		]
	] // MakeBoxes
	,
	TestID -> "With[{a}, Limit[a, a -> a]]"
]


(* ::Subsection:: *)
(*Patterns in Functions*)


Test[
	Solve[a b_ -> a b, a] // MakeBoxes // AnnotateSyntax
	,
	Solve[
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		syntaxExpr[b_, "PatternVariable"] ->
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			syntaxExpr[b, "UndefinedSymbol"]
		,
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Solve[a b_ -> a b, a]"
]


Test[
	Table[a_ = a, {a, 0, 1}] // MakeBoxes // AnnotateSyntax
	,
	Table[
		syntaxExpr[a_, "PatternVariable", "FunctionLocalVariable"] =
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
		,
		{syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"], 0, 1}
	] // MakeBoxes
	,
	TestID -> "Table[a_ = a, {a, 0, 1}]"
]


(* ::Subsection:: *)
(*Functions in Patterns LHS*)


Test[
	(Plot[a b_, {a, 0, 1}] ^= a b) // MakeBoxes // AnnotateSyntax
	,
	(
		Plot[
			syntaxExpr[a, "UndefinedSymbol"] syntaxExpr[b_, "PatternVariable"],
			{syntaxExpr[a, "UndefinedSymbol"], 0, 1}
		] ^=
			syntaxExpr[a, "UndefinedSymbol"] syntaxExpr[b, "UndefinedSymbol"]
	) // MakeBoxes
	,
	TestID -> "Plot[a b_, {a, 0, 1}] ^= a b"
]


Test[
	(a /: Limit[a, a -> a] = a) // MakeBoxes // AnnotateSyntax
	,
	(
		syntaxExpr[a, "UndefinedSymbol"] /:
			Limit[
				syntaxExpr[a, "UndefinedSymbol"],
				syntaxExpr[a, "UndefinedSymbol"] ->
					syntaxExpr[a, "UndefinedSymbol"]
			] = syntaxExpr[a, "UndefinedSymbol"]
	) // MakeBoxes
	,
	TestID -> "a /: Limit[a, a -> a] = a"
]


Test[
	(a_ /: Limit[a_, a_ -> a_] = a_) // MakeBoxes // AnnotateSyntax
	,
	(
		syntaxExpr[a_, "PatternVariable"] /:
			Limit[
				syntaxExpr[a_, "PatternVariable"],
				syntaxExpr[a_, "PatternVariable"] ->
					syntaxExpr[a_, "PatternVariable"]
			] = a_
	) // MakeBoxes
	,
	TestID -> "a_ /: Limit[a_, a_ -> a_] = a_"
]


(* ::Subsection:: *)
(*Patterns Delayed in Functions*)


Test[
	Solve[a b_ :> a b b_, a] // MakeBoxes // AnnotateSyntax
	,
	Solve[
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		syntaxExpr[b_, "PatternVariable"] :>
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			syntaxExpr[b, "PatternVariable", "UndefinedSymbol"] *
			syntaxExpr[b_, "LocalScopeConflict"]
		,
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Solve[a b_ :> a b b_, a]"
]


Test[
	Table[a_ := a a_, {a, 0, 1}] // MakeBoxes // AnnotateSyntax
	,
	Table[
		syntaxExpr[a_, "PatternVariable", "FunctionLocalVariable"] :=
			syntaxExpr[a,
				"PatternVariable", "FunctionLocalVariable", "UndefinedSymbol"
			] *
			syntaxExpr[a_, "LocalScopeConflict", "FunctionLocalVariable"]
		,
		{syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"], 0, 1}
	] // MakeBoxes
	,
	TestID -> "Table[a_ := a a_, {a, 0, 1}]"
]


(* ::Subsection:: *)
(*Functions in Patterns Delayed RHS*)


Test[
	(a_ ^:= Plot[a a_ b b_, {b, 0, 1}]) // MakeBoxes // AnnotateSyntax
	,
	(
		syntaxExpr[a_, "PatternVariable"] ^:=
			Plot[
				syntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
				syntaxExpr[a_, "LocalScopeConflict"] *
				syntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"] *
				syntaxExpr[b_, "FunctionLocalVariable"]
				,
				{
					syntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"],
					0,
					1
				}
			]
	) // MakeBoxes
	,
	TestID -> "a_ ^:= Plot[a a_ b b_, {b, 0, 1}]"
]


Test[
	(a /: a_ := Limit[a a_, a -> a]) // MakeBoxes // AnnotateSyntax
	,
	(
		syntaxExpr[a, "UndefinedSymbol"] /:
			syntaxExpr[a_, "PatternVariable"] :=
				Limit[
					syntaxExpr[a,
						"PatternVariable", "FunctionLocalVariable",
						"UndefinedSymbol"
					] *
					syntaxExpr[a_,
						"LocalScopeConflict", "FunctionLocalVariable"
					]
					,
					syntaxExpr[a,
						"PatternVariable", "FunctionLocalVariable",
						"UndefinedSymbol"
					] ->
						syntaxExpr[a,
							"PatternVariable", "FunctionLocalVariable",
							"UndefinedSymbol"
						]
				]
	) // MakeBoxes
	,
	TestID -> "a /: a_ := Limit[a a_, a -> a]"
]


(* ::Subsection:: *)
(*Patterns in Scoping*)


Test[
	With[{a}, a b_ -> a b] // MakeBoxes // AnnotateSyntax
	,
	With[{syntaxExpr[a, "LocalVariable", "UndefinedSymbol"]},
		 syntaxExpr[a, "LocalVariable", "UndefinedSymbol"] *
		 syntaxExpr[b_, "PatternVariable"] ->
		 	syntaxExpr[a, "LocalVariable", "UndefinedSymbol"] *
			syntaxExpr[b, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "With[{a}, a b_ -> a b]"
]


Test[
	Block[{a}, a_ = a] // MakeBoxes // AnnotateSyntax
	,
	Block[{syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]},
		 syntaxExpr[a_, "FunctionLocalVariable", "PatternVariable"] =
		 	syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Block[{a}, a_ = a]"
]


(* ::Subsection:: *)
(*Scoping in Patterns LHS*)


Test[
	(With[{a}, a b_] ^= a b) // MakeBoxes // AnnotateSyntax
	,
	(
		With[
			{syntaxExpr[a, "UndefinedSymbol"]},
			syntaxExpr[a, "UndefinedSymbol"] syntaxExpr[b_, "PatternVariable"]
		] ^=
			syntaxExpr[a, "UndefinedSymbol"] syntaxExpr[b, "UndefinedSymbol"]
	) // MakeBoxes
	,
	TestID -> "With[{a}, a b_] ^= a b"
]


Test[
	(a /: Module[{a}, a] = a) // MakeBoxes // AnnotateSyntax
	,
	(
		syntaxExpr[a, "UndefinedSymbol"] /:
			Module[{syntaxExpr[a, "UndefinedSymbol"]},
				syntaxExpr[a, "UndefinedSymbol"]
			] = syntaxExpr[a, "UndefinedSymbol"]
	) // MakeBoxes
	,
	TestID -> "a /: Module[{a}, a] = a"
]


Test[
	(a_ /: Block[{a_}, a_] = a_) // MakeBoxes // AnnotateSyntax
	,
	(
		syntaxExpr[a_, "PatternVariable"] /:
			Block[{syntaxExpr[a_, "PatternVariable"]},
				syntaxExpr[a_, "PatternVariable"]
			] = a_
	) // MakeBoxes
	,
	TestID -> "a_ /: Block[{a_}, a_] = a_"
]


(* ::Subsection:: *)
(*Patterns Delayed in Scoping*)


Test[
	Block[{a}, a b_ :> a b b_] // MakeBoxes // AnnotateSyntax
	,
	Block[{syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]},
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		syntaxExpr[b_, "PatternVariable"] :>
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			syntaxExpr[b, "PatternVariable", "UndefinedSymbol"] *
			syntaxExpr[b_, "LocalScopeConflict"]
	] // MakeBoxes
	,
	TestID -> "Block[{a}, a b_ :> a b b_]"
]


Test[
	Module[{a}, a_ := a a_] // MakeBoxes // AnnotateSyntax
	,
	Module[{syntaxExpr[a, "LocalVariable", "UndefinedSymbol"]},
		syntaxExpr[a_, "LocalVariable", "PatternVariable"] :=
			syntaxExpr[a,
				"LocalVariable", "PatternVariable", "UndefinedSymbol"
			] *
			syntaxExpr[a_, "LocalVariable", "LocalScopeConflict"]
	] // MakeBoxes
	,
	TestID -> "Module[{a}, a_ := a a_]"
]


(* ::Subsection:: *)
(*Scoping in Patterns Delayed RHS*)


Test[
	(a_ ^:= With[{b}, a a_ b b_]) // MakeBoxes // AnnotateSyntax
	,
	(
		syntaxExpr[a_, "PatternVariable"] ^:=
			With[{syntaxExpr[b, "LocalVariable", "UndefinedSymbol"]},
				syntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
				syntaxExpr[a_, "LocalScopeConflict"] *
				syntaxExpr[b, "LocalVariable", "UndefinedSymbol"] *
				syntaxExpr[b_, "LocalVariable"]
			]
	) // MakeBoxes
	,
	TestID -> "a_ ^:= With[{b}, a a_ b b_]"
]


Test[
	(a /: a_ := Block[{a}, a a_]) // MakeBoxes // AnnotateSyntax
	,
	(
		syntaxExpr[a, "UndefinedSymbol"] /:
			syntaxExpr[a_, "PatternVariable"] :=
				Block[
					{
						syntaxExpr[a,
							"LocalScopeConflict", "FunctionLocalVariable",
							"UndefinedSymbol"
						]
					}
					,
					syntaxExpr[a,
						"LocalScopeConflict", "FunctionLocalVariable",
						"UndefinedSymbol"
					] *
					syntaxExpr[a_,
						"LocalScopeConflict", "FunctionLocalVariable"
					]
				]
	) // MakeBoxes
	,
	TestID -> "a /: a_ := Block[{a}, a a_]"
]


(* ::Subsection:: *)
(*String in Functions*)


Test[
	RowBox[{"Integrate", "[", RowBox[{
		RowBox[{
			"a",
			"\"\<a\>\""
		}],
		",",
		"a"
	}],"]"}] // AnnotateSyntax
	,
	RowBox[{"Integrate", "[", RowBox[{
		RowBox[{
			SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
			SyntaxBox["\"\<a\>\"", "String"]
		}],
		",",
		SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"]
	}],"]"}]
	,
	TestID -> "Integrate[a \"a\", a]"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
