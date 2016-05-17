(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["SyntaxAnnotations`Tests`Integration`MixedNested`", {"MUnit`"}]


Get["SyntaxAnnotations`Tests`Integration`init`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Scoping*)


(* ::Subsubsection:: *)
(*Patterns*)


Test[
	With[{a}, a b_ -> a b] // MakeBoxes // AnnotateSyntax
	,
	With[{SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]},
		SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] *
		SyntaxExpr[b_, "PatternVariable"] ->
			SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] *
			SyntaxExpr[b, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "With[{a}, a b_ -> a b]"
]


Test[
	Block[{a}, a_ = a] // MakeBoxes // AnnotateSyntax
	,
	Block[{SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]},
		SyntaxExpr[a_, "FunctionLocalVariable", "PatternVariable"] =
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Block[{a}, a_ = a]"
]


Test[
	Module[{a = (a -> a)}, a] // MakeBoxes // AnnotateSyntax
	,
	Module[
		{
			SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] = (
				SyntaxExpr[a, "UndefinedSymbol"] ->
					SyntaxExpr[a, "UndefinedSymbol"]
			)
		},
		SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Module[{a = (a -> a)}, a]"
]
Test[
	Block[{a = (a = a)}, a] // MakeBoxes // AnnotateSyntax
	,
	Block[
		{
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] = (
				SyntaxExpr[a, "UndefinedSymbol"] =
					SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
			)
		},
		SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Block[{a = (a = a)}, a]"
]


Test[
	With[{a = (a -> Block[{b}, a b])}, a b] // MakeBoxes // AnnotateSyntax
	,
	With[
		{
			SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] = (
				SyntaxExpr[a, "UndefinedSymbol"] ->
					Block[
						{
							SyntaxExpr[b,
								"FunctionLocalVariable", "UndefinedSymbol"
							]
						},
						SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] *
						SyntaxExpr[b,
							"FunctionLocalVariable", "UndefinedSymbol"
						]
					]
			)
		},
		SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] *
		SyntaxExpr[b, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "With[{a = (a -> Block[{b}, a b])}, a b]"
]


(* ::Subsubsection:: *)
(*Patterns Delayed*)


Test[
	Block[{a}, a b_ :> a b b_] // MakeBoxes // AnnotateSyntax
	,
	Block[{SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]},
		SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		SyntaxExpr[b_, "PatternVariable"] :>
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			SyntaxExpr[b, "PatternVariable", "UndefinedSymbol"] *
			SyntaxExpr[b_, "LocalScopeConflict"]
	] // MakeBoxes
	,
	TestID -> "Block[{a}, a b_ :> a b b_]"
]


Test[
	Module[{a}, a_ := a a_] // MakeBoxes // AnnotateSyntax
	,
	Module[{SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]},
		SyntaxExpr[a_, "LocalVariable", "PatternVariable"] :=
			SyntaxExpr[a,
				"LocalVariable", "PatternVariable", "UndefinedSymbol"
			] *
			SyntaxExpr[a_, "LocalVariable", "LocalScopeConflict"]
	] // MakeBoxes
	,
	TestID -> "Module[{a}, a_ := a a_]"
]


Test[
	Block[{a = (b_ := b)}, a b] // MakeBoxes // AnnotateSyntax
	,
	Block[
		{
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] = (
				SyntaxExpr[b_, "PatternVariable"] :=
					SyntaxExpr[b, "PatternVariable", "UndefinedSymbol"]
			)
		},
		SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		SyntaxExpr[b, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Block[{a = (b_ := b)}, a b]"
]


(* ::Subsubsection:: *)
(*Function*)


Test[
	With[{a = (a #1) &}, a] // MakeBoxes // AnnotateSyntax
	,
	With[
		{
			SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] = (
				SyntaxExpr[a, "UndefinedSymbol"] *
				SyntaxExpr[#1, "PatternVariable"]
			) &
		},
		SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "With[{a = Function[b, a]}, a b Function]"
]


Test[
	Module[{a = Function[b, a]}, a b Function] // MakeBoxes // AnnotateSyntax
	,
	Module[
		{
			SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] =
				Function[
					SyntaxExpr[b, "PatternVariable", "UndefinedSymbol"],
					SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]
				]
		},
		SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] *
		SyntaxExpr[b, "UndefinedSymbol"] *
		Function
	] // MakeBoxes
	,
	TestID -> "Module[{a = Function[b, a]}, a b Function]"
]


Test[
	Block[{a = RawBoxes@RowBox[{"a", "\[Function]", "a"}]}, a] //
		MakeBoxes // AnnotateSyntax
	,
	Block[
		{
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] =
				RawBoxes@RowBox[{
					SyntaxBox["a",
						"FunctionLocalVariable", "PatternVariable",
						"UndefinedSymbol"
					],
					"\[Function]",
					SyntaxBox["a", "PatternVariable", "UndefinedSymbol"]
				}]
		},
		SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Block[{a = a \\[Function] a}, a]"
]
Test[
	With[{a = RawBoxes@RowBox[{"b", "\[Function]", "a"}]}, a b] //
		MakeBoxes // AnnotateSyntax
	,
	With[
		{
			SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] =
				RawBoxes@RowBox[{
					SyntaxBox["b", "PatternVariable", "UndefinedSymbol"],
					"\[Function]",
					SyntaxBox["a", "UndefinedSymbol"]
				}]
		},
		SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] *
		SyntaxExpr[b, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Block[{a = b \\[Function] a}, a b]"
]
Test[
	Module[
		{
			a =
				RawBoxes@RowBox[
					{"b", "\[Function]", Block[{b}, a b] // MakeBoxes}
				]
		},
		a b
	] // MakeBoxes // AnnotateSyntax
	,
	Module[
		{
			SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] =
				RawBoxes@RowBox[{
					SyntaxBox["b", "PatternVariable", "UndefinedSymbol"],
					"\[Function]",
					Block[
						{
							SyntaxExpr[b,
								"PatternVariable", "FunctionLocalVariable",
								"UndefinedSymbol"
							]
						},
						SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] *
						SyntaxExpr[b,
							"PatternVariable", "FunctionLocalVariable",
							"UndefinedSymbol"
						]
					] // MakeBoxes
				}]
		}
		,
		SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] *
		SyntaxExpr[b, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Module[{a = b \\[Function] Block[{b}, a b]}, a b]"
]


(* ::Subsubsection:: *)
(*Functions*)


Test[
	Block[{a}, Plot[a b, {b, 0, 1}]] // MakeBoxes // AnnotateSyntax
	,
	Block[
		{SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]}
		,
		Plot[
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			SyntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"]
			,
			{SyntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"], 0, 1}
		]
	] // MakeBoxes
	,
	TestID -> "Block[{a}, Plot[a b, {b, 0, 1}]]"
]


Test[
	With[{a}, Limit[a, a -> a]] // MakeBoxes // AnnotateSyntax
	,
	With[
		{SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]}
		,
		Limit[
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
			,
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] ->
				SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
		]
	] // MakeBoxes
	,
	TestID -> "With[{a}, Limit[a, a -> a]]"
]


Test[
	Block[{a = Sum[a, {b, c, d}]}, a b c d] // MakeBoxes // AnnotateSyntax
	,
	Block[
		{
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] =
				Sum[
					SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
					{
						SyntaxExpr[b,
							"FunctionLocalVariable", "UndefinedSymbol"
						],
						SyntaxExpr[c, "UndefinedSymbol"],
						SyntaxExpr[d, "UndefinedSymbol"]
					}
				]
		},
		SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		SyntaxExpr[b, "UndefinedSymbol"] *
		SyntaxExpr[c, "UndefinedSymbol"] *
		SyntaxExpr[d, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Block[{a = Sum[a, {b, c, d}]}, a b c d]"
]


Test[
	Module[{a = a + Solve[a b, {b}]}, a b Solve] // MakeBoxes // AnnotateSyntax
	,
	Module[
		{
			SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] =
				SyntaxExpr[a, "UndefinedSymbol"] +
				Solve[
					SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] *
					SyntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"]
					,
					{SyntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"]}
				]
		}
		,
		SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] *
		SyntaxExpr[b, "UndefinedSymbol"] *
		Solve
	] // MakeBoxes
	,
	TestID -> "Module[{a = a + Solve[a b, {b}]}, a b Solve]"
]


(* ::Subsection:: *)
(*Patterns*)


(* ::Subsubsection:: *)
(*Scoping*)


Test[
	(With[{a}, a b_] ^= a b) // MakeBoxes // AnnotateSyntax
	,
	(
		With[
			{SyntaxExpr[a, "UndefinedSymbol"]},
			SyntaxExpr[a, "UndefinedSymbol"] SyntaxExpr[b_, "PatternVariable"]
		] ^=
			SyntaxExpr[a, "UndefinedSymbol"] SyntaxExpr[b, "UndefinedSymbol"]
	) // MakeBoxes
	,
	TestID -> "With[{a}, a b_] ^= a b"
]


Test[
	(a /: Module[{a}, a] = a) // MakeBoxes // AnnotateSyntax
	,
	(
		SyntaxExpr[a, "UndefinedSymbol"] /:
			Module[{SyntaxExpr[a, "UndefinedSymbol"]},
				SyntaxExpr[a, "UndefinedSymbol"]
			] = SyntaxExpr[a, "UndefinedSymbol"]
	) // MakeBoxes
	,
	TestID -> "a /: Module[{a}, a] = a"
]


Test[
	(a_ /: Block[{a_}, a_] = a_) // MakeBoxes // AnnotateSyntax
	,
	(
		SyntaxExpr[a_, "PatternVariable"] /:
			Block[{SyntaxExpr[a_, "PatternVariable"]},
				SyntaxExpr[a_, "PatternVariable"]
			] = a_
	) // MakeBoxes
	,
	TestID -> "a_ /: Block[{a_}, a_] = a_"
]


(* ::Subsubsection:: *)
(*Patterns Delayed*)


Test[
	((a_ := Function[a, a_]) = a a_) // MakeBoxes // AnnotateSyntax
	,
	(
		(
			SyntaxExpr[a_, "LocalScopeConflict", "PatternVariable"] :=
				Function[
					SyntaxExpr[a,
						"LocalScopeConflict", "PatternVariable",
						"UndefinedSymbol"
					],
					SyntaxExpr[a_, "LocalScopeConflict", "PatternVariable"]
				]
		) =
			SyntaxExpr[a, "UndefinedSymbol"] a_
	) // MakeBoxes
	,
	TestID -> "(a_ := Function[a, a_]) = a a_"
]


(* ::Subsubsection:: *)
(*Functions*)


Test[
	(Plot[a b_, {a, 0, 1}] ^= a b) // MakeBoxes // AnnotateSyntax
	,
	(
		Plot[
			SyntaxExpr[a, "UndefinedSymbol"] SyntaxExpr[b_, "PatternVariable"],
			{SyntaxExpr[a, "UndefinedSymbol"], 0, 1}
		] ^=
			SyntaxExpr[a, "UndefinedSymbol"] SyntaxExpr[b, "UndefinedSymbol"]
	) // MakeBoxes
	,
	TestID -> "Plot[a b_, {a, 0, 1}] ^= a b"
]


Test[
	(a /: Limit[a, a -> a] = a) // MakeBoxes // AnnotateSyntax
	,
	(
		SyntaxExpr[a, "UndefinedSymbol"] /:
			Limit[
				SyntaxExpr[a, "UndefinedSymbol"],
				SyntaxExpr[a, "UndefinedSymbol"] ->
					SyntaxExpr[a, "UndefinedSymbol"]
			] = SyntaxExpr[a, "UndefinedSymbol"]
	) // MakeBoxes
	,
	TestID -> "a /: Limit[a, a -> a] = a"
]


Test[
	(a_ /: Limit[a_, a_ -> a_] = a_) // MakeBoxes // AnnotateSyntax
	,
	(
		SyntaxExpr[a_, "PatternVariable"] /:
			Limit[
				SyntaxExpr[a_, "PatternVariable"],
				SyntaxExpr[a_, "PatternVariable"] ->
					SyntaxExpr[a_, "PatternVariable"]
			] = a_
	) // MakeBoxes
	,
	TestID -> "a_ /: Limit[a_, a_ -> a_] = a_"
]


(* ::Subsubsection:: *)
(*InfixMessageName*)


Test[
	RowBox[{RowBox[{"f", "::", "usage"}], "=", "\"\<something\>\""}] //
		AnnotateSyntax
	,
	RowBox[{
		RowBox[{
			SyntaxBox["f", "UndefinedSymbol"],
			"::",
			SyntaxBox["usage", "String"]
		}],
		"=",
		SyntaxBox["\"\<something\>\"", "String"]
	}]
	,
	TestID -> "f::usage = \"something\""
]


(* ::Subsection:: *)
(*Patterns Delayed*)


(* ::Subsubsection:: *)
(*Scoping*)


Test[
	(a_ ^:= With[{b}, a a_ b b_]) // MakeBoxes // AnnotateSyntax
	,
	(
		SyntaxExpr[a_, "PatternVariable"] ^:=
			With[{SyntaxExpr[b, "LocalVariable", "UndefinedSymbol"]},
				SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
				SyntaxExpr[a_, "LocalScopeConflict"] *
				SyntaxExpr[b, "LocalVariable", "UndefinedSymbol"] *
				SyntaxExpr[b_, "LocalVariable"]
			]
	) // MakeBoxes
	,
	TestID -> "a_ ^:= With[{b}, a a_ b b_]"
]


Test[
	(Module[{a}, a_ := Block[{a}, a a_]] := a) // MakeBoxes // AnnotateSyntax
	,
	(
		Module[{SyntaxExpr[a, "UndefinedSymbol"]},
			SyntaxExpr[a_, "LocalScopeConflict", "PatternVariable"] :=
				Block[
					{
						SyntaxExpr[a,
							"LocalScopeConflict", "FunctionLocalVariable",
							"UndefinedSymbol"
						]
					},
					SyntaxExpr[a,
						"LocalScopeConflict", "FunctionLocalVariable",
						"UndefinedSymbol"
					] *
					SyntaxExpr[a_,
						"LocalScopeConflict", "FunctionLocalVariable"
					]
				]
		] :=
			SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	) // MakeBoxes
	,
	TestID -> "(Module[{a}, a_ := Block[{a}, a a_]] := a)"
]


Test[
	(a /: a_ := Block[{a}, a a_]) // MakeBoxes // AnnotateSyntax
	,
	(
		SyntaxExpr[a, "UndefinedSymbol"] /:
			SyntaxExpr[a_, "PatternVariable"] :=
				Block[
					{
						SyntaxExpr[a,
							"LocalScopeConflict", "FunctionLocalVariable",
							"UndefinedSymbol"
						]
					}
					,
					SyntaxExpr[a,
						"LocalScopeConflict", "FunctionLocalVariable",
						"UndefinedSymbol"
					] *
					SyntaxExpr[a_,
						"LocalScopeConflict", "FunctionLocalVariable"
					]
				]
	) // MakeBoxes
	,
	TestID -> "a /: a_ := Block[{a}, a a_]"
]


(* ::Subsubsection:: *)
(*Patterns Delayed*)


Test[
	((a_ := With[{a}, a a_]) := a) // MakeBoxes // AnnotateSyntax
	,
	(
		(
			SyntaxExpr[a_, "LocalScopeConflict", "PatternVariable"] :=
				With[
					{
						SyntaxExpr[a,
							"LocalScopeConflict", "LocalVariable",
							"UndefinedSymbol"
						]
					},
					SyntaxExpr[a,
						"LocalScopeConflict", "LocalVariable",
						"UndefinedSymbol"
					] *
					SyntaxExpr[a_, "LocalScopeConflict", "LocalVariable"]
				]
		) :=
			SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	) // MakeBoxes
	,
	TestID -> "(a_ := With[{a}, a a_]) := a"
]
Test[
	((a_ := Module[{b}, a a_ b b_]) := a) // MakeBoxes // AnnotateSyntax
	,
	(
		(
			SyntaxExpr[a_, "LocalScopeConflict", "PatternVariable"] :=
				Module[{SyntaxExpr[b, "LocalVariable", "UndefinedSymbol"]},
					SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
					SyntaxExpr[a_, "LocalScopeConflict"] *
					SyntaxExpr[b, "LocalVariable", "UndefinedSymbol"] *
					SyntaxExpr[b_, "LocalVariable"]
				]
		) :=
			SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	) // MakeBoxes
	,
	TestID -> "(a_ := Module[{b}, a a_ b b_]) := a"
]


(* ::Subsection:: *)
(*Function*)


(* ::Subsubsection:: *)
(*Scoping*)


Test[
	Function[{a}, With[{b = a}, a b]] // MakeBoxes // AnnotateSyntax
	,
	Function[{SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]},
		With[
			{
				SyntaxExpr[b, "LocalVariable", "UndefinedSymbol"] =
					SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
			},
			SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
			SyntaxExpr[b, "LocalVariable", "UndefinedSymbol"]
		]
	] // MakeBoxes
	,
	TestID -> "Function[{a}, With[{b = a}, a b]]"
]


(* ::Subsubsection:: *)
(*Patterns Delayed*)


Test[
	Function[a, a_ :> a a_] // MakeBoxes // AnnotateSyntax
	,
	Function[
		SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
		,
		SyntaxExpr[a_, "LocalScopeConflict", "PatternVariable"] :>
			SyntaxExpr[a,
				"LocalScopeConflict", "PatternVariable", "UndefinedSymbol"
			] *
			SyntaxExpr[a_, "LocalScopeConflict"]
	] // MakeBoxes
	,
	TestID -> "Function[a, a_ :> a a_]"
]


(* ::Subsection:: *)
(*Functions*)


(* ::Subsubsection:: *)
(*Scoping*)


Test[
	Solve[With[{b}, a b], a] // MakeBoxes // AnnotateSyntax
	,
	Solve[
		With[{SyntaxExpr[b, "LocalVariable", "UndefinedSymbol"]},
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			SyntaxExpr[b, "LocalVariable", "UndefinedSymbol"]
		]
		,
		SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
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
				SyntaxExpr[a,
					"LocalVariable", "FunctionLocalVariable", "UndefinedSymbol"
				]
			}
			,
			SyntaxExpr[a,
				"LocalVariable", "FunctionLocalVariable", "UndefinedSymbol"
			]
		]
		,
		{SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"], 0, 1}
	] // MakeBoxes
	,
	TestID -> "Table[Module[{a}, a], {a, 0, 1}]"
]


(* ::Subsubsection:: *)
(*Patterns*)


Test[
	Solve[a b_ -> a b, a] // MakeBoxes // AnnotateSyntax
	,
	Solve[
		SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		SyntaxExpr[b_, "PatternVariable"] ->
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			SyntaxExpr[b, "UndefinedSymbol"]
		,
		SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Solve[a b_ -> a b, a]"
]


Test[
	Table[a_ = a, {a, 0, 1}] // MakeBoxes // AnnotateSyntax
	,
	Table[
		SyntaxExpr[a_, "PatternVariable", "FunctionLocalVariable"] =
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
		,
		{SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"], 0, 1}
	] // MakeBoxes
	,
	TestID -> "Table[a_ = a, {a, 0, 1}]"
]


(* ::Subsubsection:: *)
(*Patterns Delayed*)


Test[
	Solve[a b_ :> a b b_, a] // MakeBoxes // AnnotateSyntax
	,
	Solve[
		SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		SyntaxExpr[b_, "PatternVariable"] :>
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			SyntaxExpr[b, "PatternVariable", "UndefinedSymbol"] *
			SyntaxExpr[b_, "LocalScopeConflict"]
		,
		SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Solve[a b_ :> a b b_, a]"
]


Test[
	Table[a_ := a a_, {a, 0, 1}] // MakeBoxes // AnnotateSyntax
	,
	Table[
		SyntaxExpr[a_, "PatternVariable", "FunctionLocalVariable"] :=
			SyntaxExpr[a,
				"PatternVariable", "FunctionLocalVariable", "UndefinedSymbol"
			] *
			SyntaxExpr[a_, "LocalScopeConflict", "FunctionLocalVariable"]
		,
		{SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"], 0, 1}
	] // MakeBoxes
	,
	TestID -> "Table[a_ := a a_, {a, 0, 1}]"
]


(* ::Subsubsection:: *)
(*Functions*)


Test[
	(a_ ^:= Plot[a a_ b b_, {b, 0, 1}]) // MakeBoxes // AnnotateSyntax
	,
	(
		SyntaxExpr[a_, "PatternVariable"] ^:=
			Plot[
				SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
				SyntaxExpr[a_, "LocalScopeConflict"] *
				SyntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"] *
				SyntaxExpr[b_, "FunctionLocalVariable"]
				,
				{
					SyntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"],
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
		SyntaxExpr[a, "UndefinedSymbol"] /:
			SyntaxExpr[a_, "PatternVariable"] :=
				Limit[
					SyntaxExpr[a,
						"PatternVariable", "FunctionLocalVariable",
						"UndefinedSymbol"
					] *
					SyntaxExpr[a_,
						"LocalScopeConflict", "FunctionLocalVariable"
					]
					,
					SyntaxExpr[a,
						"PatternVariable", "FunctionLocalVariable",
						"UndefinedSymbol"
					] ->
						SyntaxExpr[a,
							"PatternVariable", "FunctionLocalVariable",
							"UndefinedSymbol"
						]
				]
	) // MakeBoxes
	,
	TestID -> "a /: a_ := Limit[a a_, a -> a]"
]


(* ::Subsubsection:: *)
(*String*)


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


(* ::Subsection:: *)
(*InfixMessageName*)


(* ::Subsubsection:: *)
(*Scopping*)


Test[
	RowBox[{
		RowBox[{"Module", "[", RowBox[{
			RowBox[{"{", "a", "}"}],
			",",
			"a"
		}], "]"}],
		"::",
		"a"
	}] // AnnotateSyntax
	,
	RowBox[{
		RowBox[{"Module", "[", RowBox[{
			RowBox[{"{",
				SyntaxBox["a", "LocalVariable", "UndefinedSymbol"],
			"}"}],
			",",
			SyntaxBox["a", "LocalVariable", "UndefinedSymbol"]
		}], "]"}],
		"::",
		SyntaxBox["a", "String"]
	}]
	,
	TestID -> "Module[{a}, a]::a"
]

Test[
	RowBox[{
		"b",
		"::",
		RowBox[{"With", "[", RowBox[{
			RowBox[{"{", RowBox[{"b", "=", "b"}], "}"}],
			",",
			"b"
		}], "]"}]
	}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["b", "UndefinedSymbol"],
		"::",
		SyntaxBox[
			RowBox[{"With", "[", RowBox[{
				RowBox[{"{", RowBox[{"b", "=", "b"}], "}"}],
				",",
				"b"
			}], "]"}]
			,
			"String"
		]
	}]
	,
	TestID -> "b::With[{b = b}, b]"
]


(* ::Subsubsection:: *)
(*Patterns*)


Test[
	RowBox[{
		"sym",
		"::",
		"msg",
		"::",
		"lang",
		"::",
		RowBox[{"x_", "->", "x"}]
	}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["sym", "UndefinedSymbol"],
		"::",
		SyntaxBox["msg", "String"],
		"::",
		SyntaxBox["lang", "String"],
		SyntaxBox["::", "ExcessArgument"],
		SyntaxBox[RowBox[{"x_", "->", "x"}], "ExcessArgument"]
	}]
	,
	TestID -> "sym::msg::lang::x_->x"
]


(* ::Subsubsection:: *)
(*Functions*)


Test[
	RowBox[{
		"c",
		"::",
		"d",
		"::",
		RowBox[{"Sum", "[", RowBox[{
			RowBox[{"c", " ", "d", " ", "e", " ", "f"}],
			",",
			RowBox[{"{", RowBox[{"e", ",", "f"}], "}"}]
		}], "]"}]
	}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["c", "UndefinedSymbol"],
		"::",
		SyntaxBox["d", "String"],
		"::",
		SyntaxBox[
			RowBox[{"Sum", "[", RowBox[{
				RowBox[{"c", " ", "d", " ", "e", " ", "f"}],
				",",
				RowBox[{"{", RowBox[{"e", ",", "f"}], "}"}]
			}], "]"}]
			,
			"String"
		]
	}]
	,
	TestID -> "c::d::Sum[c d e f, {e, f}]"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
