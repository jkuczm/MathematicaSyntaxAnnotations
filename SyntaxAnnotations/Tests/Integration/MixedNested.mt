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


(* ::Subsection:: *)
(*Functions in Scoping*)


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
			SyntaxExpr[a,
				"FunctionLocalVariable", "LocalVariable", "UndefinedSymbol"
			]
			,
			SyntaxExpr[a,
				"FunctionLocalVariable", "LocalVariable", "UndefinedSymbol"
			] ->
				SyntaxExpr[a,
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


(* ::Subsection:: *)
(*Functions in Patterns LHS*)


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


(* ::Subsection:: *)
(*Patterns Delayed in Functions*)


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


(* ::Subsection:: *)
(*Functions in Patterns Delayed RHS*)


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


(* ::Subsection:: *)
(*Patterns in Scoping*)


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


(* ::Subsection:: *)
(*Scoping in Patterns LHS*)


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


(* ::Subsection:: *)
(*Patterns Delayed in Scoping*)


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


(* ::Subsection:: *)
(*Scoping in Patterns Delayed RHS*)


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


(* ::Subsection:: *)
(*Scopping in InfixMessageName*)


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


(* ::Subsection:: *)
(*Functions in InfixMessageName*)


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


(* ::Subsection:: *)
(*Patterns in InfixMessageName*)


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


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
