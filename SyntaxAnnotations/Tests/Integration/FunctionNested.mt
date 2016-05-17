(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["SyntaxAnnotations`Tests`Integration`FunctionNested`", {"MUnit`"}]


Get["SyntaxAnnotations`Tests`Integration`init`"]


(* ::Section:: *)
(*Tests*)


Test[
	Function[a, Function[a, a]] // MakeBoxes // AnnotateSyntax
	,
	Function[
		SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"],
		Function[
			SyntaxExpr[a,
				"LocalScopeConflict", "PatternVariable", "UndefinedSymbol"
			],
			SyntaxExpr[a,
				"LocalScopeConflict", "PatternVariable", "UndefinedSymbol"
			]
		]
	] // MakeBoxes
	,
	TestID -> "Function[a, Function[a, a]]"
]
Test[
	Function[a, Function[{a}, a]] // MakeBoxes // AnnotateSyntax
	,
	Function[
		SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"],
		Function[
			{
				SyntaxExpr[a,
					"LocalScopeConflict", "PatternVariable", "UndefinedSymbol"
				]
			},
			SyntaxExpr[a,
				"LocalScopeConflict", "PatternVariable", "UndefinedSymbol"
			]
		]
	] // MakeBoxes
	,
	TestID -> "Function[a, Function[{a}, a]]"
]
Test[
	Function[{a}, Function[a, a]] // MakeBoxes // AnnotateSyntax
	,
	Function[
		{SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]},
		Function[
			SyntaxExpr[a,
				"LocalScopeConflict", "PatternVariable", "UndefinedSymbol"
			],
			SyntaxExpr[a,
				"LocalScopeConflict", "PatternVariable", "UndefinedSymbol"
			]
		]
	] // MakeBoxes
	,
	TestID -> "Function[{a}, Function[a, a]]"
]
Test[
	Function[{a}, Function[{a}, a]] // MakeBoxes // AnnotateSyntax
	,
	Function[
		{SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]},
		Function[
			{
				SyntaxExpr[a,
					"LocalScopeConflict", "PatternVariable", "UndefinedSymbol"
				]
			},
			SyntaxExpr[a,
				"LocalScopeConflict", "PatternVariable", "UndefinedSymbol"
			]
		]
	] // MakeBoxes
	,
	TestID -> "Function[{a}, Function[{a}, a]]"
]

Test[
	Function[Function[a, a], a Function] // MakeBoxes // AnnotateSyntax
	,
	Function[
		Function[
			SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"],
			SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
		],
		SyntaxExpr[a, "UndefinedSymbol"] Function
	] // MakeBoxes
	,
	TestID -> "Function[Function[a, a], a Function]"
]
Test[
	Function[Function[{a}, a], a Function] // MakeBoxes // AnnotateSyntax
	,
	Function[
		Function[
			{SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]},
			SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
		],
		SyntaxExpr[a, "UndefinedSymbol"] Function
	] // MakeBoxes
	,
	TestID -> "Function[Function[{a}, a], a Function]"
]
Test[
	Function[{Function[a, a]}, a Function] // MakeBoxes // AnnotateSyntax
	,
	Function[
		{SyntaxExpr[Function, "PatternVariable"][
			SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"],
			SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
		]},
		SyntaxExpr[a, "UndefinedSymbol"] *
		SyntaxExpr[Function, "PatternVariable"]
	] // MakeBoxes
	,
	TestID -> "Function[{Function[a, a]}, a Function]"
]
Test[
	Function[{Function[{a}, a]}, a Function] // MakeBoxes // AnnotateSyntax
	,
	Function[
		{SyntaxExpr[Function, "PatternVariable"][
			{SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]},
			SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
		]},
		SyntaxExpr[a, "UndefinedSymbol"] *
		SyntaxExpr[Function, "PatternVariable"]
	] // MakeBoxes
	,
	TestID -> "Function[{Function[{a}, a]}, a Function]"
]


Test[
	RowBox[{"Function", "[",
		RowBox[{"#", " ", RowBox[{"Function", "[", "##6", "]"}]}],
	"]"}] // AnnotateSyntax
	,
	RowBox[{"Function", "[",
		RowBox[{SyntaxBox["#", "PatternVariable"],
		" ",
		RowBox[{"Function", "[", SyntaxBox["##6", "PatternVariable"], "]"}]}],
	"]"}]
	,
	TestID -> "Function[# Function[##6]]"
]


Test[
	RowBox[{RowBox[{RowBox[{"Function", "[", "#2", "]"}], "+", "#2"}], "&"}] //
		AnnotateSyntax
	,
	RowBox[{RowBox[{
		RowBox[{"Function", "[", SyntaxBox["#2", "PatternVariable"], "]"}],
		"+",
		SyntaxBox["#2", "PatternVariable"]
	}], "&"}]
	,
	TestID -> "(Function[#2] #2)&"
]


Test[
	Function[a, (a #)&] // MakeBoxes // AnnotateSyntax
	,
	Function[
		SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"],
		(
			SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
			SyntaxExpr[#, "PatternVariable"]
		)&
	] // MakeBoxes
	,
	TestID -> "Function[a, (a #)&]"
]


Test[
	RowBox[{
		RowBox[{"a", "\[Function]", "b"}],
		"\[Function]",
		RowBox[{"a", " ", "b"}]}
	] // AnnotateSyntax
	,
	RowBox[{
		RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			"\[Function]",
			SyntaxBox["b", "PatternVariable", "UndefinedSymbol"]}],
		"\[Function]",
		RowBox[{
			SyntaxBox["a", "UndefinedSymbol"],
			" ",
			SyntaxBox["b", "PatternVariable", "UndefinedSymbol"]
		}]
	}]
	,
	TestID -> "((a \\[Function] b) \\[Function] a b)"
]


Test[
	RowBox[{
		RowBox[{"Function", "[",
			RowBox[{RowBox[{"{", "a", "}"}], ",", "b", ",", "c"}],
		"]"}],
		"\[Function]",
		RowBox[{"a", " ", "b", " ", "c", " ", "Function"}]
	}] // AnnotateSyntax
	,
	RowBox[{
		RowBox[{SyntaxBox["Function", "PatternVariable"], "[",
			RowBox[{
				RowBox[{"{",
					SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
				"}"}], ",",
				SyntaxBox["b", "UndefinedSymbol"], ",",
				SyntaxBox["c", "UndefinedSymbol"]
			}],
		"]"}],
		"\[Function]",
		RowBox[{
			SyntaxBox["a", "UndefinedSymbol"], " ",
			SyntaxBox["b", "UndefinedSymbol"], " ",
			SyntaxBox["c", "UndefinedSymbol"], " ",
			SyntaxBox["Function", "PatternVariable"]
		}]
	}]
	,
	TestID -> "Function[{a}, b, c] \\[Function] a b c Function"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
