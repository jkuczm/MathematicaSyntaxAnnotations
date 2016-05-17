(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["SyntaxAnnotations`Tests`Integration`FunctionsNested`",
	{"MUnit`"}
]


Get["SyntaxAnnotations`Tests`Integration`init`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Table*)


Test[
	Table[Table[a b c d e f, {a, b, c}], {d, e, f}] //
		MakeBoxes // AnnotateSyntax
	,
	Table[
		Table[
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			SyntaxExpr[b, "UndefinedSymbol"] *
			SyntaxExpr[c, "UndefinedSymbol"] *
			SyntaxExpr[d, "FunctionLocalVariable", "UndefinedSymbol"] *
			SyntaxExpr[e, "UndefinedSymbol"] *
			SyntaxExpr[f, "UndefinedSymbol"]
			,
			{
				SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
				SyntaxExpr[b, "UndefinedSymbol"],
				SyntaxExpr[c, "UndefinedSymbol"]
			}
		]
		,
		{
			SyntaxExpr[d, "FunctionLocalVariable", "UndefinedSymbol"],
			SyntaxExpr[e, "UndefinedSymbol"],
			SyntaxExpr[f, "UndefinedSymbol"]
		}
	] // MakeBoxes
	,
	TestID -> "Table[Table[a b c d e f, {a, b, c}], {d, e, f}]"
]


Test[
	Table[Table[a b c, {a, b, c}], {a, b, c}] // MakeBoxes // AnnotateSyntax
	,
	Table[
		Table[
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			SyntaxExpr[b, "UndefinedSymbol"] *
			SyntaxExpr[c, "UndefinedSymbol"]
			,
			{
				SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
				SyntaxExpr[b, "UndefinedSymbol"],
				SyntaxExpr[c, "UndefinedSymbol"]
			}
		]
		,
		{
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
			SyntaxExpr[b, "UndefinedSymbol"],
			SyntaxExpr[c, "UndefinedSymbol"]
		}
	] // MakeBoxes
	,
	TestID -> "Table[Table[a b c, {a, b, c}], {a, b, c}]"
]


(* ::Subsection:: *)
(*Plot*)


Test[
	Plot[Plot[a b c d e f, {a, b, c}], {d, e, f}] //
		MakeBoxes // AnnotateSyntax
	,
	Plot[
		Plot[
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			SyntaxExpr[b, "UndefinedSymbol"] *
			SyntaxExpr[c, "UndefinedSymbol"] *
			SyntaxExpr[d, "FunctionLocalVariable", "UndefinedSymbol"] *
			SyntaxExpr[e, "UndefinedSymbol"] *
			SyntaxExpr[f, "UndefinedSymbol"]
			,
			{
				SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
				SyntaxExpr[b, "UndefinedSymbol"],
				SyntaxExpr[c, "UndefinedSymbol"]
			}
		]
		,
		{
			SyntaxExpr[d, "FunctionLocalVariable", "UndefinedSymbol"],
			SyntaxExpr[e, "UndefinedSymbol"],
			SyntaxExpr[f, "UndefinedSymbol"]
		}
	] // MakeBoxes
	,
	TestID -> "Plot[Plot[a b c d e f, {a, b, c}], {d, e, f}]"
]


Test[
	Plot[Plot[a b c, {a, b, c}], {a, b, c}] // MakeBoxes // AnnotateSyntax
	,
	Plot[
		Plot[
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			SyntaxExpr[b, "UndefinedSymbol"] *
			SyntaxExpr[c, "UndefinedSymbol"]
			,
			{
				SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
				SyntaxExpr[b, "UndefinedSymbol"],
				SyntaxExpr[c, "UndefinedSymbol"]
			}
		]
		,
		{
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
			SyntaxExpr[b, "UndefinedSymbol"],
			SyntaxExpr[c, "UndefinedSymbol"]
		}
	] // MakeBoxes
	,
	TestID -> "Plot[Plot[a b c, {a, b, c}], {a, b, c}]"
]


(* ::Subsection:: *)
(*Solve*)


Test[
	Solve[Solve[a b, b], a] // MakeBoxes // AnnotateSyntax
	,
	Solve[
		Solve[
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			SyntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"]
			,
			SyntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"]
		]
		,
		SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Solve[Solve[a b, b], a]"
]


Test[
	Solve[Solve[a, a], a] // MakeBoxes // AnnotateSyntax
	,
	Solve[
		Solve[
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
		]
		,
		SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Solve[Solve[a, a], a]"
]


(* ::Subsection:: *)
(*Limit*)


Test[
	Limit[Limit[a b c d, c -> d], a -> b] // MakeBoxes // AnnotateSyntax
	,
	Limit[
		Limit[
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			SyntaxExpr[b, "UndefinedSymbol"] *
			SyntaxExpr[c, "FunctionLocalVariable", "UndefinedSymbol"] *
			SyntaxExpr[d, "UndefinedSymbol"]
			,
			SyntaxExpr[c, "FunctionLocalVariable", "UndefinedSymbol"] ->
				SyntaxExpr[d, "UndefinedSymbol"]
		]
		,
		SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] ->
			SyntaxExpr[b, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Limit[Limit[a b c d, c -> d], a -> b]"
]


Test[
	Limit[Limit[a b, a -> b], a -> b] // MakeBoxes // AnnotateSyntax
	,
	Limit[
		Limit[
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			SyntaxExpr[b, "UndefinedSymbol"]
			,
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] ->
				SyntaxExpr[b, "UndefinedSymbol"]
		]
		,
		SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] ->
			SyntaxExpr[b, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Limit[Limit[a b, a -> b], a -> b]"
]


(* ::Subsection:: *)
(*Mixed*)


Test[
	Table[Solve[a b c d, a], {b, c, d}] // MakeBoxes // AnnotateSyntax
	,
	Table[
		Solve[
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			SyntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"] *
			SyntaxExpr[c, "UndefinedSymbol"] *
			SyntaxExpr[d, "UndefinedSymbol"]
			,
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
		]
		,
		{
			SyntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"],
			SyntaxExpr[c, "UndefinedSymbol"],
			SyntaxExpr[d, "UndefinedSymbol"]
		}
	] // MakeBoxes
	,
	TestID -> "Table[Solve[a b c d, a], {b, c, d}]"
]


Test[
	Limit[Plot[a b c, {a, b, c}], a -> b] // MakeBoxes // AnnotateSyntax
	,
	Limit[
		Plot[
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			SyntaxExpr[b, "UndefinedSymbol"] *
			SyntaxExpr[c, "UndefinedSymbol"]
			,
		{
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
			SyntaxExpr[b, "UndefinedSymbol"],
			SyntaxExpr[c, "UndefinedSymbol"]
		}
		]
		,
		SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] ->
			SyntaxExpr[b, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Limit[Plot[a b c, {a, b, c}], a -> b]"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
