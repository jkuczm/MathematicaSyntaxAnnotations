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
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			syntaxExpr[b, "UndefinedSymbol"] *
			syntaxExpr[c, "UndefinedSymbol"] *
			syntaxExpr[d, "FunctionLocalVariable", "UndefinedSymbol"] *
			syntaxExpr[e, "UndefinedSymbol"] *
			syntaxExpr[f, "UndefinedSymbol"]
			,
			{
				syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
				syntaxExpr[b, "UndefinedSymbol"],
				syntaxExpr[c, "UndefinedSymbol"]
			}
		]
		,
		{
			syntaxExpr[d, "FunctionLocalVariable", "UndefinedSymbol"],
			syntaxExpr[e, "UndefinedSymbol"],
			syntaxExpr[f, "UndefinedSymbol"]
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
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			syntaxExpr[b, "UndefinedSymbol"] *
			syntaxExpr[c, "UndefinedSymbol"]
			,
			{
				syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
				syntaxExpr[b, "UndefinedSymbol"],
				syntaxExpr[c, "UndefinedSymbol"]
			}
		]
		,
		{
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
			syntaxExpr[b, "UndefinedSymbol"],
			syntaxExpr[c, "UndefinedSymbol"]
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
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			syntaxExpr[b, "UndefinedSymbol"] *
			syntaxExpr[c, "UndefinedSymbol"] *
			syntaxExpr[d, "FunctionLocalVariable", "UndefinedSymbol"] *
			syntaxExpr[e, "UndefinedSymbol"] *
			syntaxExpr[f, "UndefinedSymbol"]
			,
			{
				syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
				syntaxExpr[b, "UndefinedSymbol"],
				syntaxExpr[c, "UndefinedSymbol"]
			}
		]
		,
		{
			syntaxExpr[d, "FunctionLocalVariable", "UndefinedSymbol"],
			syntaxExpr[e, "UndefinedSymbol"],
			syntaxExpr[f, "UndefinedSymbol"]
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
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			syntaxExpr[b, "UndefinedSymbol"] *
			syntaxExpr[c, "UndefinedSymbol"]
			,
			{
				syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
				syntaxExpr[b, "UndefinedSymbol"],
				syntaxExpr[c, "UndefinedSymbol"]
			}
		]
		,
		{
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
			syntaxExpr[b, "UndefinedSymbol"],
			syntaxExpr[c, "UndefinedSymbol"]
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
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			syntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"]
			,
			syntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"]
		]
		,
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Solve[Solve[a b, b], a]"
]


Test[
	Solve[Solve[a, a], a] // MakeBoxes // AnnotateSyntax
	,
	Solve[
		Solve[
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
		]
		,
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
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
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			syntaxExpr[b, "UndefinedSymbol"] *
			syntaxExpr[c, "FunctionLocalVariable", "UndefinedSymbol"] *
			syntaxExpr[d, "UndefinedSymbol"]
			,
			syntaxExpr[c, "FunctionLocalVariable", "UndefinedSymbol"] ->
				syntaxExpr[d, "UndefinedSymbol"]
		]
		,
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] ->
			syntaxExpr[b, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Limit[Limit[a b c d, c -> d], a -> b]"
]


Test[
	Limit[Limit[a b, a -> b], a -> b] // MakeBoxes // AnnotateSyntax
	,
	Limit[
		Limit[
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			syntaxExpr[b, "UndefinedSymbol"]
			,
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] ->
				syntaxExpr[b, "UndefinedSymbol"]
		]
		,
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] ->
			syntaxExpr[b, "UndefinedSymbol"]
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
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			syntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"] *
			syntaxExpr[c, "UndefinedSymbol"] *
			syntaxExpr[d, "UndefinedSymbol"]
			,
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
		]
		,
		{
			syntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"],
			syntaxExpr[c, "UndefinedSymbol"],
			syntaxExpr[d, "UndefinedSymbol"]
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
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			syntaxExpr[b, "UndefinedSymbol"] *
			syntaxExpr[c, "UndefinedSymbol"]
			,
		{
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
			syntaxExpr[b, "UndefinedSymbol"],
			syntaxExpr[c, "UndefinedSymbol"]
		}
		]
		,
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] ->
			syntaxExpr[b, "UndefinedSymbol"]
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
