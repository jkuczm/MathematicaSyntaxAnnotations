(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["SyntaxAnnotations`Tests`Integration`Scoping`", {"MUnit`"}]


Get["SyntaxAnnotations`Tests`Integration`init`"]


(* ::Section:: *)
(*Tests*)


Test[
	With[a, a] // MakeBoxes // AnnotateSyntax
	,
	With[
		SyntaxExpr[a, "UndefinedSymbol"],
		SyntaxExpr[a, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "With[a, a]"
]


Test[
	Module[{}, a] // MakeBoxes // AnnotateSyntax
	,
	Module[{}, SyntaxExpr[a, "UndefinedSymbol"]] // MakeBoxes
	,
	TestID -> "Module[{}, a]"
]

Test[
	Block[RawBoxes@RowBox[{"{", ",", "}"}], a] // MakeBoxes // AnnotateSyntax
	,
	Block[
		RawBoxes@RowBox[{"{", ",", "}"}],
		SyntaxExpr[a, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Block[{,}, a]"
]


Test[
	Module[{a}, a] // MakeBoxes // AnnotateSyntax
	,
	Module[{SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]},
		SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Module[{a}, a]"
]


Test[
	Block[{a}, a_] // MakeBoxes // AnnotateSyntax
	,
	Block[{SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]},
		SyntaxExpr[a_, "FunctionLocalVariable"]
	] // MakeBoxes
	,
	TestID -> "Block[{a}, a_]"
]


Test[
	With[{a}, a__] // MakeBoxes // AnnotateSyntax
	,
	With[{SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]},
		SyntaxExpr[a__, "LocalVariable"]
	] // MakeBoxes
	,
	TestID -> "With[{a}, a__]"
]


Test[
	Module[{a}, a___] // MakeBoxes // AnnotateSyntax
	,
	Module[{SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]},
		SyntaxExpr[a___, "LocalVariable"]
	] // MakeBoxes
	,
	TestID -> "Module[{a}, a___]"
]


Test[
	Block[{a}, _a] // MakeBoxes // AnnotateSyntax
	,
	Block[{SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]},
		SyntaxExpr[_a, "FunctionLocalVariable"]
	] // MakeBoxes
	,
	TestID -> "Block[{a}, _a]"
]


Test[
	With[{a}, __a] // MakeBoxes // AnnotateSyntax
	,
	With[{SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]},
		SyntaxExpr[__a, "LocalVariable"]
	] // MakeBoxes
	,
	TestID -> "With[{a}, __a]"
]


Test[
	Module[{a}, ___a] // MakeBoxes // AnnotateSyntax
	,
	Module[{SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]},
		SyntaxExpr[___a, "LocalVariable"]
	] // MakeBoxes
	,
	TestID -> "Module[{a}, ___a]"
]


Test[
	Block[{a}, b] // MakeBoxes // AnnotateSyntax
	,
	Block[{SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]},
		SyntaxExpr[b, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Block[{a}, b]"
]


Test[
	With[{a}, a_b] // MakeBoxes // AnnotateSyntax
	,
	With[{SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]},
		SyntaxExpr[a_b, "LocalVariable"]
	] // MakeBoxes
	,
	TestID -> "With[{a}, a_b]"
]


Test[
	Module[{b}, a_b] // MakeBoxes // AnnotateSyntax
	,
	Module[{SyntaxExpr[b, "LocalVariable", "UndefinedSymbol"]},
		a_b
	] // MakeBoxes
	,
	TestID -> "Module[{b}, a_b]"
]


Test[
	Block[{a = b}, a b] // MakeBoxes // AnnotateSyntax
	,
	Block[
		{
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] =
				SyntaxExpr[b, "UndefinedSymbol"]
		}
		,
		SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		SyntaxExpr[b, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Block[{a = b}, a b]"
]


Test[
	With[{a = a}, a] // MakeBoxes // AnnotateSyntax
	,
	With[
		{
			SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] =
				SyntaxExpr[a, "UndefinedSymbol"]
		}
		,
		SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "With[{a = a}, a]"
]


Test[
	Module[{a = x, b = y, c = z}, a b c x y z] // MakeBoxes // AnnotateSyntax
	,
	Module[
		{
			SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] =
				SyntaxExpr[x, "UndefinedSymbol"],
			SyntaxExpr[b, "LocalVariable", "UndefinedSymbol"] =
				SyntaxExpr[y, "UndefinedSymbol"],
			SyntaxExpr[c, "LocalVariable", "UndefinedSymbol"] =
				SyntaxExpr[z, "UndefinedSymbol"]
		}
		,
		SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] *
		SyntaxExpr[b, "LocalVariable", "UndefinedSymbol"] *
		SyntaxExpr[c, "LocalVariable", "UndefinedSymbol"] *
		SyntaxExpr[x, "UndefinedSymbol"] *
		SyntaxExpr[y, "UndefinedSymbol"] *
		SyntaxExpr[z, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Module[{a = x, b = y, c = z}, a b c x y z]"
]


Test[
	Block[{a = b, b = a}, a b] // MakeBoxes // AnnotateSyntax
	,
	Block[
		{
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] =
				SyntaxExpr[b, "UndefinedSymbol"],
			SyntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"] =
				SyntaxExpr[a, "UndefinedSymbol"]
		}
		,
		SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		SyntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Block[{a = b, b = a}, a b]"
]


Test[
	With[{f[a = a, b = b]}, f a b] // MakeBoxes // AnnotateSyntax
	,
	With[
		{SyntaxExpr[f, "LocalVariable", "UndefinedSymbol"][
			SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] =
				SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]
			,
			SyntaxExpr[b, "LocalVariable", "UndefinedSymbol"] =
				SyntaxExpr[b, "LocalVariable", "UndefinedSymbol"]
		]}
		,
		SyntaxExpr[f, "LocalVariable", "UndefinedSymbol"] *
		SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] *
		SyntaxExpr[b, "LocalVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Module[{f[a = a, b = b]}, f a b]"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
