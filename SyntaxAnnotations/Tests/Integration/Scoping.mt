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
		syntaxExpr[a, "UndefinedSymbol"],
		syntaxExpr[a, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "With[a, a]"
]


Test[
	Module[{a}, a] // MakeBoxes // AnnotateSyntax
	,
	Module[{syntaxExpr[a, "LocalVariable", "UndefinedSymbol"]},
		syntaxExpr[a, "LocalVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Module[{a}, a]"
]


Test[
	Block[{a}, a_] // MakeBoxes // AnnotateSyntax
	,
	Block[{syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]},
		syntaxExpr[a_, "FunctionLocalVariable"]
	] // MakeBoxes
	,
	TestID -> "Block[{a}, a_]"
]


Test[
	With[{a}, a__] // MakeBoxes // AnnotateSyntax
	,
	With[{syntaxExpr[a, "LocalVariable", "UndefinedSymbol"]},
		syntaxExpr[a__, "LocalVariable"]
	] // MakeBoxes
	,
	TestID -> "With[{a}, a__]"
]


Test[
	Module[{a}, a___] // MakeBoxes // AnnotateSyntax
	,
	Module[{syntaxExpr[a, "LocalVariable", "UndefinedSymbol"]},
		syntaxExpr[a___, "LocalVariable"]
	] // MakeBoxes
	,
	TestID -> "Module[{a}, a___]"
]


Test[
	Block[{a}, _a] // MakeBoxes // AnnotateSyntax
	,
	Block[{syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]},
		syntaxExpr[_a, "FunctionLocalVariable"]
	] // MakeBoxes
	,
	TestID -> "Block[{a}, _a]"
]


Test[
	With[{a}, __a] // MakeBoxes // AnnotateSyntax
	,
	With[{syntaxExpr[a, "LocalVariable", "UndefinedSymbol"]},
		syntaxExpr[__a, "LocalVariable"]
	] // MakeBoxes
	,
	TestID -> "With[{a}, __a]"
]


Test[
	Module[{a}, ___a] // MakeBoxes // AnnotateSyntax
	,
	Module[{syntaxExpr[a, "LocalVariable", "UndefinedSymbol"]},
		syntaxExpr[___a, "LocalVariable"]
	] // MakeBoxes
	,
	TestID -> "Module[{a}, ___a]"
]


Test[
	Block[{a}, b] // MakeBoxes // AnnotateSyntax
	,
	Block[{syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]},
		syntaxExpr[b, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Block[{a}, b]"
]


Test[
	With[{a}, a_b] // MakeBoxes // AnnotateSyntax
	,
	With[{syntaxExpr[a, "LocalVariable", "UndefinedSymbol"]},
		syntaxExpr[a_b, "LocalVariable"]
	] // MakeBoxes
	,
	TestID -> "With[{a}, a_b]"
]


Test[
	Module[{b}, a_b] // MakeBoxes // AnnotateSyntax
	,
	Module[{syntaxExpr[b, "LocalVariable", "UndefinedSymbol"]},
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
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] =
				syntaxExpr[b, "UndefinedSymbol"]
		}
		,
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		syntaxExpr[b, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Block[{a = b}, a b]"
]


Test[
	With[{a = a}, a] // MakeBoxes // AnnotateSyntax
	,
	With[
		{
			syntaxExpr[a, "LocalVariable", "UndefinedSymbol"] =
				syntaxExpr[a, "UndefinedSymbol"]
		}
		,
		syntaxExpr[a, "LocalVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "With[{a = a}, a]"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
