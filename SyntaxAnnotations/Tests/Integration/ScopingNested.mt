(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["SyntaxAnnotations`Tests`Integration`ScopingNested`", {"MUnit`"}]


Get["SyntaxAnnotations`Tests`Integration`init`"]


(* ::Section:: *)
(*Tests*)


Test[
	With[{a}, Block[{b}, a b]] // MakeBoxes // AnnotateSyntax
	,
	With[{SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]},
		Block[{SyntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"]},
			SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] *
			SyntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"]
		]
	] // MakeBoxes
	,
	TestID -> "With[{a}, Block[{b}, a b]"
]


Test[
	With[{a}, With[{a}, a]] // MakeBoxes // AnnotateSyntax
	,
	With[{SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]},
		With[
			{
				SyntaxExpr[a,
					"LocalScopeConflict", "LocalVariable", "UndefinedSymbol"
				]
			},
			SyntaxExpr[a,
				"LocalScopeConflict", "LocalVariable", "UndefinedSymbol"
			]
		]
	] // MakeBoxes
	,
	TestID -> "With[{a}, With[{a}, a]]"
]


Test[
	Module[{a}, Module[{a}, a]] // MakeBoxes // AnnotateSyntax
	,
	Module[{SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]},
		Module[
			{
				SyntaxExpr[a,
					"LocalScopeConflict", "LocalVariable", "UndefinedSymbol"
				]
			},
			SyntaxExpr[a,
				"LocalScopeConflict", "LocalVariable", "UndefinedSymbol"
			]
		]
	] // MakeBoxes
	,
	TestID -> "Module[{a}, Module[{a}, a]]"
]


Test[
	Block[{a}, Block[{a}, a]] // MakeBoxes // AnnotateSyntax
	,
	Block[{SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]},
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
			]
		]
	] // MakeBoxes
	,
	TestID -> "Block[{a}, Block[{a}, a]"
]


Test[
	Module[{a}, Block[{a}, a]] // MakeBoxes // AnnotateSyntax
	,
	Module[{SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]},
		Block[
			{
				SyntaxExpr[a,
					"LocalScopeConflict", "FunctionLocalVariable",
					"LocalVariable", "UndefinedSymbol"
				]
			},
			SyntaxExpr[a,
				"LocalScopeConflict", "FunctionLocalVariable", "LocalVariable",
				"UndefinedSymbol"
			]
		]
	] // MakeBoxes
	,
	TestID -> "Module[{a}, Block[{a}, a]"
]


Test[
	Block[{a}, With[{b=a}, a b]] // MakeBoxes // AnnotateSyntax
	,
	Block[{SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]},
		With[
			{
				SyntaxExpr[b, "LocalVariable", "UndefinedSymbol"] =
					SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
			},
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			SyntaxExpr[b, "LocalVariable", "UndefinedSymbol"]
		]
	] // MakeBoxes
	,
	TestID -> "Block[{a}, With[{b=a}, a b]]"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
