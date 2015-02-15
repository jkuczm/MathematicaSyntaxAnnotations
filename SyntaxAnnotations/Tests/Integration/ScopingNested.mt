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
	With[{syntaxExpr[a, "LocalVariable", "UndefinedSymbol"]},
		Block[{syntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"]},
			syntaxExpr[a, "LocalVariable", "UndefinedSymbol"] *
			syntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"]
		]
	] // MakeBoxes
	,
	TestID -> "With[{a}, Block[{b}, a b]"
]


Test[
	With[{a}, With[{a}, a]] // MakeBoxes // AnnotateSyntax
	,
	With[{syntaxExpr[a, "LocalVariable", "UndefinedSymbol"]},
		With[
			{
				syntaxExpr[a,
					"LocalScopeConflict", "LocalVariable", "UndefinedSymbol"
				]
			},
			syntaxExpr[a,
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
	Module[{syntaxExpr[a, "LocalVariable", "UndefinedSymbol"]},
		Module[
			{
				syntaxExpr[a,
					"LocalScopeConflict", "LocalVariable", "UndefinedSymbol"
				]
			},
			syntaxExpr[a,
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
	Block[{syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]},
		Block[
			{
				syntaxExpr[a,
					"LocalScopeConflict", "FunctionLocalVariable",
					"UndefinedSymbol"
				]
			},
			syntaxExpr[a,
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
	Module[{syntaxExpr[a, "LocalVariable", "UndefinedSymbol"]},
		Block[
			{
				syntaxExpr[a,
					"LocalScopeConflict", "FunctionLocalVariable",
					"LocalVariable", "UndefinedSymbol"
				]
			},
			syntaxExpr[a,
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
	Block[{syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]},
		With[
			{
				syntaxExpr[b, "LocalVariable", "UndefinedSymbol"] =
					syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
			},
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			syntaxExpr[b, "LocalVariable", "UndefinedSymbol"]
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
