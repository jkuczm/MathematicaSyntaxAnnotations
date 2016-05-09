(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["SyntaxAnnotations`Tests`Integration`ScopingNested`", {"MUnit`"}]


Get["SyntaxAnnotations`Tests`Integration`init`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Inner scoping in body*)


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
					"LocalScopeConflict", "LocalVariable",
					"FunctionLocalVariable", "UndefinedSymbol"
				]
			},
			SyntaxExpr[a,
				"LocalScopeConflict", "LocalVariable", "FunctionLocalVariable",
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


(* ::Subsubsection:: *)
(*Non-assignment*)


Test[
	With[{Module[{a}, a]}, a Module] // MakeBoxes // AnnotateSyntax
	,
	With[
		{
			SyntaxExpr[Module, "LocalVariable"][
				{SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]},
				SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]
			]
		},
		SyntaxExpr[a, "UndefinedSymbol"] *
		SyntaxExpr[Module, "LocalVariable"]
	] // MakeBoxes
	,
	TestID -> "With[{Module[{a}, a]}, a Module]"
]
Test[
	Module[{With[{a}, b]}, a b With] // MakeBoxes // AnnotateSyntax
	,
	Module[
		{
			SyntaxExpr[With, "LocalVariable"][
				{SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]},
				SyntaxExpr[b, "UndefinedSymbol"]
			]
		},
		SyntaxExpr[a, "UndefinedSymbol"] *
		SyntaxExpr[b, "UndefinedSymbol"] *
		SyntaxExpr[With, "LocalVariable"]
	] // MakeBoxes
	,
	TestID -> "Module[{With[{a}, b]}, a b With]"
]


Test[
	Block[{Module[{a = a}, a]}, a Module] // MakeBoxes // AnnotateSyntax
	,
	Block[
		{
			SyntaxExpr[Module, "FunctionLocalVariable"][
				{
					SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] =
						SyntaxExpr[a, "UndefinedSymbol"]
				},
				SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]
			]
		},
		SyntaxExpr[a, "UndefinedSymbol"] *
		SyntaxExpr[Module, "FunctionLocalVariable"]
	] // MakeBoxes
	,
	TestID -> "Block[{Module[{a = a}, a]}, a Module]"
]
Test[
	With[{With[{a := b}, c]}, a b c With] // MakeBoxes // AnnotateSyntax
	,
	With[
		{
			SyntaxExpr[With, "LocalVariable"][
				{
					SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] :=
						SyntaxExpr[b, "UndefinedSymbol"]
				},
				SyntaxExpr[c, "UndefinedSymbol"]
			]
		},
		SyntaxExpr[a, "UndefinedSymbol"] *
		SyntaxExpr[b, "UndefinedSymbol"] *
		SyntaxExpr[c, "UndefinedSymbol"] *
		SyntaxExpr[With, "LocalVariable"]
	] // MakeBoxes
	,
	TestID -> "With[{With[{a := b}, c]}, a b c With]"
]


(* ::Subsubsection:: *)
(*Assignment LHS*)


Test[
	With[{Module[{a}, a] := a}, a Module] // MakeBoxes // AnnotateSyntax
	,
	With[
		{
			SyntaxExpr[Module, "LocalVariable"][
				{SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]},
				SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]
			] :=
				SyntaxExpr[a, "UndefinedSymbol"]
		},
		SyntaxExpr[a, "UndefinedSymbol"] *
		SyntaxExpr[Module, "LocalVariable"]
	] // MakeBoxes
	,
	TestID -> "With[{Module[{a}, a] := a}, a Module]"
]
Test[
	With[{Block[{a}, b] = c}, a b c Block] // MakeBoxes // AnnotateSyntax
	,
	With[
		{
			SyntaxExpr[Block, "LocalVariable"][
				{SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]},
				SyntaxExpr[b, "UndefinedSymbol"]
			] =
				SyntaxExpr[c, "UndefinedSymbol"]
		},
		SyntaxExpr[a, "UndefinedSymbol"] *
		SyntaxExpr[b, "UndefinedSymbol"] *
		SyntaxExpr[c, "UndefinedSymbol"] *
		SyntaxExpr[Block, "LocalVariable"]
	] // MakeBoxes
	,
	TestID -> "With[{Block[{a}, b] = c}, a b c Block]"
]


Test[
	Module[{With[{a := a}, a] = a}, a With] // MakeBoxes // AnnotateSyntax
	,
	Module[
		{
			SyntaxExpr[With, "LocalVariable"][
				{
					SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] :=
						SyntaxExpr[a, "UndefinedSymbol"]
				},
				SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]
			] =
				SyntaxExpr[a, "UndefinedSymbol"]
		},
		SyntaxExpr[a, "UndefinedSymbol"] *
		SyntaxExpr[With, "LocalVariable"]
	] // MakeBoxes
	,
	TestID -> "Module[{With[{a := a}, a] = a}, a With]"
]
Test[
	Module[{Module[{a = b}, c] := d}, a b c d Module] //
		MakeBoxes // AnnotateSyntax
	,
	Module[
		{
			SyntaxExpr[Module, "LocalVariable"][
				{
					SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] =
						SyntaxExpr[b, "UndefinedSymbol"]
				},
				SyntaxExpr[c, "UndefinedSymbol"]
			] :=
				SyntaxExpr[d, "UndefinedSymbol"]
		},
		SyntaxExpr[a, "UndefinedSymbol"] *
		SyntaxExpr[b, "UndefinedSymbol"] *
		SyntaxExpr[c, "UndefinedSymbol"] *
		SyntaxExpr[d, "UndefinedSymbol"] *
		SyntaxExpr[Module, "LocalVariable"]
	] // MakeBoxes
	,
	TestID -> "Module[{Module[{a = b}, c] := d}, a b c d Module]"
]


(* ::Subsubsection:: *)
(*Assignment RHS*)


Test[
	Module[{a = Block[{a}, a]}, a Block] // MakeBoxes // AnnotateSyntax
	,
	Module[
		{
			SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] =
				Block[
					{
						SyntaxExpr[a,
							"LocalScopeConflict", "LocalVariable",
							"FunctionLocalVariable", "UndefinedSymbol"
						]
					},
					SyntaxExpr[a,
						"LocalScopeConflict", "LocalVariable",
						"FunctionLocalVariable", "UndefinedSymbol"
					]
				]
		},
		SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] *
		Block
	] // MakeBoxes
	,
	TestID -> "Module[{a = Block[{a}, a]}, a Block]"
]
Test[
	Block[{a := With[{b}, c]}, a b c With] // MakeBoxes // AnnotateSyntax
	,
	Block[
		{
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] :=
				With[{SyntaxExpr[b, "LocalVariable", "UndefinedSymbol"]},
					SyntaxExpr[c, "UndefinedSymbol"]
				]
		},
		SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		SyntaxExpr[b, "UndefinedSymbol"] *
		SyntaxExpr[c, "UndefinedSymbol"] *
		With
	] // MakeBoxes
	,
	TestID -> "Block[{a := With[{b}, c]}, a b c With]"
]


Test[
	Block[{a := Module[{a := a}, a]}, a Module] // MakeBoxes // AnnotateSyntax
	,
	Block[
		{
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] :=
				Module[
					{
						SyntaxExpr[a,
							"LocalScopeConflict", "LocalVariable",
							"UndefinedSymbol"
						] :=
							SyntaxExpr[a,
								"LocalScopeConflict", "UndefinedSymbol"
							]
					},
					SyntaxExpr[a,
						"LocalScopeConflict", "LocalVariable",
						"UndefinedSymbol"
					]
				]
		},
		SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		Module
	] // MakeBoxes
	,
	TestID -> "Block[{a := Module[{a := a}, a]}, a Module]"
]
Test[
	Block[{a = Block[{b = c}, d]}, a b c d Block] //
		MakeBoxes // AnnotateSyntax
	,
	Block[
		{
			SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] =
				Block[
					{
						SyntaxExpr[b,
							"FunctionLocalVariable", "UndefinedSymbol"
						] =
							SyntaxExpr[c, "UndefinedSymbol"]
					},
					SyntaxExpr[d, "UndefinedSymbol"]
				]
		},
		SyntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		SyntaxExpr[b, "UndefinedSymbol"] *
		SyntaxExpr[c, "UndefinedSymbol"] *
		SyntaxExpr[d, "UndefinedSymbol"] *
		Block
	] // MakeBoxes
	,
	TestID -> "Block[{a = Block[{b = c}, d]}, a b c d Block]"
]


Test[
	With[{a = a Block[{b := a}, a b]}, a b Block] //
		MakeBoxes // AnnotateSyntax
	,
	With[
		{
			SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] =
				SyntaxExpr[a, "UndefinedSymbol"] *
				Block[
					{
						SyntaxExpr[b,
							"FunctionLocalVariable", "UndefinedSymbol"
						] :=
							SyntaxExpr[a, "UndefinedSymbol"]
					},
					SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] *
					SyntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"]
				]
		},
		SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] *
		SyntaxExpr[b, "UndefinedSymbol"] *
		Block
	] // MakeBoxes
	,
	TestID -> "With[{a = a Block[{b := a}, a b]}, a b Block]"
]


Test[
	With[{a := With[a, a]}, a With] // MakeBoxes // AnnotateSyntax
	,
	With[
		{
			SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] :=
				With[
					SyntaxExpr[a, "UndefinedSymbol"],
					SyntaxExpr[a, "UndefinedSymbol"]
				]
		},
		SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] *
		With
	] // MakeBoxes
	,
	TestID -> "With[{a := With[a, a]}, With]"
]
Test[
	With[{a = Module[{}, a]}, a Module] // MakeBoxes // AnnotateSyntax
	,
	With[
		{
			SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] =
				Module[{},
					SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]
				]
		},
		SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] *
		Module
	] // MakeBoxes
	,
	TestID -> "With[{a = Module[{}, a]}, With]"
]


(* ::Subsubsection:: *)
(*Multiple vars*)


Test[
	Module[{With, a, b := With[{b := a}, a]}, a b With] //
		MakeBoxes // AnnotateSyntax
	,
	Module[
		{
			SyntaxExpr[With, "LocalVariable"],
			SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"],
			SyntaxExpr[b, "LocalVariable", "UndefinedSymbol"] :=
				With[
					{
						SyntaxExpr[b,
							"LocalScopeConflict", "LocalVariable",
							"UndefinedSymbol"
						] :=
							SyntaxExpr[a, "UndefinedSymbol"]
					},
					SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"]
				]
		},
		SyntaxExpr[a, "LocalVariable", "UndefinedSymbol"] *
		SyntaxExpr[b, "LocalVariable", "UndefinedSymbol"] *
		SyntaxExpr[With, "LocalVariable"]
	] // MakeBoxes
	,
	TestID -> "Module[{With, a, b := With[{b := a}, a]}, a b With]"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
