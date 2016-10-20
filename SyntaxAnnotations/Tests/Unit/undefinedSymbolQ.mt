(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["SyntaxAnnotations`Tests`Unit`undefinedSymbolQ`", {"MUnit`"}]


Get["SyntaxAnnotations`"]

PrependTo[$ContextPath, "SyntaxAnnotations`Private`"]


(* ::Section:: *)
(*Tests*)


Block[{a},
	a = 1;
	
	Test[
		undefinedSymbolQ[a]
		,
		False
		,
		TestID -> "own value: Symbol"
	];
	Test[
		undefinedSymbolQ["a"]
		,
		False
		,
		TestID -> "own value: String"
	];
	
	SetAttributes[a, ReadProtected];
	Test[
		undefinedSymbolQ[a]
		,
		False
		,
		TestID -> "own value: ReadProtected: Symbol"
	];
	Test[
		undefinedSymbolQ["a"]
		,
		False
		,
		TestID -> "own value: ReadProtected: String"
	]
]

Block[{b},
	b[_][_] = 2;
	
	Test[
		undefinedSymbolQ[b]
		,
		False
		,
		TestID -> "sub value: Symbol"
	];
	Test[
		undefinedSymbolQ["b"]
		,
		False
		,
		TestID -> "sub value: String"
	];
	
	SetAttributes[b, ReadProtected];
	Test[
		undefinedSymbolQ[b]
		,
		False
		,
		TestID -> "sub value: ReadProtected: Symbol"
	];
	Test[
		undefinedSymbolQ["b"]
		,
		False
		,
		TestID -> "sub value: ReadProtected: String"
	]
]

Block[{c, d},
	d[c] ^= 3;
	
	Test[
		undefinedSymbolQ[c]
		,
		False
		,
		TestID -> "up value: Symbol"
	];
	Test[
		undefinedSymbolQ["c"]
		,
		False
		,
		TestID -> "up value: String"
	];
	
	SetAttributes[c, ReadProtected];
	Test[
		undefinedSymbolQ[c]
		,
		False
		,
		TestID -> "up value: ReadProtected: Symbol"
	];
	Test[
		undefinedSymbolQ["c"]
		,
		False
		,
		TestID -> "up value: ReadProtected: String"
	]
]

Block[{e},
	e[_] = 4;
	
	Test[
		undefinedSymbolQ[e]
		,
		False
		,
		TestID -> "down value: Symbol"
	];
	Test[
		undefinedSymbolQ["e"]
		,
		False
		,
		TestID -> "down value: String"
	];
	
	SetAttributes[e, ReadProtected];
	Test[
		undefinedSymbolQ[e]
		,
		False
		,
		TestID -> "down value: ReadProtected: Symbol"
	];
	Test[
		undefinedSymbolQ["e"]
		,
		False
		,
		TestID -> "down value: ReadProtected: String"
	]
]

Block[{f},
	N[f] = 5;
	
	Test[
		undefinedSymbolQ[f]
		,
		False
		,
		TestID -> "n value: Symbol"
	];
	Test[
		undefinedSymbolQ["f"]
		,
		False
		,
		TestID -> "n value: String"
	];
	
	SetAttributes[f, ReadProtected];
	Test[
		undefinedSymbolQ[f]
		,
		False
		,
		TestID -> "n value: ReadProtected: Symbol"
	];
	Test[
		undefinedSymbolQ["f"]
		,
		False
		,
		TestID -> "n value: ReadProtected: String"
	]
]

Block[{g},
	Format[g] = 6;
	
	Test[
		undefinedSymbolQ[g]
		,
		False
		,
		TestID -> "format value: Symbol"
	];
	Test[
		undefinedSymbolQ["g"]
		,
		False
		,
		TestID -> "format value: String"
	];
	
	SetAttributes[g, ReadProtected];
	Test[
		undefinedSymbolQ[g]
		,
		False
		,
		TestID -> "format value: ReadProtected: Symbol"
	];
	Test[
		undefinedSymbolQ["g"]
		,
		False
		,
		TestID -> "format value: ReadProtected: String"
	]
]

Block[{h},
	Default[h] = 7;
	
	Test[
		undefinedSymbolQ[h]
		,
		False
		,
		TestID -> "default value: Symbol"
	];
	Test[
		undefinedSymbolQ["h"]
		,
		False
		,
		TestID -> "default value: String"
	];
	
	SetAttributes[h, ReadProtected];
	Test[
		undefinedSymbolQ[h]
		,
		False
		,
		TestID -> "default value: ReadProtected: Symbol"
	];
	Test[
		undefinedSymbolQ["h"]
		,
		False
		,
		TestID -> "default value: ReadProtected: String"
	]
]

Block[{i},
	i::test = "i test";
	
	Test[
		undefinedSymbolQ[i]
		,
		False
		,
		TestID -> "messages: Symbol"
	];
	Test[
		undefinedSymbolQ["i"]
		,
		False
		,
		TestID -> "messages: String"
	];
	
	SetAttributes[i, ReadProtected];
	Test[
		undefinedSymbolQ[i]
		,
		False
		,
		TestID -> "messages: ReadProtected: Symbol"
	];
	Test[
		undefinedSymbolQ["i"]
		,
		False
		,
		TestID -> "messages: ReadProtected: String"
	]
]
(* Messages leak out from Block so clear them. *)
ClearAll[i]

Block[{j},
	SetAttributes[j, HoldAll];
	
	Test[
		undefinedSymbolQ[j]
		,
		True
		,
		TestID -> "attributes: Symbol"
	];
	Test[
		undefinedSymbolQ["j"]
		,
		True
		,
		TestID -> "attributes: String"
	];
	
	SetAttributes[j, ReadProtected];
	Test[
		undefinedSymbolQ[j]
		,
		True
		,
		TestID -> "attributes: ReadProtected: Symbol"
	];
	Test[
		undefinedSymbolQ["j"]
		,
		True
		,
		TestID -> "attributes: ReadProtected: String"
	]
]

Block[{k},
	Test[
		undefinedSymbolQ[k]
		,
		True
		,
		TestID -> "nothing: Symbol"
	];
	Test[
		undefinedSymbolQ["k"]
		,
		True
		,
		TestID -> "nothing: String"
	];
	
	SetAttributes[k, ReadProtected];
	Test[
		undefinedSymbolQ[k]
		,
		True
		,
		TestID -> "nothing: ReadProtected: Symbol"
	];
	Test[
		undefinedSymbolQ["k"]
		,
		True
		,
		TestID -> "nothing: ReadProtected: String"
	]
]

Block[{l},
	Test[
		undefinedSymbolQ[l_]
		,
		False
		,
		TestID -> "named pattern: Symbol"
	];
	Test[
		undefinedSymbolQ["l_"]
		,
		False
		,
		TestID -> "named pattern: String"
	]
]

Block[{m},
	Test[
		undefinedSymbolQ[_m]
		,
		False
		,
		TestID -> "pattern with head: Symbol"
	];
	Test[
		undefinedSymbolQ["_m"]
		,
		False
		,
		TestID -> "pattern with head: String"
	]
]

Block[{n},
	Test[
		undefinedSymbolQ[2 n]
		,
		False
		,
		TestID -> "non-symbol: Symbol"
	];
	Test[
		undefinedSymbolQ["2 n"]
		,
		False
		,
		TestID -> "non-symbol: String"
	]
]

Test[
	undefinedSymbolQ[1]
	,
	False
	,
	TestID -> "Integer: Symbol"
]
Test[
	undefinedSymbolQ["2"]
	,
	False
	,
	TestID -> "Integer: String"
]

Test[
	undefinedSymbolQ[Block]
	,
	False
	,
	TestID -> "built-in: Symbol"
]
Test[
	undefinedSymbolQ["Block"]
	,
	False
	,
	TestID -> "built-in: String"
]

Test[
	undefinedSymbolQ[Null]
	,
	False
	,
	TestID -> "built-in Null: Symbol"
]
Test[
	undefinedSymbolQ["Null"]
	,
	False
	,
	TestID -> "built-in Null: String"
]

Test[
	undefinedSymbolQ[\[Infinity]]
	,
	False
	,
	TestID -> "parsed to built-in: Symbol"
]
Test[
	undefinedSymbolQ["\[Infinity]"]
	,
	False
	,
	TestID -> "parsed to built-in: String"
]


Block[{a, b},
	a := (b = "leak");
	
	Test[
		undefinedSymbolQ[a]
		,
		False
		,
		TestID -> "evaluation leak: Symbol: returned value"
	];
	Test[
		b // Hold[#]&
		,
		b // Hold
		,
		TestID -> "evaluation leak: Symbol: test variable"
	]
]
Block[{a, b},
	a := (b = "leak");
	
	Test[
		undefinedSymbolQ["a"]
		,
		False
		,
		TestID -> "evaluation leak: String: returned value"
	];
	Test[
		b // Hold[#]&
		,
		b // Hold
		,
		TestID -> "evaluation leak: String: test variable"
	]
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
