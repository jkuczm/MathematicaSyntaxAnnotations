(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["SyntaxAnnotations`Tests`Unit`extractSymbolName`", {"MUnit`"}]


Get["SyntaxAnnotations`"]

PrependTo[$ContextPath, "SyntaxAnnotations`Private`"]


(* ::Section:: *)
(*Tests*)


Test[
	extractSymbolName[""]
	,
	Alternatives[]
	,
	TestID -> "empty"
]

Test[
	extractSymbolName["1"]
	,
	Alternatives[]
	,
	TestID -> "integer"
]

Test[
	extractSymbolName["2a"]
	,
	Alternatives[]
	,
	TestID -> "start with integer"
]

Test[
	extractSymbolName["b"]
	,
	Alternatives["b"]
	,
	TestID -> "single letter"
]

Test[
	extractSymbolName["c3"]
	,
	Alternatives["c3"]
	,
	TestID -> "end with integer"
]

Test[
	extractSymbolName["_d"]
	,
	Alternatives["d"]
	,
	TestID -> "Blank with head"
]

Test[
	extractSymbolName["__e"]
	,
	Alternatives["e"]
	,
	TestID -> "BlankSequence with head"
]

Test[
	extractSymbolName["___f"]
	,
	Alternatives["f"]
	,
	TestID -> "BlankNullSequence with head"
]

Test[
	extractSymbolName["g_"]
	,
	Alternatives["g"]
	,
	TestID -> "named Blank"
]

Test[
	extractSymbolName["h__"]
	,
	Alternatives["h"]
	,
	TestID -> "named BlankSequence"
]

Test[
	extractSymbolName["i___"]
	,
	Alternatives["i"]
	,
	TestID -> "named BlankNullSequence"
]

Test[
	extractSymbolName["j_k"]
	,
	Alternatives["j"]
	,
	TestID -> "named Blank with head"
]

Test[
	extractSymbolName["l__m"]
	,
	Alternatives["l"]
	,
	TestID -> "named BlankSequence with head"
]

Test[
	extractSymbolName["n___o"]
	,
	Alternatives["n"]
	,
	TestID -> "named BlankNullSequence with head"
]

Test[
	extractSymbolName["aBc"]
	,
	Alternatives["aBc"]
	,
	TestID -> "multiple letters"
]

Test[
	extractSymbolName["a2c"]
	,
	Alternatives["a2c"]
	,
	TestID -> "integer inside"
]

Test[
	extractSymbolName["a_b__c"]
	,
	Alternatives["a"]
	,
	TestID -> "two underscore sequences"
]

Test[
	extractSymbolName["Block"]
	,
	Alternatives["Block"]
	,
	TestID -> "built-in symbol name"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
