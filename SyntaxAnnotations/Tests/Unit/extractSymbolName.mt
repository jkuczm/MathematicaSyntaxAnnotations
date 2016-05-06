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
	{}
	,
	TestID -> "empty"
]

Test[
	extractSymbolName["1"]
	,
	{}
	,
	TestID -> "integer"
]

Test[
	extractSymbolName["2a"]
	,
	{}
	,
	TestID -> "start with integer"
]

Test[
	extractSymbolName["b"]
	,
	{"b"}
	,
	TestID -> "single letter"
]

Test[
	extractSymbolName["c3"]
	,
	{"c3"}
	,
	TestID -> "end with integer"
]

Test[
	extractSymbolName["_d"]
	,
	{"d"}
	,
	TestID -> "Blank with head"
]

Test[
	extractSymbolName["__e"]
	,
	{"e"}
	,
	TestID -> "BlankSequence with head"
]

Test[
	extractSymbolName["___f"]
	,
	{"f"}
	,
	TestID -> "BlankNullSequence with head"
]

Test[
	extractSymbolName["g_"]
	,
	{"g"}
	,
	TestID -> "named Blank"
]

Test[
	extractSymbolName["h__"]
	,
	{"h"}
	,
	TestID -> "named BlankSequence"
]

Test[
	extractSymbolName["i___"]
	,
	{"i"}
	,
	TestID -> "named BlankNullSequence"
]

Test[
	extractSymbolName["j_k"]
	,
	{"j"}
	,
	TestID -> "named Blank with head"
]

Test[
	extractSymbolName["l__m"]
	,
	{"l"}
	,
	TestID -> "named BlankSequence with head"
]

Test[
	extractSymbolName["n___o"]
	,
	{"n"}
	,
	TestID -> "named BlankNullSequence with head"
]

Test[
	extractSymbolName["aBc"]
	,
	{"aBc"}
	,
	TestID -> "multiple letters"
]

Test[
	extractSymbolName["a2c"]
	,
	{"a2c"}
	,
	TestID -> "integer inside"
]

Test[
	extractSymbolName["a_b__c"]
	,
	{"a"}
	,
	TestID -> "two underscore sequences"
]

Test[
	extractSymbolName["Block"]
	,
	{"Block"}
	,
	TestID -> "built-in symbol name"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
