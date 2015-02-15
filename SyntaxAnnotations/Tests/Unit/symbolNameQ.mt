(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["SyntaxAnnotations`Tests`Unit`symbolNameQ`", {"MUnit`"}]


Get["SyntaxAnnotations`"]

PrependTo[$ContextPath, "SyntaxAnnotations`Private`"]


(* ::Section:: *)
(*Tests*)


Test[
	symbolNameQ[""]
	,
	False
	,
	TestID -> "empty"
]

Test[
	symbolNameQ["1"]
	,
	False
	,
	TestID -> "integer"
]

Test[
	symbolNameQ["2a"]
	,
	False
	,
	TestID -> "start with integer"
]

Test[
	symbolNameQ["b"]
	,
	True
	,
	TestID -> "single letter"
]

Test[
	symbolNameQ["c3"]
	,
	True
	,
	TestID -> "end with integer"
]

Test[
	symbolNameQ["_d"]
	,
	False
	,
	TestID -> "Blank with head"
]

Test[
	symbolNameQ["__e"]
	,
	False
	,
	TestID -> "BlankSequence with head"
]

Test[
	symbolNameQ["___f"]
	,
	False
	,
	TestID -> "BlankNullSequence with head"
]

Test[
	symbolNameQ["g_"]
	,
	False
	,
	TestID -> "named Blank"
]

Test[
	symbolNameQ["h__"]
	,
	False
	,
	TestID -> "named BlankSequence"
]

Test[
	symbolNameQ["i___"]
	,
	False
	,
	TestID -> "named BlankNullSequence"
]

Test[
	symbolNameQ["j_k"]
	,
	False
	,
	TestID -> "named Blank with head"
]

Test[
	symbolNameQ["l__m"]
	,
	False
	,
	TestID -> "named BlankSequence with head"
]

Test[
	symbolNameQ["n___o"]
	,
	False
	,
	TestID -> "named BlankNullSequence with head"
]

Test[
	symbolNameQ["aBc"]
	,
	True
	,
	TestID -> "multiple letters"
]

Test[
	symbolNameQ["a2c"]
	,
	True
	,
	TestID -> "integer inside"
]

Test[
	symbolNameQ["a_b__c"]
	,
	False
	,
	TestID -> "two underscore sequences"
]

Test[
	symbolNameQ["Block"]
	,
	True
	,
	TestID -> "built-in symbol name"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
