(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["SyntaxAnnotations`Tests`Unit`whitespaceQ`", {"MUnit`"}]


Get["SyntaxAnnotations`"]

PrependTo[$ContextPath, "SyntaxAnnotations`Private`"]


(* ::Section:: *)
(*Tests*)


Test[
	whitespaceQ[""]
	,
	True
	,
	TestID -> "empty"
]
Test[
	whitespaceQ[" "]
	,
	True
	,
	TestID -> "space"
]
Test[
	whitespaceQ["\n"]
	,
	True
	,
	TestID -> "\\n"
]
Test[
	whitespaceQ["\t"]
	,
	True
	,
	TestID -> "\\t"
]
Test[
	whitespaceQ[" \t \n  "]
	,
	True
	,
	TestID -> "whitespace combination"
]


Test[
	whitespaceQ["1"]
	,
	False
	,
	TestID -> "integer"
]
Test[
	whitespaceQ["b"]
	,
	False
	,
	TestID -> "single letter"
]
Test[
	whitespaceQ["$"]
	,
	False
	,
	TestID -> "dollar sign"
]
Test[
	whitespaceQ["\[UnderBracket]"]
	,
	False
	,
	TestID -> "\\[UnderBracket]"
]
Test[
	whitespaceQ["Null"]
	,
	False
	,
	TestID -> "Null"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
