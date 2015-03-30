(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["SyntaxAnnotations`Tests`Unit`whitespaceQ`", {"MUnit`"}]


Get["SyntaxAnnotations`"]

PrependTo[$ContextPath, "SyntaxAnnotations`Private`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*No arguments*)


Test[
	whitespaceQ[]
	,
	True
	,
	TestID -> "No arguments"
]


(* ::Subsection:: *)
(*One argument*)


Test[
	whitespaceQ[""]
	,
	True
	,
	TestID -> "One argument: empty"
]
Test[
	whitespaceQ[" "]
	,
	True
	,
	TestID -> "One argument: space"
]
Test[
	whitespaceQ["\n"]
	,
	True
	,
	TestID -> "One argument: \\n"
]
Test[
	whitespaceQ["\t"]
	,
	True
	,
	TestID -> "One argument: \\t"
]
Test[
	whitespaceQ["\[IndentingNewLine]"]
	,
	True
	,
	TestID -> "One argument: \\[IndentingNewLine]"
]
Test[
	whitespaceQ[" \t \n  \[IndentingNewLine] "]
	,
	True
	,
	TestID -> "One argument: whitespace combination"
]


Test[
	whitespaceQ["1"]
	,
	False
	,
	TestID -> "One argument: Integer"
]
Test[
	whitespaceQ["b"]
	,
	False
	,
	TestID -> "One argument: single letter"
]
Test[
	whitespaceQ["$"]
	,
	False
	,
	TestID -> "One argument: dollar sign"
]
Test[
	whitespaceQ["\[UnderBracket]"]
	,
	False
	,
	TestID -> "One argument: \\[UnderBracket]"
]
Test[
	whitespaceQ["Null"]
	,
	False
	,
	TestID -> "One argument: Null"
]


(* ::Subsection:: *)
(*Many arguments*)


Test[
	whitespaceQ["", " ", "\n", "\t", "\[IndentingNewLine]"]
	,
	True
	,
	TestID -> "Many arguments: whitespace"
]


Test[
	whitespaceQ["", " ", "\n", "a", "\t", "\[IndentingNewLine]"]
	,
	False
	,
	TestID -> "Many arguments: one non-whitespace"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
