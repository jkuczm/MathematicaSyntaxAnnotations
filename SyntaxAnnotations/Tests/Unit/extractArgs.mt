(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["SyntaxAnnotations`Tests`Unit`extractArgs`", {"MUnit`"}]


Get["SyntaxAnnotations`"]

PrependTo[$ContextPath, "SyntaxAnnotations`Private`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*String*)


Test[
	extractArgs["a", {1}]
	,
	{"a"}
	,
	TestID -> "String: {1}"
]
Test[
	extractArgs["a", {1, 2}]
	,
	{"a"}
	,
	TestID -> "String: {1, 2}"
]
Test[
	extractArgs["a", {2, Infinity}]
	,
	{}
	,
	TestID -> "String: {2, Infinity}"
]
Test[
	extractArgs["a", 0]
	,
	{"a"}
	,
	TestID -> "String: 0"
]


(* ::Subsection:: *)
(*syntaxBox*)


Test[
	extractArgs[syntaxBox["a", type], {1}]
	,
	{"a"}
	,
	TestID -> "syntaxBox: {1}"
]
Test[
	extractArgs[syntaxBox["a", type], {1, 2}]
	,
	{"a"}
	,
	TestID -> "syntaxBox: {1, 2}"
]
Test[
	extractArgs[syntaxBox["a", type], {2, Infinity}]
	,
	{}
	,
	TestID -> "syntaxBox: {2, Infinity}"
]
Test[
	extractArgs[syntaxBox["a", type], 0]
	,
	{"a"}
	,
	TestID -> "syntaxBox: 0"
]


(* ::Subsection:: *)
(*RowBox: two comma separated strings*)


Test[
	extractArgs[RowBox[{"b", ",", "d"}], {1}]
	,
	{"b"}
	,
	TestID -> "RowBox: two comma separated strings: {1}"
]
Test[
	extractArgs[RowBox[{"b", ",", "d"}], {1, 2}]
	,
	{"b", "d"}
	,
	TestID -> "RowBox: two comma separated strings: {1, 2}"
]
Test[
	extractArgs[RowBox[{"b", ",", "d"}], {2, Infinity}]
	,
	{"d"}
	,
	TestID -> "RowBox: two comma separated strings: {2, Infinity}"
]
Test[
	extractArgs[RowBox[{"b", ",", "d"}], 0]
	,
	{RowBox[{"b", ",", "d"}]}
	,
	TestID -> "RowBox: two comma separated strings: 0"
]


(* ::Subsection:: *)
(*Complex*)


Test[
	extractArgs[
		RowBox[{
			syntaxBox["c", "Undefined"], ",",
			RowBox[{"x", "=", "y"}], ",",
			"e"
		}],
		{1}
	]
	,
	{"c"}
	,
	TestID -> "Complex: {1}"
]
Test[
	extractArgs[
		RowBox[{
			syntaxBox["c", "Undefined"], ",",
			RowBox[{"x", "=", "y"}], ",",
			"e"
		}],
		{1, 2}
	]
	,
	{"c", RowBox[{"x", "=", "y"}]}
	,
	TestID -> "Complex: {1, 2}"
]
Test[
	extractArgs[
		RowBox[{
			syntaxBox["c", "Undefined"], ",",
			RowBox[{"x", "=", "y"}], ",",
			"e"
		}],
		{2, Infinity}
	]
	,
	{RowBox[{"x", "=", "y"}], "e"}
	,
	TestID -> "Complex: {2, Infinity}"
]
Test[
	extractArgs[
		RowBox[{
			syntaxBox["c", "Undefined"], ",",
			RowBox[{"x", "=", "y"}], ",",
			"e"
		}],
		0
	]
	,
	{RowBox[{"c", ",", RowBox[{"x", "=", "y"}], ",", "e"}]}
	,
	TestID -> "Complex: 0"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
