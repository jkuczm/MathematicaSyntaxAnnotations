(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage[
	"SyntaxAnnotations`Tests`Integration`InfixMessageName`",
	{"MUnit`"}
]


Get["SyntaxAnnotations`Tests`Integration`init`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*One operator*)


Test[
	RowBox[{"::"}] // AnnotateSyntax
	,
	RowBox[{"::"}]
	,
	TestID -> "One operator: ::"
]

Test[
	RowBox[{"a", "::"}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["a", "UndefinedSymbol"],
		SyntaxBox["::", "SyntaxError"]
	}]
	,
	TestID -> "One operator: a::"
]
Test[
	RowBox[{"::", "b"}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["::", "SyntaxError"],
		SyntaxBox["b", "String"]
	}]
	,
	TestID -> "One operator: ::b"
]

Test[
	RowBox[{"c", "::", "d"}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["c", "UndefinedSymbol"],
		"::",
		SyntaxBox["d", "String"]
	}]
	,
	TestID -> "One operator: c::d"
]
Test[
	RowBox[{"e", "f", "::"}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["e", "UndefinedSymbol"],
		SyntaxBox["f", "UndefinedSymbol"],
		SyntaxBox["::", "SyntaxError"]
	}]
	,
	TestID -> "One operator: e f::"
]
Test[
	RowBox[{"::", "g", "h"}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["::", "SyntaxError"],
		SyntaxBox["g", "String"],
		SyntaxBox["h", "String"]
	}]
	,
	TestID -> "One operator: ::g h"
]

Test[
	RowBox[{"i", "j", "::", "k", "l"}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["i", "UndefinedSymbol"],
		SyntaxBox["j", "UndefinedSymbol"],
		"::",
		SyntaxBox["k", "String"],
		SyntaxBox["l", "String"]
	}]
	,
	TestID -> "One operator: i j::k l"
]


(* ::Subsection:: *)
(*Two operators*)


Test[
	RowBox[{"::", "::"}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["::", "SyntaxError"],
		SyntaxBox["::", "SyntaxError"]
	}]
	,
	TestID -> "Two operators: ::::"
]

Test[
	RowBox[{"a", "::", "::"}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["a", "UndefinedSymbol"],
		"::",
		SyntaxBox["::", "SyntaxError"]
	}]
	,
	TestID -> "Two operators: a::::"
]
Test[
	RowBox[{"::", "b", "::"}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["::", "SyntaxError"],
		SyntaxBox["b", "String"],
		SyntaxBox["::", "SyntaxError"]
	}]
	,
	TestID -> "Two operators: ::b::"
]
Test[
	RowBox[{"::", "::", "c"}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["::", "SyntaxError"],
		"::",
		SyntaxBox["c", "String"]
	}]
	,
	TestID -> "Two operators: ::::c"
]

Test[
	RowBox[{"d", "::", "e", "::"}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["d", "UndefinedSymbol"],
		"::",
		SyntaxBox["e", "String"],
		SyntaxBox["::", "SyntaxError"]
	}]
	,
	TestID -> "Two operators: d::e::"
]
Test[
	RowBox[{"f", "::", "::", "g"}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["f", "UndefinedSymbol"],
		"::",
		"::",
		SyntaxBox["g", "String"]
	}]
	,
	TestID -> "Two operators: f::::g"
]
Test[
	RowBox[{"::", "h", "::", "i"}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["::", "SyntaxError"],
		SyntaxBox["h", "String"],
		"::",
		SyntaxBox["i", "String"]
	}]
	,
	TestID -> "Two operators: ::h::i"
]

Test[
	RowBox[{"j", "::", "k", "::", "l"}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["j", "UndefinedSymbol"],
		"::",
		SyntaxBox["k", "String"],
		"::",
		SyntaxBox["l", "String"]
	}]
	,
	TestID -> "Two operators: j::k::l"
]


(* ::Subsection:: *)
(*Three operators*)


Test[
	RowBox[{"::", "::", "::"}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["::", "SyntaxError"],
		"::",
		SyntaxBox["::", "ExcessArgument"]
	}]
	,
	TestID -> "Three operators: ::::::"
]

Test[
	RowBox[{"a", "::", "::", "::"}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["a", "UndefinedSymbol"],
		"::",
		"::",
		SyntaxBox["::", "ExcessArgument"]
	}]
	,
	TestID -> "Three operators: a::::::"
]
Test[
	RowBox[{"::", "b", "::", "::"}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["::", "SyntaxError"],
		SyntaxBox["b", "String"],
		"::",
		SyntaxBox["::", "ExcessArgument"]
	}]
	,
	TestID -> "Three operators: ::b::::"
]
Test[
	RowBox[{"::", "::", "c", "::"}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["::", "SyntaxError"],
		"::",
		SyntaxBox["c", "String"],
		SyntaxBox["::", "ExcessArgument"]
	}]
	,
	TestID -> "Three operators: ::::c::"
]
Test[
	RowBox[{"::", "::", "::", "d"}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["::", "SyntaxError"],
		"::",
		SyntaxBox["::", "ExcessArgument"],
		SyntaxBox["d", "ExcessArgument"]
	}]
	,
	TestID -> "Three operators: ::::::d"
]


Test[
	RowBox[{"e", "::", "f", "::", "g", "::", "h"}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["e", "UndefinedSymbol"],
		"::",
		SyntaxBox["f", "String"],
		"::",
		SyntaxBox["g", "String"],
		SyntaxBox["::", "ExcessArgument"],
		SyntaxBox["h", "ExcessArgument"]
	}]
	,
	TestID -> "Three operators: e::f::g::h"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
