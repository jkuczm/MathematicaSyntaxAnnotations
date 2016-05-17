(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["SyntaxAnnotations`Tests`Integration`Strings`", {"MUnit`"}]


Get["SyntaxAnnotations`Tests`Integration`init`"]


(* ::Section:: *)
(*Tests*)


Test[
	"\"\<string text\>\"" // AnnotateSyntax
	,
	SyntaxBox["\"\<string text\>\"", "String"]
	,
	TestID -> "Basic"
]

Test[
	"\"string text\"" // AnnotateSyntax
	,
	SyntaxBox["\"string text\"", "String"]
	,
	TestID -> "No legacy literal newlines wrapper"
]

Test[
	"\"\<\[Alpha]\[UnderBracket] + c\>\"" // AnnotateSyntax
	,
	SyntaxBox["\"\<\[Alpha]\[UnderBracket] + c\>\"", "String"]
	,
	TestID -> "Non-ASCII characters"
]

Test[
	"\"\<string \\\"with quotes\\\"\>\"" // AnnotateSyntax
	,
	SyntaxBox["\"\<string \\\"with quotes\\\"\>\"", "String"]
	,
	TestID -> "With escaped quotes"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
