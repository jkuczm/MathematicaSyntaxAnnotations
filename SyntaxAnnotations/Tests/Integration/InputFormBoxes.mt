(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["SyntaxAnnotations`Tests`Integration`InputFormBoxes`", {"MUnit`"}]


Get["SyntaxAnnotations`Tests`Integration`init`"]


(* ::Section:: *)
(*Tests*)


Test[
	RowBox[{
		"a_", " ", "/:", " ", "b_", " ", "=", " ", RowBox[{"a", "*", "b"}]
	}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["a_", "PatternVariable"],
		" ", "/:", " ",
		SyntaxBox["b_", "PatternVariable"],
		" ", "=", " ",
		RowBox[{
			SyntaxBox["a", "UndefinedSymbol"],
			"*",
			SyntaxBox["b", "UndefinedSymbol"]
		}]
	}]
	,
	TestID -> "a_ /: b_ = a*b"
]


Test[
	RowBox[{"a_", " ", ":>", " ", "a"}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["a_", "PatternVariable"],
		" ", ":>", " ",
		SyntaxBox["a", "PatternVariable", "UndefinedSymbol"]
	}]
	,
	TestID -> "a_ :> a"
]


Test[
	RowBox[{"Module", "[", RowBox[{
		RowBox[{"{", RowBox[{"a", " ", "=", " ", "b"}], "}"}],
		",", " ",
		RowBox[{"a", "*", "b"}]
	}], "]"}] // AnnotateSyntax
	,
	RowBox[{"Module", "[", RowBox[{
		RowBox[{"{", RowBox[{
			SyntaxBox["a", "LocalVariable", "UndefinedSymbol"],
			" ", "=", " ",
			SyntaxBox["b", "UndefinedSymbol"]
		}], "}"}],
		",", " ",
		RowBox[{
			SyntaxBox["a", "LocalVariable", "UndefinedSymbol"],
			"*",
			SyntaxBox["b", "UndefinedSymbol"]
		}]
	}], "]"}]
	,
	TestID -> "Module[{a = b}, a*b]"
]


Test[
	RowBox[{"Function", "[", RowBox[{
		RowBox[{"{", RowBox[{"a", ",", " ", "b"}], "}"}],
		",", " ",
		RowBox[{"a", "*", "b"}]
	}], "]"}] // AnnotateSyntax
	,
	RowBox[{"Function", "[", RowBox[{
		RowBox[{"{", RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			",", " ",
			SyntaxBox["b", "PatternVariable", "UndefinedSymbol"]
		}], "}"}],
		",", " ",
		RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			"*",
			SyntaxBox["b", "PatternVariable", "UndefinedSymbol"]
		}]
	}], "]"}]
	,
	TestID -> "Function[{a, b}, a*b]"
]


Test[
	RowBox[{
		RowBox[{"{", RowBox[{"a", ",", " ", "b"}], "}"}],
		" ", "\[Function]", " ",
		RowBox[{"a", "*", "b"}]
	}] // AnnotateSyntax
	,
	RowBox[{
		RowBox[{"{", RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			",", " ",
			SyntaxBox["b", "PatternVariable", "UndefinedSymbol"]
		}], "}"}],
		" ", "\[Function]", " ",
		RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			"*",
			SyntaxBox["b", "PatternVariable", "UndefinedSymbol"]
		}]
	}]
	,
	TestID -> "{a, b} \\[Function] a*b"
]


Test[
	RowBox[{"Table", "[", RowBox[{
		RowBox[{"a", "*", "b"}],
		",", " ",
		RowBox[{"{", RowBox[{"a", ",", " ", "b"}], "}"}]}],
	"]"}] // AnnotateSyntax
	,
	RowBox[{"Table", "[", RowBox[{
		RowBox[{
			SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
			"*",
			SyntaxBox["b", "UndefinedSymbol"]
		}],
		",", " ",
		RowBox[{"{", RowBox[{
			SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
			",", " ",
			SyntaxBox["b", "UndefinedSymbol"]
		}], "}"}]}],
	"]"}]
	,
	TestID -> "Table[a*b, {a, b}]"
]


Test[
	RowBox[{"Solve", "[", RowBox[{"a", ",", " ", "a"}], "]"}] // AnnotateSyntax
	,
	RowBox[{"Solve", "[", RowBox[{
		SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
		",", " ",
		SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"]
	}], "]"}]
	,
	TestID -> "Solve[a, a]"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
