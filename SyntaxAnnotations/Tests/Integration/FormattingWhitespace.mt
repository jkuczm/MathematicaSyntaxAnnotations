(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage[
	"SyntaxAnnotations`Tests`Integration`FormattingWhitespace`", {"MUnit`"}
]


Get["SyntaxAnnotations`Tests`Integration`init`"]


(* ::Section:: *)
(*Tests*)


Test[
	RowBox[{
		"\t", "a_", "\[IndentingNewLine]",
			"/:", "\n\t",
				"b_", " ", ":=", "\n",
					"\t", RowBox[{" ", "a", " \t", "b"}], " "
	}] // AnnotateSyntax
	,
	RowBox[{
		"\t", SyntaxBox["a_", "PatternVariable"], "\[IndentingNewLine]",
			"/:", "\n\t",
				SyntaxBox["b_", "PatternVariable"], " ", ":=", "\n",
					"\t", RowBox[{
						" ",
						SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
						" \t",
						SyntaxBox["b", "PatternVariable", "UndefinedSymbol"]
					}],
					" "
	}]
	,
	TestID -> "a_ /: b_ := a b"
]


Test[
	RowBox[{
		" ", "a_", " \t\t", "->", " \[IndentingNewLine]",
			" \n ",
			"a", "\n"
	}] // AnnotateSyntax
	,
	RowBox[{
		" ", SyntaxBox["a_", "PatternVariable"], " \t\t",
		"->", " \[IndentingNewLine]",
			" \n ",
			SyntaxBox["a", "UndefinedSymbol"], "\n"
	}]
	,
	TestID -> "a_ -> a"
]


Test[
	RowBox[{"\n", "With", "\t", "[", RowBox[{
		" ", RowBox[{"{", "\[IndentingNewLine]",
			RowBox[{"\n", "a", "\t", "=", " ", "\[IndentingNewLine]", "b"}],
		"}", "\n "}],
		",", " \t",
		RowBox[{"a", " \t", "b"}]
	}], "", "]", "\n \t"}] // AnnotateSyntax
	,
	RowBox[{"\n", "With", "\t", "[", RowBox[{
		" ", RowBox[{"{", "\[IndentingNewLine]", RowBox[{
			"\n",
			SyntaxBox["a", "LocalVariable", "UndefinedSymbol"],
			"\t", "=", " ", "\[IndentingNewLine]",
			SyntaxBox["b", "UndefinedSymbol"]
		}], "}", "\n "}],
		",", " \t",
		RowBox[{
			SyntaxBox["a", "LocalVariable", "UndefinedSymbol"],
			" \t",
			SyntaxBox["b", "UndefinedSymbol"]
		}]
	}], "", "]", "\n \t"}]
	,
	TestID -> "Module[{a = b}, a b]"
]


Test[
	RowBox[{"", "Function", "\[IndentingNewLine]", " ", "[", "", RowBox[{
		"", "2", "\n", "", "#", "\[IndentingNewLine] "
	}], "", "]", "\n"}] // AnnotateSyntax
	,
	RowBox[{"", "Function", "\[IndentingNewLine]", " ", "[", "", RowBox[{
		"", "2", "\n", "",
		SyntaxBox["#", "PatternVariable"], "\[IndentingNewLine] "
	}], "", "]", "\n"}]
	,
	TestID -> "Function[2 #]"
]
Test[
	RowBox[{"", "Function", "\[IndentingNewLine]", " ", "[", "", RowBox[{
		"\t", RowBox[{"", "{", "\n", RowBox[{
			" ", "a",
			"", ",", "\t ",
			"b", "\n"
		}], "", "}", " "}],
		"\[IndentingNewLine] ", ",", " ", "\t",
		RowBox[{"a", " ", "b"}]
	}], "", "]", "\n"}] // AnnotateSyntax
	,
	RowBox[{"", "Function", "\[IndentingNewLine]", " ", "[", "", RowBox[{
		"\t", RowBox[{"", "{", "\n", RowBox[{
			" ", SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			"", ",", "\t ",
			SyntaxBox["b", "PatternVariable", "UndefinedSymbol"], "\n"
		}], "", "}", " "}],
		"\[IndentingNewLine] ", ",", " ", "\t",
		RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			" ",
			SyntaxBox["b", "PatternVariable", "UndefinedSymbol"]
		}]
	}], "", "]", "\n"}]
	,
	TestID -> "Function[{a, b}, a b]"
]


Test[
	RowBox[{
		"\n", RowBox[{"\[IndentingNewLine]", "{", "\t", RowBox[{
			"\n", "a", ",", " ", "b", " "
		}], "", "}", "\t\n"}],
		" ", "\[Function]", " \[IndentingNewLine]",
		RowBox[{"a", " ", "b"}], ""
	}] // AnnotateSyntax
	,
	RowBox[{
		"\n", RowBox[{"\[IndentingNewLine]", "{", "\t", RowBox[{
			"\n", SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			",", " ",
			SyntaxBox["b", "PatternVariable", "UndefinedSymbol"], " "
		}], "", "}", "\t\n"}],
		" ", "\[Function]", " \[IndentingNewLine]",
		RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			" ",
			SyntaxBox["b", "PatternVariable", "UndefinedSymbol"]
		}], ""
	}]
	,
	TestID -> "{a, b} \\[Function] a*b"
]


Test[
	RowBox[{"\t", " ", "Table", "", "[", "\n ", RowBox[{
		"\[IndentingNewLine]\t", RowBox[{
			"", "a", " ", "b", "\t"
		}],
		" \n", ",", " ",
		RowBox[{"", "{", "  ", RowBox[{
			"\t", "a", "", ",", "\n", "b", "\[IndentingNewLine]"
		}], " ", "}", "\t"}]}],
	"", "]", ""}] // AnnotateSyntax
	,
	RowBox[{"\t", " ", "Table", "", "[", "\n ",RowBox[{
		"\[IndentingNewLine]\t", RowBox[{
			"", SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
			" ",
			SyntaxBox["b", "UndefinedSymbol"], "\t"
		}],
		" \n", ",", " ",
		RowBox[{"", "{", "  ", RowBox[{
			"\t", SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
			"", ",", "\n",
			SyntaxBox["b", "UndefinedSymbol"], "\[IndentingNewLine]"
		}], " ", "}", "\t"}]}],
	"", "]", ""}]
	,
	TestID -> "Table[a b, {a, b}]"
]


Test[
	RowBox[{"\[IndentingNewLine]\n", "Solve", "\t", "\t", "[", "", RowBox[{
		"", "a", ",", "\n", "a", "\[IndentingNewLine]"
	}], " ", "]", "", "\t"}] // AnnotateSyntax
	,
	RowBox[{"\[IndentingNewLine]\n", "Solve", "\t", "\t", "[", "", RowBox[{
		"", SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
		",", "\n",
		SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
		"\[IndentingNewLine]"
	}], " ", "]", "", "\t"}]
	,
	TestID -> "Solve[a, a]"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
