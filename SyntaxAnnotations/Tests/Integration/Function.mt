(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["SyntaxAnnotations`Tests`Integration`Function`", {"MUnit`"}]


Get["SyntaxAnnotations`Tests`Integration`init`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*&*)


Test[
	a & // MakeBoxes // AnnotateSyntax
	,
	SyntaxExpr[a, "UndefinedSymbol"] & // MakeBoxes
	,
	TestID -> "a &"
]

Test[
	# & // MakeBoxes // AnnotateSyntax
	,
	SyntaxExpr[#, "PatternVariable"] & // MakeBoxes
	,
	TestID -> "# &"
]

Test[
	#2 & // MakeBoxes // AnnotateSyntax
	,
	SyntaxExpr[#2, "PatternVariable"] & // MakeBoxes
	,
	TestID -> "#2 &"
]

Test[
	## & // MakeBoxes // AnnotateSyntax
	,
	SyntaxExpr[##, "PatternVariable"] & // MakeBoxes
	,
	TestID -> "## &"
]

Test[
	##3 & // MakeBoxes // AnnotateSyntax
	,
	SyntaxExpr[##3, "PatternVariable"] & // MakeBoxes
	,
	TestID -> "##3 &"
]

If[$VersionNumber >= 10,
	Test[
		#name & // MakeBoxes // AnnotateSyntax
		,
		SyntaxExpr[#name, "PatternVariable"] & // MakeBoxes
		,
		TestID -> "#name &"
	]
]

Test[
	# + ##3 & // MakeBoxes // AnnotateSyntax
	,
	SyntaxExpr[#, "PatternVariable"] + SyntaxExpr[##3, "PatternVariable"] & //
		MakeBoxes
	,
	TestID -> "# + ##3 &"
]


(* ::Subsection:: *)
(*Function one argument*)


Test[
	RowBox[{"Function", "[", "a", "]"}] // AnnotateSyntax
	,
	RowBox[{"Function", "[", SyntaxBox["a", "UndefinedSymbol"], "]"}]
	,
	TestID -> "Function[a]"
]

Test[
	RowBox[{"Function", "[", "#", "]"}] // AnnotateSyntax
	,
	RowBox[{"Function", "[", SyntaxBox["#", "PatternVariable"], "]"}]
	,
	TestID -> "Function[#]"
]

Test[
	RowBox[{"Function", "[", "#1", "]"}] // AnnotateSyntax
	,
	RowBox[{"Function", "[", SyntaxBox["#1", "PatternVariable"], "]"}]
	,
	TestID -> "Function[#1]"
]

Test[
	RowBox[{"Function", "[", "##", "]"}] // AnnotateSyntax
	,
	RowBox[{"Function", "[", SyntaxBox["##", "PatternVariable"], "]"}]
	,
	TestID -> "Function[##]"
]

Test[
	RowBox[{"Function", "[", "##11", "]"}] // AnnotateSyntax
	,
	RowBox[{"Function", "[", SyntaxBox["##11", "PatternVariable"], "]"}]
	,
	TestID -> "Function[##11]"
]

If[$VersionNumber >= 10,
	Test[
		RowBox[{"Function", "[", "#name", "]"}] // AnnotateSyntax
		,
		RowBox[{"Function", "[", SyntaxBox["#name", "PatternVariable"], "]"}]
		,
		TestID -> "Function[#name]"
	]
]

Test[
	RowBox[{"Function", "[", RowBox[{"##", "+", "#9"}], "]"}] // AnnotateSyntax
	,
	RowBox[{"Function", "[", RowBox[{
		SyntaxBox["##", "PatternVariable"],
		"+",
		SyntaxBox["#9", "PatternVariable"]
	}], "]"}]
	,
	TestID -> "Function[## + #9]"
]


(* ::Subsection:: *)
(*Function two arguments*)


(* ::Subsubsection:: *)
(*First non-List*)


Test[
	Function[a, b] // MakeBoxes // AnnotateSyntax
	,
	Function[
		SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"],
		SyntaxExpr[b, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Function[a, b]"
]

Test[
	Function[a, a] // MakeBoxes // AnnotateSyntax
	,
	Function[
		SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"],
		SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Function[a, a]"
]

Test[
	Function[a, _a] // MakeBoxes // AnnotateSyntax
	,
	Function[
		SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"],
		SyntaxExpr[_a, "PatternVariable"]
	] // MakeBoxes
	,
	TestID -> "Function[a, _a]"
]

Test[
	Function[a, a_] // MakeBoxes // AnnotateSyntax
	,
	Function[
		SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"],
		SyntaxExpr[a_, "PatternVariable"]
	] // MakeBoxes
	,
	TestID -> "Function[a, a_]"
]

Test[
	Function[_a, a] // MakeBoxes // AnnotateSyntax
	,
	Function[
		SyntaxExpr[_a, "PatternVariable"],
		SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Function[_a, a]"
]
Test[
	Function[_\[SpadeSuit]1, \[SpadeSuit]1] // MakeBoxes // AnnotateSyntax
	,
	Function[
		SyntaxExpr[_\[SpadeSuit]1, "PatternVariable"],
		SyntaxExpr[\[SpadeSuit]1, "PatternVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Function[_\\[SpadeSuit]1, \\[SpadeSuit]1]"
]

Test[
	Function[a_, a] // MakeBoxes // AnnotateSyntax
	,
	Function[a_, SyntaxExpr[a, "UndefinedSymbol"]] // MakeBoxes
	,
	TestID -> "Function[a_, a]"
]

Test[
	Function[a:b, a b] // MakeBoxes // AnnotateSyntax
	,
	Function[
		RawBoxes@RowBox[{
			SyntaxBox["a", "UndefinedSymbol"],
			":",
			SyntaxBox["b", "UndefinedSymbol"]
		}],
		SyntaxExpr[a, "UndefinedSymbol"] SyntaxExpr[b, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Function[a:b, a b]"
]

Test[
	Function[a b, a b] // MakeBoxes // AnnotateSyntax
	,
	Function[
		SyntaxExpr[a, "UndefinedSymbol"] SyntaxExpr[b, "UndefinedSymbol"],
		SyntaxExpr[a, "UndefinedSymbol"] SyntaxExpr[b, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Function[a b, a b]"
]

Test[
	Function[a = b, a b] // MakeBoxes // AnnotateSyntax
	,
	Function[
		SyntaxExpr[a, "UndefinedSymbol"] = SyntaxExpr[b, "UndefinedSymbol"],
		SyntaxExpr[a, "UndefinedSymbol"] SyntaxExpr[b, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Function[a = b, a b]"
]


(* ::Subsubsection:: *)
(*First List*)


Test[
	Function[{a}, b] // MakeBoxes // AnnotateSyntax
	,
	Function[
		{SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]},
		SyntaxExpr[b, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Function[{a}, b]"
]

Test[
	Function[{a}, a] // MakeBoxes // AnnotateSyntax
	,
	Function[
		{SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]},
		SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Function[{a}, a]"
]

Test[
	Function[{a}, _a] // MakeBoxes // AnnotateSyntax
	,
	Function[
		{SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]},
		SyntaxExpr[_a, "PatternVariable"]
	] // MakeBoxes
	,
	TestID -> "Function[{a}, _a]"
]

Test[
	Function[{a}, a_] // MakeBoxes // AnnotateSyntax
	,
	Function[
		{SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]},
		SyntaxExpr[a_, "PatternVariable"]
	] // MakeBoxes
	,
	TestID -> "Function[{a}, a_]"
]

Test[
	Function[{_a}, a] // MakeBoxes // AnnotateSyntax
	,
	Function[
		{SyntaxExpr[_a, "PatternVariable"]},
		SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Function[{_a}, a]"
]

Test[
	Function[{a_}, a] // MakeBoxes // AnnotateSyntax
	,
	Function[{a_}, SyntaxExpr[a, "UndefinedSymbol"]] // MakeBoxes
	,
	TestID -> "Function[{a_}, a]"
]

Test[
	Function[{a:b}, a b] // MakeBoxes // AnnotateSyntax
	,
	Function[
		{
			RawBoxes@RowBox[{
				SyntaxBox["a", "UndefinedSymbol"],
				":",
				SyntaxBox["b", "PatternVariable", "UndefinedSymbol"]
			}]
		},
		SyntaxExpr[a, "UndefinedSymbol"] *
		SyntaxExpr[b, "PatternVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Function[{a:b}, a b]"
]

Test[
	Function[{a b}, a b] // MakeBoxes // AnnotateSyntax
	,
	Function[
		{
			SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
			SyntaxExpr[b, "PatternVariable", "UndefinedSymbol"]
		}
		,
		SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
		SyntaxExpr[b, "PatternVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Function[{a b}, a b]"
]

Test[
	Function[{a = b}, a b] // MakeBoxes // AnnotateSyntax
	,
	Function[
		{
			SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] =
				SyntaxExpr[b, "UndefinedSymbol"]
		}
		,
		SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
		SyntaxExpr[b, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Function[{a = b}, a b]"
]

Test[
	Function[{a = a}, a] // MakeBoxes // AnnotateSyntax
	,
	Function[
		{
			SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] =
				SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
		}
		,
		SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Function[{a = a}, a]"
]

Test[
	Function[{a_ = b}, a b] // MakeBoxes // AnnotateSyntax
	,
	Function[
		{SyntaxExpr[a_, "PatternVariable"] = SyntaxExpr[b, "UndefinedSymbol"]},
		SyntaxExpr[a, "UndefinedSymbol"] SyntaxExpr[b, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Function[{a_ = b}, a b]"
]

Test[
	Function[{f[a] = a b}, f a b] // MakeBoxes // AnnotateSyntax
	,
	Function[
		{
			SyntaxExpr[f, "PatternVariable", "UndefinedSymbol"][
				SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
			] =
				SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
				SyntaxExpr[b, "UndefinedSymbol"]
		},
		SyntaxExpr[f, "PatternVariable", "UndefinedSymbol"] *
		SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
		SyntaxExpr[b, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Function[{a_ = b}, a b]"
]

Test[
	Function[{a, b}, a b] // MakeBoxes // AnnotateSyntax
	,
	Function[
		{
			SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"],
			SyntaxExpr[b, "PatternVariable", "UndefinedSymbol"]
		}
		,
		SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"] *
		SyntaxExpr[b, "PatternVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Function[{a, b}, a b]"
]


(* ::Subsection:: *)
(*Function three arguments*)


Test[
	Function[a, b, c] // MakeBoxes // AnnotateSyntax
	,
	Function[
		SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"],
		SyntaxExpr[b, "UndefinedSymbol"],
		SyntaxExpr[c, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Function[a, b, c]"
]

Test[
	Function[a, a, a] // MakeBoxes // AnnotateSyntax
	,
	Function[
		SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"],
		SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"],
		SyntaxExpr[a, "PatternVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Function[a, a, a]"
]


(* ::Subsection:: *)
(*\[Function]*)


(* ::Subsubsection:: *)
(*First non-List*)


Test[
	RowBox[{"a", "\[Function]", "b"}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
		"\[Function]",
		SyntaxBox["b", "UndefinedSymbol"]
	}]
	,
	TestID -> "a \\[Function] b"
]

Test[
	RowBox[{"a", "\[Function]", "a"}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
		"\[Function]",
		SyntaxBox["a", "PatternVariable", "UndefinedSymbol"]
	}]
	,
	TestID -> "a \\[Function] a"
]

Test[
	RowBox[{"a", "\[Function]", "_a"}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
		"\[Function]",
		SyntaxBox["_a", "PatternVariable"]
	}]
	,
	TestID -> "a \\[Function] _a"
]

Test[
	RowBox[{"a", "\[Function]", "a_"}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
		"\[Function]",
		SyntaxBox["a_", "PatternVariable"]
	}]
	,
	TestID -> "a \\[Function] a_"
]

Test[
	RowBox[{"_a", "\[Function]", "a"}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox["_a", "PatternVariable"],
		"\[Function]",
		SyntaxBox["a", "PatternVariable", "UndefinedSymbol"]
	}]
	,
	TestID -> "_a \\[Function] a"
]

Test[
	RowBox[{"a_", "\[Function]", "a"}] // AnnotateSyntax
	,
	RowBox[{"a_", "\[Function]", SyntaxBox["a", "UndefinedSymbol"]}]
	,
	TestID -> "a_ \\[Function] a"
]

Test[
	RowBox[{
		RowBox[{"a", " ", "b"}],
		"\[Function]",
		RowBox[{"a", " ", "b"}]
	}] // AnnotateSyntax
	,
	RowBox[{
		RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			" ",
			SyntaxBox["b", "PatternVariable", "UndefinedSymbol"]
		}],
		"\[Function]",
		RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			" ",
			SyntaxBox["b", "PatternVariable", "UndefinedSymbol"]
		}]
	}]
	,
	TestID -> "a b \\[Function] a b"
]

Test[
	RowBox[{
		RowBox[{"(", RowBox[{"a", "=", "b"}], ")"}],
		"\[Function]",
		RowBox[{"a", " ", "b"}]
	}] // AnnotateSyntax
	,
	RowBox[{
		RowBox[{"(", RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			"=",
			SyntaxBox["b", "UndefinedSymbol"]
		}], ")"}],
		"\[Function]",
		RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			" ",
			SyntaxBox["b", "UndefinedSymbol"]
		}]
	}]
	,
	TestID -> "(a = b) \\[Function] a b"
]

Test[
	RowBox[{
		RowBox[{"(", RowBox[{"a", "=", "a"}], ")"}],
		"\[Function]",
		"a"
	}] // AnnotateSyntax
	,
	RowBox[{
		RowBox[{"(", RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			"=",
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"]
		}], ")"}],
		"\[Function]",
		SyntaxBox["a", "PatternVariable", "UndefinedSymbol"]
	}]
	,
	TestID -> "(a = a) \\[Function] a"
]


(* ::Subsubsection:: *)
(*First List*)


Test[
	RowBox[{RowBox[{"{", "a", "}"}], "\[Function]", "b"}] // AnnotateSyntax
	,
	RowBox[{
		RowBox[{"{",
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
		"}"}],
		"\[Function]",
		SyntaxBox["b", "UndefinedSymbol"]
	}]
	,
	TestID -> "{a} \\[Function] b"
]

Test[
	RowBox[{RowBox[{"{", "a", "}"}], "\[Function]", "a"}] // AnnotateSyntax
	,
	RowBox[{
		RowBox[{"{",
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
		"}"}],
		"\[Function]",
		SyntaxBox["a", "PatternVariable", "UndefinedSymbol"]
	}]
	,
	TestID -> "{a} \\[Function] a"
]

Test[
	RowBox[{RowBox[{"{", "a", "}"}], "\[Function]", "_a"}] // AnnotateSyntax
	,
	RowBox[{
		RowBox[{"{",
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
		"}"}],
		"\[Function]",
		SyntaxBox["_a", "PatternVariable"]
	}]
	,
	TestID -> "{a} \\[Function] _a"
]

Test[
	RowBox[{RowBox[{"{", "a", "}"}], "\[Function]", "a_"}] // AnnotateSyntax
	,
	RowBox[{
		RowBox[{"{",
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
		"}"}],
		"\[Function]",
		SyntaxBox["a_", "PatternVariable"]
	}]
	,
	TestID -> "{a} \\[Function] a_"
]

Test[
	RowBox[{RowBox[{"{", "_a", "}"}], "\[Function]", "a"}] // AnnotateSyntax
	,
	RowBox[{
		RowBox[{"{", SyntaxBox["_a", "PatternVariable"], "}"}],
		"\[Function]",
		SyntaxBox["a", "PatternVariable", "UndefinedSymbol"]
	}]
	,
	TestID -> "{_a} \\[Function] a"
]

Test[
	RowBox[{RowBox[{"{", "a_", "}"}], "\[Function]", "a"}] // AnnotateSyntax
	,
	RowBox[{
		RowBox[{"{", "a_", "}"}],
		"\[Function]",
		SyntaxBox["a", "UndefinedSymbol"]
	}]
	,
	TestID -> "{a_} \\[Function] a"
]

Test[
	RowBox[{
		RowBox[{"{", RowBox[{"a", " ", "b"}], "}"}],
		"\[Function]",
		RowBox[{"a", " ", "b"}]
	}] // AnnotateSyntax
	,
	RowBox[{
		RowBox[{"{", RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			" ",
			SyntaxBox["b", "PatternVariable", "UndefinedSymbol"]
		}], "}"}],
		"\[Function]",
		RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			" ",
			SyntaxBox["b", "PatternVariable", "UndefinedSymbol"]
		}]
	}]
	,
	TestID -> "{a b} \\[Function] a b"
]

Test[
	RowBox[{
		RowBox[{"{", RowBox[{"a", "=", "b"}], "}"}],
		"\[Function]",
		RowBox[{"a", " ", "b"}]
	}] // AnnotateSyntax
	,
	RowBox[{
		RowBox[{"{", RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			"=",
			SyntaxBox["b", "UndefinedSymbol"]
		}], "}"}],
		"\[Function]",
		RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			" ",
			SyntaxBox["b", "UndefinedSymbol"]
		}]
	}]
	,
	TestID -> "{a = b} \\[Function] a b"
]

Test[
	RowBox[{
		RowBox[{"{", RowBox[{"a", ",", "b"}], "}"}],
		"\[Function]",
		RowBox[{"a", " ", "b"}]
	}] // AnnotateSyntax
	,
	RowBox[{
		RowBox[{"{", RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			",",
			SyntaxBox["b", "PatternVariable", "UndefinedSymbol"]
		}], "}"}],
		"\[Function]",
		RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			" ",
			SyntaxBox["b", "PatternVariable", "UndefinedSymbol"]
		}]
	}]
	,
	TestID -> "{a, b} \\[Function] a b"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
