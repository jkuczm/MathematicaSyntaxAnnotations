(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["SyntaxAnnotations`Tests`Integration`Functions`", {"MUnit`"}]


Get["SyntaxAnnotations`Tests`Integration`init`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Table*)


Test[
	Table[a, a] // MakeBoxes // AnnotateSyntax
	,
	Table[
		syntaxExpr[a, "UndefinedSymbol"],
		syntaxExpr[a, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Table[a, a]"
]


Test[
	Table[a, {a}] // MakeBoxes // AnnotateSyntax
	,
	Table[
		syntaxExpr[a, "UndefinedSymbol"],
		{syntaxExpr[a, "UndefinedSymbol"]}
	] // MakeBoxes
	,
	TestID -> "Table[a, {a}]"
]


Test[
	Table[a b, {a, b}] // MakeBoxes // AnnotateSyntax
	,
	Table[
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		syntaxExpr[b, "UndefinedSymbol"]
		,
		{
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
			syntaxExpr[b, "UndefinedSymbol"]
		}
	] // MakeBoxes
	,
	TestID -> "Table[a b, {a, b}]"
]


Test[
	Table[a b c, {a, b, c}] // MakeBoxes // AnnotateSyntax
	,
	Table[
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		syntaxExpr[b, "UndefinedSymbol"] *
		syntaxExpr[c, "UndefinedSymbol"]
		,
		{
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
			syntaxExpr[b, "UndefinedSymbol"],
			syntaxExpr[c, "UndefinedSymbol"]
		}
	] // MakeBoxes
	,
	TestID -> "Table[a b c, {a, b, c}]"
]


Test[
	Table[a b c d, {a, b, c, d}] // MakeBoxes // AnnotateSyntax
	,
	Table[
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		syntaxExpr[b, "UndefinedSymbol"] *
		syntaxExpr[c, "UndefinedSymbol"] *
		syntaxExpr[d, "UndefinedSymbol"]
		,
		{
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
			syntaxExpr[b, "UndefinedSymbol"],
			syntaxExpr[c, "UndefinedSymbol"],
			syntaxExpr[d, "UndefinedSymbol"]
		}
	] // MakeBoxes
	,
	TestID -> "Table[a b c d, {a, b, c, d}]"
]


Test[
	Table[a b c d, {a, b}, {c, d}] // MakeBoxes // AnnotateSyntax
	,
	Table[
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		syntaxExpr[b, "UndefinedSymbol"] *
		syntaxExpr[c, "FunctionLocalVariable", "UndefinedSymbol"] *
		syntaxExpr[d, "UndefinedSymbol"]
		,
		{
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
			syntaxExpr[b, "UndefinedSymbol"]
		}
		,
		{
			syntaxExpr[c, "FunctionLocalVariable", "UndefinedSymbol"],
			syntaxExpr[d, "UndefinedSymbol"]
		}
	] // MakeBoxes
	,
	TestID -> "Table[a b c d, {a, b}, {c, d}]"
]


(* ::Subsection:: *)
(*Plot*)


Test[
	Plot[a, a] // MakeBoxes // AnnotateSyntax
	,
	Plot[
		syntaxExpr[a, "UndefinedSymbol"],
		syntaxExpr[a, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Plot[a, a]"
]


Test[
	Plot[a, {a}] // MakeBoxes // AnnotateSyntax
	,
	Plot[
		syntaxExpr[a, "UndefinedSymbol"],
		{syntaxExpr[a, "UndefinedSymbol"]}
	] // MakeBoxes
	,
	TestID -> "Plot[a, {a}]"
]


Test[
	Plot[a b, {a, b}] // MakeBoxes // AnnotateSyntax
	,
	Plot[
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		syntaxExpr[b, "UndefinedSymbol"],
		{
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
			syntaxExpr[b, "UndefinedSymbol"]
		}
	] // MakeBoxes
	,
	TestID -> "Plot[a b, {a, b}]"
]


Test[
	Plot[a b c, {a, b, c}] // MakeBoxes // AnnotateSyntax
	,
	Plot[
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		syntaxExpr[b, "UndefinedSymbol"] *
		syntaxExpr[c, "UndefinedSymbol"],
		{
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
			syntaxExpr[b, "UndefinedSymbol"],
			syntaxExpr[c, "UndefinedSymbol"]
		}
	] // MakeBoxes
	,
	TestID -> "Plot[a b c, {a, b, c}]"
]


Test[
	Plot[a b c d, {a, b}, {c, d}] // MakeBoxes // AnnotateSyntax
	,
	Plot[
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		syntaxExpr[b, "UndefinedSymbol"] *
		syntaxExpr[c, "UndefinedSymbol"] *
		syntaxExpr[d, "UndefinedSymbol"],
		{
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
			syntaxExpr[b, "UndefinedSymbol"]
		},
		{
			syntaxExpr[c, "UndefinedSymbol"],
			syntaxExpr[d, "UndefinedSymbol"]
		}
	] // MakeBoxes
	,
	TestID -> "Plot[a b c d, {a, b}, {c, d}]"
]


(* ::Subsection:: *)
(*Solve*)


Test[
	Solve[b, a] // MakeBoxes // AnnotateSyntax
	,
	Solve[
		syntaxExpr[b, "UndefinedSymbol"],
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Solve[b, a]"
]


Test[
	Solve[a, a] // MakeBoxes // AnnotateSyntax
	,
	Solve[
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Solve[a, a]"
]


Test[
	Solve[a, a_] // MakeBoxes // AnnotateSyntax
	,
	Solve[syntaxExpr[a, "UndefinedSymbol"], a_] // MakeBoxes
	,
	TestID -> "Solve[a, a_]"
]


Test[
	Solve[a, _a] // MakeBoxes // AnnotateSyntax
	,
	Solve[syntaxExpr[a, "UndefinedSymbol"], _a] // MakeBoxes
	,
	TestID -> "Solve[a, _a]"
]


Test[
	Solve[a, {a}] // MakeBoxes // AnnotateSyntax
	,
	Solve[
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
		{syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]}
	] // MakeBoxes
	,
	TestID -> "Solve[a, {a}]"
]


Test[
	Solve[a b, {a, b}] // MakeBoxes // AnnotateSyntax
	,
	Solve[
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		syntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"]
		,
		{
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
			syntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"]
		}
	] // MakeBoxes
	,
	TestID -> "Solve[a b, {a, b}]"
]


Test[
	Solve[a b, a, b] // MakeBoxes // AnnotateSyntax
	,
	Solve[
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		syntaxExpr[b, "UndefinedSymbol"]
		,
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
		,
		syntaxExpr[b, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Solve[a b, a, b]"
]


(* ::Subsection:: *)
(*Limit*)


Test[
	Limit[a, a] // MakeBoxes // AnnotateSyntax
	,
	Limit[
		syntaxExpr[a, "UndefinedSymbol"],
		syntaxExpr[a, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Limit[a, a]"
]


Test[
	Limit[a b, a -> b] // MakeBoxes // AnnotateSyntax
	,
	Limit[
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		syntaxExpr[b, "UndefinedSymbol"]
		,
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] ->
			syntaxExpr[b, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Limit[a b, a -> b]"
]


Test[
	Limit[a b, a \[Rule] b] // MakeBoxes // AnnotateSyntax
	,
	Limit[
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		syntaxExpr[b, "UndefinedSymbol"]
		,
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] ->
			syntaxExpr[b, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Limit[a b, a \\[Rule] b]"
]


Test[
	Limit[a b, a :> b] // MakeBoxes // AnnotateSyntax
	,
	Limit[
		syntaxExpr[a, "UndefinedSymbol"] syntaxExpr[b, "UndefinedSymbol"],
		syntaxExpr[a, "UndefinedSymbol"] :> syntaxExpr[b, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Limit[a b, :> b]"
]


Test[
	Limit[a b c d, a -> b, c -> d] // MakeBoxes // AnnotateSyntax
	,
	Limit[
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		syntaxExpr[b, "UndefinedSymbol"] *
		syntaxExpr[c, "UndefinedSymbol"] *
		syntaxExpr[d, "UndefinedSymbol"]
		,
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] ->
			syntaxExpr[b, "UndefinedSymbol"]
		,
		syntaxExpr[c, "UndefinedSymbol"] -> syntaxExpr[d, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "Limit[a b c d, a -> b, c -> d]"
]


(* ::Subsection:: *)
(*Sum*)


Test[
	RowBox[{"Sum", "[", RowBox[{"a", ",", "a"}], "]"}] // AnnotateSyntax
	,
	RowBox[{"Sum", "[", RowBox[{
		SyntaxBox["a", "UndefinedSymbol"],
		",",
		SyntaxBox["a", "UndefinedSymbol"]
	}], "]"}]
	,
	TestID -> "Sum[a, a]"
]


Test[
	RowBox[{"Sum", "[", RowBox[{"a", ",",  RowBox[{"{", "a", "}"}]}], "]"}] //
		AnnotateSyntax
	,
	RowBox[{"Sum", "[", RowBox[{
		SyntaxBox["a", "UndefinedSymbol"],
		",",
		RowBox[{"{", SyntaxBox["a", "UndefinedSymbol"], "}"}]
	}], "]"}]
	,
	TestID -> "Sum[a, {a}]"
]


Test[
	RowBox[{"Sum", "[", RowBox[{
		RowBox[{"a", " ", "b"}],
		",",
		RowBox[{"{", RowBox[{"a", ",", "b"}], "}"}]
	}], "]"}] // AnnotateSyntax
	,
	RowBox[{"Sum", "[", RowBox[{
		RowBox[{
			SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
			" ",
			SyntaxBox["b", "UndefinedSymbol"]
		}],
		",",
		RowBox[{"{", RowBox[{
			SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
			",",
			SyntaxBox["b", "UndefinedSymbol"]
		}], "}"}]
	}], "]"}]
	,
	TestID -> "Sum[a b, {a, b}]"
]


Test[
	RowBox[{"Sum", "[", RowBox[{
		RowBox[{"a", " ", "b", " ", "c"}],
		",",
		RowBox[{"{", RowBox[{"a", ",", "b", ",", "c"}], "}"}]
	}], "]"}] // AnnotateSyntax
	,
	RowBox[{"Sum", "[", RowBox[{
		RowBox[{
			SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
			" ",
			SyntaxBox["b", "UndefinedSymbol"],
			" ",
			SyntaxBox["c", "UndefinedSymbol"]
		}],
		",",
		RowBox[{"{", RowBox[{
			SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
			",",
			SyntaxBox["b", "UndefinedSymbol"],
			",",
			SyntaxBox["c", "UndefinedSymbol"]
		}], "}"}]
	}], "]"}]
	,
	TestID -> "Sum[a b c, {a, b, c}]"
]


Test[
	RowBox[{"Sum", "[", RowBox[{
		RowBox[{"a", " ", "b", " ", "c", " ", "d", " ", "e", " ", "f"}],
		",",
		RowBox[{"{", RowBox[{"a", ",", "b", ",", "c"}], "}"}],
		",",
		RowBox[{"{", RowBox[{"d", ",", "e", ",", "f"}], "}"}]
	}], "]"}] // AnnotateSyntax
	,
	RowBox[{"Sum", "[", RowBox[{
		RowBox[{
			SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
			" ",
			SyntaxBox["b", "UndefinedSymbol"],
			" ",
			SyntaxBox["c", "UndefinedSymbol"],
			" ",
			SyntaxBox["d", "FunctionLocalVariable", "UndefinedSymbol"],
			" ",
			SyntaxBox["e", "UndefinedSymbol"],
			" ",
			SyntaxBox["f", "UndefinedSymbol"]
		}],
		",",
		RowBox[{"{", RowBox[{
			SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
			",",
			SyntaxBox["b", "UndefinedSymbol"],
			",",
			SyntaxBox["c", "UndefinedSymbol"]
		}], "}"}],
		",",
		RowBox[{"{", RowBox[{
			SyntaxBox["d", "FunctionLocalVariable", "UndefinedSymbol"],
			",",
			SyntaxBox["e", "UndefinedSymbol"],
			",",
			SyntaxBox["f", "UndefinedSymbol"]
		}], "}"}]
	}], "]"}]
	,
	TestID -> "Sum[a b c d e f, {a, b, c}, {d, e, f}]"
]


(* ::Subsection:: *)
(*\[Sum]*)


Test[
	Sum[a, a] // MakeBoxes // AnnotateSyntax
	,
	Sum[
		syntaxExpr[a, "UndefinedSymbol"],
		syntaxExpr[a, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "\[Sum]: Sum[a, a]"
]


Test[
	Sum[a, {a}] // MakeBoxes // AnnotateSyntax
	,
	Sum[
		syntaxExpr[a, "UndefinedSymbol"],
		{syntaxExpr[a, "UndefinedSymbol"]}
	] // MakeBoxes
	,
	TestID -> "\[Sum]: Sum[a, {a}]"
]


Test[
	Sum[a b, {a, b}] // MakeBoxes // AnnotateSyntax
	,
	Sum[
		syntaxExpr[a, "UndefinedSymbol"] syntaxExpr[b, "UndefinedSymbol"],
		{syntaxExpr[a, "UndefinedSymbol"], syntaxExpr[b, "UndefinedSymbol"]}
	] // MakeBoxes
	,
	TestID -> "\[Sum]: Sum[a b, {a, b}]"
]


Test[
	Sum[a b c, {a, b, c}] // MakeBoxes // AnnotateSyntax
	,
	Sum[
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		syntaxExpr[b, "UndefinedSymbol"] *
		syntaxExpr[c, "UndefinedSymbol"]
		,
		{
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
			syntaxExpr[b, "UndefinedSymbol"],
			syntaxExpr[c, "UndefinedSymbol"]
		}
	] // MakeBoxes
	,
	TestID -> "\[Sum]: Sum[a b c, {a, b, c}]"
]


Test[
	Sum[a b c d e f, {a, b, c}, {d, e, f}] // MakeBoxes // AnnotateSyntax
	,
	Sum[
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		syntaxExpr[b, "UndefinedSymbol"] *
		syntaxExpr[c, "UndefinedSymbol"] *
		syntaxExpr[d, "FunctionLocalVariable", "UndefinedSymbol"] *
		syntaxExpr[e, "UndefinedSymbol"] *
		syntaxExpr[f, "UndefinedSymbol"]
		,
		{
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
			syntaxExpr[b, "UndefinedSymbol"],
			syntaxExpr[c, "UndefinedSymbol"]
		}
		,
		{
			syntaxExpr[d, "FunctionLocalVariable", "UndefinedSymbol"],
			syntaxExpr[e, "UndefinedSymbol"],
			syntaxExpr[f, "UndefinedSymbol"]
		}
	] // MakeBoxes
	,
	TestID -> "\[Sum]: Sum[a b c d e f, {a, b, c}, {d, e, f}]"
]


(* ::Subsection:: *)
(*Product*)


Test[
	RowBox[{"Product", "[", RowBox[{"a", ",", "a"}], "]"}] // AnnotateSyntax
	,
	RowBox[{"Product", "[", RowBox[{
		SyntaxBox["a", "UndefinedSymbol"],
		",",
		SyntaxBox["a", "UndefinedSymbol"]
	}], "]"}]
	,
	TestID -> "Product[a, a]"
]


Test[
	RowBox[{"Product", "[",
		RowBox[{"a", ",",  RowBox[{"{", "a", "}"}]}],
	"]"}] // AnnotateSyntax
	,
	RowBox[{"Product", "[", RowBox[{
		SyntaxBox["a", "UndefinedSymbol"],
		",",
		RowBox[{"{", SyntaxBox["a", "UndefinedSymbol"], "}"}]
	}], "]"}]
	,
	TestID -> "Product[a, {a}]"
]


Test[
	RowBox[{"Product", "[", RowBox[{
		RowBox[{"a", " ", "b"}],
		",",
		RowBox[{"{", RowBox[{"a", ",", "b"}], "}"}]
	}], "]"}] // AnnotateSyntax
	,
	RowBox[{"Product", "[", RowBox[{
		RowBox[{
			SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
			" ",
			SyntaxBox["b", "UndefinedSymbol"]
		}],
		",",
		RowBox[{"{", RowBox[{
			SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
			",",
			SyntaxBox["b", "UndefinedSymbol"]
		}], "}"}]
	}], "]"}]
	,
	TestID -> "Product[a b, {a, b}]"
]


Test[
	RowBox[{"Product", "[", RowBox[{
		RowBox[{"a", " ", "b", " ", "c"}],
		",",
		RowBox[{"{", RowBox[{"a", ",", "b", ",", "c"}], "}"}]
	}], "]"}] // AnnotateSyntax
	,
	RowBox[{"Product", "[", RowBox[{
		RowBox[{
			SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
			" ",
			SyntaxBox["b", "UndefinedSymbol"],
			" ",
			SyntaxBox["c", "UndefinedSymbol"]
		}],
		",",
		RowBox[{"{", RowBox[{
			SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
			",",
			SyntaxBox["b", "UndefinedSymbol"],
			",",
			SyntaxBox["c", "UndefinedSymbol"]
		}], "}"}]
	}], "]"}]
	,
	TestID -> "Product[a b c, {a, b, c}]"
]


Test[
	RowBox[{"Product", "[", RowBox[{
		RowBox[{"a", " ", "b", " ", "c", " ", "d", " ", "e", " ", "f"}],
		",",
		RowBox[{"{", RowBox[{"a", ",", "b", ",", "c"}], "}"}],
		",",
		RowBox[{"{", RowBox[{"d", ",", "e", ",", "f"}], "}"}]
	}], "]"}] // AnnotateSyntax
	,
	RowBox[{"Product", "[", RowBox[{
		RowBox[{
			SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
			" ",
			SyntaxBox["b", "UndefinedSymbol"],
			" ",
			SyntaxBox["c", "UndefinedSymbol"],
			" ",
			SyntaxBox["d", "FunctionLocalVariable", "UndefinedSymbol"],
			" ",
			SyntaxBox["e", "UndefinedSymbol"],
			" ",
			SyntaxBox["f", "UndefinedSymbol"]
		}],
		",",
		RowBox[{"{", RowBox[{
			SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
			",",
			SyntaxBox["b", "UndefinedSymbol"],
			",",
			SyntaxBox["c", "UndefinedSymbol"]
		}], "}"}],
		",",
		RowBox[{"{", RowBox[{
			SyntaxBox["d", "FunctionLocalVariable", "UndefinedSymbol"],
			",",
			SyntaxBox["e", "UndefinedSymbol"],
			",",
			SyntaxBox["f", "UndefinedSymbol"]
		}], "}"}]
	}], "]"}]
	,
	TestID -> "Product[a b c d e f, {a, b, c}, {d, e, f}]"
]


(* ::Subsection:: *)
(*\[Product]*)


Test[
	Product[a, a] // MakeBoxes // AnnotateSyntax
	,
	Product[
		syntaxExpr[a, "UndefinedSymbol"],
		syntaxExpr[a, "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "\[Product]: Product[a, a]"
]


Test[
	Product[a, {a}] // MakeBoxes // AnnotateSyntax
	,
	Product[
		syntaxExpr[a, "UndefinedSymbol"],
		{syntaxExpr[a, "UndefinedSymbol"]}
	] // MakeBoxes
	,
	TestID -> "\[Product]: Product[a, {a}]"
]


Test[
	Product[a b, {a, b}] // MakeBoxes // AnnotateSyntax
	,
	Product[
		syntaxExpr[a, "UndefinedSymbol"] syntaxExpr[b, "UndefinedSymbol"],
		{syntaxExpr[a, "UndefinedSymbol"], syntaxExpr[b, "UndefinedSymbol"]}
	] // MakeBoxes
	,
	TestID -> "\[Product]: Product[a b, {a, b}]"
]


Test[
	Product[a b c, {a, b, c}] // MakeBoxes // AnnotateSyntax
	,
	Product[
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		syntaxExpr[b, "UndefinedSymbol"] *
		syntaxExpr[c, "UndefinedSymbol"]
		,
		{
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
			syntaxExpr[b, "UndefinedSymbol"],
			syntaxExpr[c, "UndefinedSymbol"]
		}
	] // MakeBoxes
	,
	TestID -> "\[Product]: Product[a b c, {a, b, c}]"
]


Test[
	Product[a b c d e f, {a, b, c}, {d, e, f}] // MakeBoxes // AnnotateSyntax
	,
	Product[
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		syntaxExpr[b, "UndefinedSymbol"] *
		syntaxExpr[c, "UndefinedSymbol"] *
		syntaxExpr[d, "FunctionLocalVariable", "UndefinedSymbol"] *
		syntaxExpr[e, "UndefinedSymbol"] *
		syntaxExpr[f, "UndefinedSymbol"]
		,
		{
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
			syntaxExpr[b, "UndefinedSymbol"],
			syntaxExpr[c, "UndefinedSymbol"]
		}
		,
		{
			syntaxExpr[d, "FunctionLocalVariable", "UndefinedSymbol"],
			syntaxExpr[e, "UndefinedSymbol"],
			syntaxExpr[f, "UndefinedSymbol"]
		}
	] // MakeBoxes
	,
	TestID -> "\[Product]: Product[a b c d e f, {a, b, c}, {d, e, f}]"
]


(* ::Subsection:: *)
(*Integrate*)


Test[
	RowBox[{"Integrate", "[", RowBox[{"a", ",", "a"}], "]"}] // AnnotateSyntax
	,
	RowBox[{"Integrate", "[", RowBox[{
		SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
		",",
		SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"]
	}], "]"}]
	,
	TestID -> "Integrate[a, a]"
]

Test[
	RowBox[{"Integrate", "[", RowBox[{
		RowBox[{"a", " ", "b"}],
		",",
		"a",
		",",
		"b"
	}], "]"}] // AnnotateSyntax
	,
	RowBox[{"Integrate", "[", RowBox[{
		RowBox[{
			SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
			" ",
			SyntaxBox["b", "FunctionLocalVariable", "UndefinedSymbol"]
		}],
		",",
		SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
		",",
		SyntaxBox["b", "FunctionLocalVariable", "UndefinedSymbol"]
	}], "]"}]
	,
	TestID -> "Integrate[a b, a, b]"
]


Test[
	RowBox[{"Integrate", "[", RowBox[{
		RowBox[{"a", " ", "b", " ", "c"}],
		RowBox[{"{", RowBox[{"a", ",", "b", ",", "c"}], "}"}]
	}], "]"}] // AnnotateSyntax
	,
	RowBox[{"Integrate", "[", RowBox[{
		RowBox[{
			SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
			" ",
			SyntaxBox["b", "UndefinedSymbol"],
			" ",
			SyntaxBox["c", "UndefinedSymbol"]
		}]
		,
		RowBox[{"{", RowBox[{
			SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
			",",
			SyntaxBox["b", "UndefinedSymbol"],
			",",
			SyntaxBox["c", "UndefinedSymbol"]
		}], "}"}]
	}], "]"}]
	,
	TestID -> "Integrate[a b c, {a, b, c}]"
]

Test[
	RowBox[{"Integrate", "[", RowBox[{
		RowBox[{"a", " ", "b", " ", "c", " ", "d", " ", "e", " ", "f"}],
		RowBox[{"{", RowBox[{"a", ",", "b", ",", "c"}], "}"}],
		RowBox[{"{", RowBox[{"d", ",", "e", ",", "f"}], "}"}]
	}], "]"}] // AnnotateSyntax
	,
	RowBox[{"Integrate", "[", RowBox[{
		RowBox[{
			SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
			" ",
			SyntaxBox["b", "UndefinedSymbol"],
			" ",
			SyntaxBox["c", "UndefinedSymbol"],
			" ",
			SyntaxBox["d", "FunctionLocalVariable", "UndefinedSymbol"],
			" ",
			SyntaxBox["e", "UndefinedSymbol"],
			" ",
			SyntaxBox["f", "UndefinedSymbol"]
		}]
		,
		RowBox[{"{", RowBox[{
			SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
			",",
			SyntaxBox["b", "UndefinedSymbol"],
			",",
			SyntaxBox["c", "UndefinedSymbol"]
		}], "}"}]
		,
		RowBox[{"{", RowBox[{
			SyntaxBox["d", "FunctionLocalVariable", "UndefinedSymbol"],
			",",
			SyntaxBox["e", "UndefinedSymbol"],
			",",
			SyntaxBox["f", "UndefinedSymbol"]
		}], "}"}]
	}], "]"}]
	,
	TestID -> "Integrate[a b c d e f, {a, b, c}, {d, e, f}]"
]


(* ::Subsection:: *)
(*\[Integral]*)


Test[
	Integrate[a, a] // MakeBoxes // AnnotateSyntax
	,
	Integrate[
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "\[Integral]: Integrate[a, a]"
]

Test[
	Integrate[a b, a, b] // MakeBoxes // AnnotateSyntax
	,
	Integrate[
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		syntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"]
		,
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
		,
		syntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"]
	] // MakeBoxes
	,
	TestID -> "\[Integral]: Integrate[a b, a, b]"
]


Test[
	Integrate[a b c, {a, b, c}] // MakeBoxes // AnnotateSyntax
	,
	Integrate[
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		syntaxExpr[b, "UndefinedSymbol"] *
		syntaxExpr[c, "UndefinedSymbol"]
		,
		{
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
			syntaxExpr[b, "UndefinedSymbol"],
			syntaxExpr[c, "UndefinedSymbol"]
		}
	] // MakeBoxes
	,
	TestID -> "\[Integral]: Integrate[a b c, {a, b, c}]"
]

Test[
	Integrate[a b c d e f, {a, b, c}, {d, e, f}] // MakeBoxes // AnnotateSyntax
	,
	Integrate[
		syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
		syntaxExpr[b, "UndefinedSymbol"] *
		syntaxExpr[c, "UndefinedSymbol"] *
		syntaxExpr[d, "FunctionLocalVariable", "UndefinedSymbol"] *
		syntaxExpr[e, "UndefinedSymbol"] *
		syntaxExpr[f, "UndefinedSymbol"]
		,
		{
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
			syntaxExpr[b, "UndefinedSymbol"],
			syntaxExpr[c, "UndefinedSymbol"]
		}
		,
		{
			syntaxExpr[d, "FunctionLocalVariable", "UndefinedSymbol"],
			syntaxExpr[e, "UndefinedSymbol"],
			syntaxExpr[f, "UndefinedSymbol"]
		}
	] // MakeBoxes
	,
	TestID -> "\[Integral]: Integrate[a b c d e f, {a, b, c}, {d, e, f}]"
]


(* ::Subsection:: *)
(*Custom function*)


Module[
	{customFunction}
	,
	SyntaxInformation[customFunction] = {
		"LocalVariables" -> {"Integrate", {2, 3}}
	};
	
	Test[
		customFunction[a] // MakeBoxes // AnnotateSyntax
		,
		customFunction[syntaxExpr[a, "UndefinedSymbol"]] // MakeBoxes
		,
		TestID -> "customFunction[a]"
	];
	
	Test[
		customFunction[a, a] // MakeBoxes // AnnotateSyntax
		,
		customFunction[
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
		] // MakeBoxes
		,
		TestID -> "customFunction[a, a]"
	];
	
	Test[
		customFunction[a b, a, b] // MakeBoxes // AnnotateSyntax
		,
		customFunction[
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			syntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"]
			,
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
			,
			syntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"]
		] // MakeBoxes
		,
		TestID -> "customFunction[a b, a, b]"
	];
	
	Test[
		customFunction[a b c, a, b, c] // MakeBoxes // AnnotateSyntax
		,
		customFunction[
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			syntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"] *
			syntaxExpr[c, "UndefinedSymbol"]
			,
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"]
			,
			syntaxExpr[b, "FunctionLocalVariable", "UndefinedSymbol"]
			,
			syntaxExpr[c, "UndefinedSymbol"]
		] // MakeBoxes
		,
		TestID -> "customFunction[a b c, a, b, c]"
	];
	
	Test[
		customFunction[a b c, {a, b, c}] // MakeBoxes // AnnotateSyntax
		,
		customFunction[
			syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"] *
			syntaxExpr[b, "UndefinedSymbol"] *
			syntaxExpr[c, "UndefinedSymbol"]
			,
			{
				syntaxExpr[a, "FunctionLocalVariable", "UndefinedSymbol"],
				syntaxExpr[b, "UndefinedSymbol"],
				syntaxExpr[c, "UndefinedSymbol"]
			}
		] // MakeBoxes
		,
		TestID -> "customFunction[a b c, {a, b, c}]"
	]
]


Module[
	{customFunctionNoSI}
	,
	Test[
		customFunctionNoSI[a, b] // MakeBoxes // AnnotateSyntax
		,
		customFunctionNoSI[
			syntaxExpr[a, "UndefinedSymbol"],
			syntaxExpr[b, "UndefinedSymbol"]
		] // MakeBoxes
		,
		TestID -> "customFunctionNoSI[a, b]"
	]
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
