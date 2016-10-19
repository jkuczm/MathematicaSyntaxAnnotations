(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["SyntaxAnnotations`Tests`Integration`Patterns`", {"MUnit`"}]


Get["SyntaxAnnotations`Tests`Integration`init`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Rule*)


Test[
	AnnotateSyntax @ MakeBoxes[a -> a]
	,
	MakeBoxes[
		SyntaxExpr[a, "UndefinedSymbol"] -> SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a -> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ -> a]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] -> SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a_ -> a"
]
Test[
	AnnotateSyntax @ MakeBoxes[a\[UnderBracket]b_ -> a\[UnderBracket]b]
	,
	MakeBoxes[
		SyntaxExpr[a\[UnderBracket]b_, "PatternVariable"] ->
			SyntaxExpr[a\[UnderBracket]b, "UndefinedSymbol"]
	]
	,
	TestID -> "a\\[UnderBracket]b_ -> a\\[UnderBracket]b"
]


Test[
	AnnotateSyntax @ MakeBoxes[a__ -> a]
	,
	MakeBoxes[
		SyntaxExpr[a__, "PatternVariable"] -> SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a__ -> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a___ -> a]
	,
	MakeBoxes[
		SyntaxExpr[a___, "PatternVariable"] -> SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a___ -> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[_a -> a]
	,
	MakeBoxes[_a -> SyntaxExpr[a, "UndefinedSymbol"]]
	,
	TestID -> "_a -> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[__a -> a]
	,
	MakeBoxes[__a -> SyntaxExpr[a, "UndefinedSymbol"]]
	,
	TestID -> "__a -> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[___a -> a]
	,
	MakeBoxes[___a -> SyntaxExpr[a, "UndefinedSymbol"]]
	,
	TestID -> "___a -> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_b -> a b]
	,
	MakeBoxes[
		SyntaxExpr[a_b, "PatternVariable"] ->
			SyntaxExpr[a, "UndefinedSymbol"] SyntaxExpr[b, "UndefinedSymbol"]
	]
	,
	TestID -> "a_b -> a b"
]


Test[
	AnnotateSyntax @ MakeBoxes[a -> a_]
	,
	MakeBoxes[SyntaxExpr[a, "UndefinedSymbol"] -> a_]
	,
	TestID -> "a -> a_"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ \[Rule] a]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] -> SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a_ \\[Rule] a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ b_ -> a b]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] SyntaxExpr[b_, "PatternVariable"] ->
			SyntaxExpr[a, "UndefinedSymbol"] SyntaxExpr[b, "UndefinedSymbol"]
	]
	,
	TestID -> "a_ b_ -> a b"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ a -> a]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] SyntaxExpr[a, "UndefinedSymbol"] ->
			SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a_ a -> a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a:a -> a]
	,
	MakeBoxes[
		RawBoxes@RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			":",
			SyntaxBox["a", "UndefinedSymbol"]
		}] ->
			SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a:a -> a"
]
Test[
	AnnotateSyntax @ MakeBoxes[a:b -> a b]
	,
	MakeBoxes[
		RawBoxes@RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			":",
			SyntaxBox["b", "UndefinedSymbol"]
		}] ->
			SyntaxExpr[a, "UndefinedSymbol"] SyntaxExpr[b, "UndefinedSymbol"]
	]
	,
	TestID -> "a:b -> a b"
]

Test[
	AnnotateSyntax @ MakeBoxes[a_:a -> a]
	,
	MakeBoxes[
		RawBoxes@RowBox[{
			SyntaxBox["a_", "PatternVariable"],
			":",
			SyntaxBox["a", "UndefinedSymbol"]
		}] ->
			SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a_:a -> a"
]
Test[
	AnnotateSyntax @ MakeBoxes[a__:b -> a b]
	,
	MakeBoxes[
		RawBoxes@RowBox[{
			SyntaxBox["a__", "PatternVariable"],
			":",
			SyntaxBox["b", "UndefinedSymbol"]
		}] ->
			SyntaxExpr[a, "UndefinedSymbol"] SyntaxExpr[b, "UndefinedSymbol"]
	]
	,
	TestID -> "a__:b -> a b"
]
Test[
	AnnotateSyntax @ MakeBoxes[a___:a -> a]
	,
	MakeBoxes[
		RawBoxes@RowBox[{
			SyntaxBox["a___", "PatternVariable"],
			":",
			SyntaxBox["a", "UndefinedSymbol"]
		}] ->
			SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a___:a -> a"
]

Test[
	AnnotateSyntax @ MakeBoxes[_a:b -> a b]
	,
	MakeBoxes[
		RawBoxes@RowBox[{
			SyntaxBox["_a", "PatternVariable"],
			":",
			SyntaxBox["b", "UndefinedSymbol"]
		}] ->
			SyntaxExpr[a, "UndefinedSymbol"] SyntaxExpr[b, "UndefinedSymbol"]
	]
	,
	TestID -> "_a:b -> a b"
]
Test[
	AnnotateSyntax @ MakeBoxes[__a:a -> a]
	,
	MakeBoxes[
		RawBoxes@RowBox[{
			SyntaxBox["__a", "PatternVariable"],
			":",
			SyntaxBox["a", "UndefinedSymbol"]
		}] ->
			SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "__a:a -> a"
]
Test[
	AnnotateSyntax @ MakeBoxes[___a:b -> a b]
	,
	MakeBoxes[
		RawBoxes@RowBox[{
			SyntaxBox["___a", "PatternVariable"],
			":",
			SyntaxBox["b", "UndefinedSymbol"]
		}] ->
			SyntaxExpr[a, "UndefinedSymbol"] SyntaxExpr[b, "UndefinedSymbol"]
	]
	,
	TestID -> "___a:b -> a b"
]

Test[
	AnnotateSyntax @ MakeBoxes[a_b:c -> a b c]
	,
	MakeBoxes[
		RawBoxes@RowBox[{
			SyntaxBox["a_b", "PatternVariable"],
			":",
			SyntaxBox["c", "UndefinedSymbol"]
		}] ->
			SyntaxExpr[a, "UndefinedSymbol"] *
			SyntaxExpr[b, "UndefinedSymbol"] *
			SyntaxExpr[c, "UndefinedSymbol"]
	]
	,
	TestID -> "a_b:c -> a b c"
]


Test[
	AnnotateSyntax @ MakeBoxes[a:a:a -> a]
	,
	MakeBoxes[
		RawBoxes@RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			":",
			SyntaxBox["a", "UndefinedSymbol"],
			":",
			SyntaxBox["a", "UndefinedSymbol"]
		}] ->
			SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a:a:a -> a"
]
Test[
	AnnotateSyntax @ MakeBoxes[a:b:c -> a b c]
	,
	MakeBoxes[
		RawBoxes@RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			":",
			SyntaxBox["b", "UndefinedSymbol"],
			":",
			SyntaxBox["c", "UndefinedSymbol"]
		}] ->
			SyntaxExpr[a, "UndefinedSymbol"] *
			SyntaxExpr[b, "UndefinedSymbol"] *
			SyntaxExpr[c, "UndefinedSymbol"]
	]
	,
	TestID -> "a:b:c -> a b c"
]


(* ::Subsection:: *)
(*Set*)


Test[
	AnnotateSyntax @ MakeBoxes[a = a]
	,
	MakeBoxes[
		SyntaxExpr[a, "UndefinedSymbol"] = SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a = a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ = a]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] = SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a_ = a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a = a_]
	,
	MakeBoxes[SyntaxExpr[a, "UndefinedSymbol"] = a_]
	,
	TestID -> "a = a_"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ b_ = a b]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] SyntaxExpr[b_, "PatternVariable"] =
			SyntaxExpr[a, "UndefinedSymbol"] SyntaxExpr[b, "UndefinedSymbol"]
	]
	,
	TestID -> "a_ b_ = a b"
]


Test[
	AnnotateSyntax @ MakeBoxes[a:a = a]
	,
	MakeBoxes[
		RawBoxes@RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			":",
			SyntaxBox["a", "UndefinedSymbol"]
		}] =
			SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a:a = a"
]
Test[
	AnnotateSyntax @ MakeBoxes[a:b = a b]
	,
	MakeBoxes[
		RawBoxes@RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			":",
			SyntaxBox["b", "UndefinedSymbol"]
		}] =
			SyntaxExpr[a, "UndefinedSymbol"] SyntaxExpr[b, "UndefinedSymbol"]
	]
	,
	TestID -> "a:b = a b"
]

Test[
	AnnotateSyntax @ MakeBoxes[a:a:a = a]
	,
	MakeBoxes[
		RawBoxes@RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			":",
			SyntaxBox["a", "UndefinedSymbol"],
			":",
			SyntaxBox["a", "UndefinedSymbol"]
		}] =
			SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a:a:a = a"
]
Test[
	AnnotateSyntax @ MakeBoxes[a:b:c = a b c]
	,
	MakeBoxes[
		RawBoxes@RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			":",
			SyntaxBox["b", "UndefinedSymbol"],
			":",
			SyntaxBox["c", "UndefinedSymbol"]
		}] =
			SyntaxExpr[a, "UndefinedSymbol"] *
			SyntaxExpr[b, "UndefinedSymbol"] *
			SyntaxExpr[c, "UndefinedSymbol"]
	]
	,
	TestID -> "a:b:c = a b c"
]


(* ::Subsection:: *)
(*UpSet*)


Test[
	AnnotateSyntax @ MakeBoxes[a ^= a]
	,
	MakeBoxes[
		SyntaxExpr[a, "UndefinedSymbol"] ^= SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a ^= a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ ^= a]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] ^= SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a_ ^= a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a ^= a_]
	,
	MakeBoxes[SyntaxExpr[a, "UndefinedSymbol"] ^= a_]
	,
	TestID -> "a ^= a_"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ b_ ^= a b]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] SyntaxExpr[b_, "PatternVariable"] ^=
			SyntaxExpr[a, "UndefinedSymbol"] SyntaxExpr[b, "UndefinedSymbol"]
	]
	,
	TestID -> "a_ b_ ^= a b"
]


Test[
	AnnotateSyntax @ MakeBoxes[a:a ^= a]
	,
	MakeBoxes[
		RawBoxes@RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			":",
			SyntaxBox["a", "UndefinedSymbol"]
		}] ^=
			SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a:a ^= a"
]
Test[
	AnnotateSyntax @ MakeBoxes[a:b ^= a b]
	,
	MakeBoxes[
		RawBoxes@RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			":",
			SyntaxBox["b", "UndefinedSymbol"]
		}] ^=
			SyntaxExpr[a, "UndefinedSymbol"] SyntaxExpr[b, "UndefinedSymbol"]
	]
	,
	TestID -> "a:b ^= a b"
]

Test[
	AnnotateSyntax @ MakeBoxes[a:a:a ^= a]
	,
	MakeBoxes[
		RawBoxes@RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			":",
			SyntaxBox["a", "UndefinedSymbol"],
			":",
			SyntaxBox["a", "UndefinedSymbol"]
		}] ^=
			SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a:a:a ^= a"
]
Test[
	AnnotateSyntax @ MakeBoxes[a:b:c ^= a b c]
	,
	MakeBoxes[
		RawBoxes@RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			":",
			SyntaxBox["b", "UndefinedSymbol"],
			":",
			SyntaxBox["c", "UndefinedSymbol"]
		}] ^=
			SyntaxExpr[a, "UndefinedSymbol"] *
			SyntaxExpr[b, "UndefinedSymbol"] *
			SyntaxExpr[c, "UndefinedSymbol"]
	]
	,
	TestID -> "a:b:c ^= a b c"
]


(* ::Subsection:: *)
(*TagSet*)


Test[
	AnnotateSyntax @ MakeBoxes[a /: a = a]
	,
	MakeBoxes[
		SyntaxExpr[a, "UndefinedSymbol"] /: SyntaxExpr[a, "UndefinedSymbol"] =
			SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a /: a = a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ /: a = a]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] /: SyntaxExpr[a, "UndefinedSymbol"] =
			SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a_ /: a = a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a /: a_ = a]
	,
	MakeBoxes[
		SyntaxExpr[a, "UndefinedSymbol"] /: SyntaxExpr[a_, "PatternVariable"] =
			SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a /: a_ = a"
]


Test[
	AnnotateSyntax @ MakeBoxes[a /: a = a_]
	,
	MakeBoxes[
		SyntaxExpr[a, "UndefinedSymbol"] /: SyntaxExpr[a, "UndefinedSymbol"] =
			a_
	]
	,
	TestID -> "a /: a = a_"
]


Test[
	AnnotateSyntax @ MakeBoxes[a_ b_ /: a b = a b]
	,
	MakeBoxes[
		SyntaxExpr[a_, "PatternVariable"] SyntaxExpr[b_, "PatternVariable"] /:
			SyntaxExpr[a, "UndefinedSymbol"] SyntaxExpr[b, "UndefinedSymbol"] =
				SyntaxExpr[a, "UndefinedSymbol"] *
				SyntaxExpr[b, "UndefinedSymbol"]
	]
	,
	TestID -> "a_ b_ /: a b = a b"
]


Test[
	AnnotateSyntax @ MakeBoxes[a b /: a_ b_ = a b]
	,
	MakeBoxes[
		SyntaxExpr[a, "UndefinedSymbol"] SyntaxExpr[b, "UndefinedSymbol"] /:
			SyntaxExpr[a_, "PatternVariable"] *
			SyntaxExpr[b_, "PatternVariable"] =
				SyntaxExpr[a, "UndefinedSymbol"] *
				SyntaxExpr[b, "UndefinedSymbol"]
	]
	,
	TestID -> "a b /: a_ b_ = a b"
]


Test[
	AnnotateSyntax @ MakeBoxes[a b /: a b = a_ b_]
	,
	MakeBoxes[
		SyntaxExpr[a, "UndefinedSymbol"] SyntaxExpr[b, "UndefinedSymbol"] /:
			SyntaxExpr[a, "UndefinedSymbol"] SyntaxExpr[b, "UndefinedSymbol"] =
				a_ b_
	]
	,
	TestID -> "a b /: a b = a_ b_"
]


Test[
	AnnotateSyntax @ MakeBoxes[a:a /: a = a]
	,
	MakeBoxes[
		RawBoxes@RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			":",
			SyntaxBox["a", "UndefinedSymbol"]
		}] /:
			SyntaxExpr[a, "UndefinedSymbol"] =
				SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a:a /: a = a"
]
Test[
	AnnotateSyntax @ MakeBoxes[a /: b:c = a b c]
	,
	MakeBoxes[
		SyntaxExpr[a, "UndefinedSymbol"] /: 
			RawBoxes@RowBox[{
				SyntaxBox["b", "PatternVariable", "UndefinedSymbol"],
				":",
				SyntaxBox["c", "UndefinedSymbol"]
			}] =
				SyntaxExpr[a, "UndefinedSymbol"] *
				SyntaxExpr[b, "UndefinedSymbol"] *
				SyntaxExpr[c, "UndefinedSymbol"]
	]
	,
	TestID -> "a /: b:c = a b c"
]

Test[
	AnnotateSyntax @ MakeBoxes[a /: a:a:a = a]
	,
	MakeBoxes[
		SyntaxExpr[a, "UndefinedSymbol"] /: 
			RawBoxes@RowBox[{
				SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
				":",
				SyntaxBox["a", "UndefinedSymbol"],
				":",
				SyntaxBox["a", "UndefinedSymbol"]
			}] =
				SyntaxExpr[a, "UndefinedSymbol"]
	]
	,
	TestID -> "a /: a:a:a = a"
]
Test[
	AnnotateSyntax @ MakeBoxes[a:b:c /: d = a b c d]
	,
	MakeBoxes[
		RawBoxes@RowBox[{
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			":",
			SyntaxBox["b", "UndefinedSymbol"],
			":",
			SyntaxBox["c", "UndefinedSymbol"]
		}] /:
			SyntaxExpr[d, "UndefinedSymbol"] =
				SyntaxExpr[a, "UndefinedSymbol"] *
				SyntaxExpr[b, "UndefinedSymbol"] *
				SyntaxExpr[c, "UndefinedSymbol"] *
				SyntaxExpr[d, "UndefinedSymbol"]
	]
	,
	TestID -> "a:b:c /: d = a b c d"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
