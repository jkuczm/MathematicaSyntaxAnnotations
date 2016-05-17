(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["SyntaxAnnotations`Tests`Integration`Comments`", {"MUnit`"}]


Get["SyntaxAnnotations`Tests`Integration`init`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Comment only*)


Test[
	RowBox[{"(*", "word", "*)"}] // AnnotateSyntax
	,
	SyntaxBox[RowBox[{"(*", "word", "*)"}], "Comment"]
	,
	TestID -> "Comment only: single word"
]

Test[
	RowBox[{"(*", " ", "text", "   ", "\[IndentingNewLine]", "  \t", "*)"}] //
		AnnotateSyntax
	,
	SyntaxBox[
		RowBox[{"(*", " ", "text", "   ", "\[IndentingNewLine]", "  \t", "*)"}]
		,
		"Comment"
	]
	,
	TestID -> "Comment only: single word + whitespace"
]

Test[
	RowBox[{"(*", RowBox[{
		"comment", " ", "text", "\n",
		"with", " ", "many", " ", "words"
	}], " ", "*)"}] // AnnotateSyntax
	,
	SyntaxBox[
		RowBox[{"(*", RowBox[{
			"comment", " ", "text", "\n",
			"with", " ", "many", " ", "words"
		}], " ", "*)"}]
		,
		"Comment"
	]
	,
	TestID -> "Comment only: multiple words"
]


Test[
	RowBox[{"(*", "a", RowBox[{"(*", "b", "*)"}], "*)"}] // AnnotateSyntax
	,
	SyntaxBox[RowBox[{"(*", "a", RowBox[{"(*", "b", "*)"}], "*)"}], "Comment"]
	,
	TestID -> "Comment only: nested"
]


Test[
	RowBox[{"(*", "\"\<string in comment\>\"", "*)"}] // AnnotateSyntax
	,
	SyntaxBox[RowBox[{"(*", "\"\<string in comment\>\"", "*)"}], "Comment"]
	,
	TestID -> "Comment only: String in comment"
]


(* ::Subsection:: *)
(*Mixed*)


Test[
	RowBox[{
		RowBox[{"(*", "a", "*)"}],
		"\n",
		" ",
		RowBox[{"(*", "*)"}],
		"\t"
	}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox[RowBox[{"(*", "a", "*)"}], "Comment"],
		"\n",
		" ",
		SyntaxBox[RowBox[{"(*", "*)"}], "Comment"],
		"\t"
	}]
	,
	TestID -> "Mixed: comments, whitespace"
]


Test[
	RowBox[{
		RowBox[{"(*", " ", "a", "*)"}],
		RowBox[{
			"a_",
			RowBox[{"(*", "*)"}],
			"\[RuleDelayed]",
			RowBox[{"(*", "\[IndentingNewLine]", "*)"}],
			"a",
			RowBox[{"(*", "word", "*)"}]
		}]
	}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox[RowBox[{"(*", " ", "a", "*)"}], "Comment"],
		RowBox[{
			SyntaxBox["a_", "PatternVariable"],
			SyntaxBox[RowBox[{"(*", "*)"}], "Comment"],
			"\[RuleDelayed]",
			SyntaxBox[RowBox[{"(*", "\[IndentingNewLine]", "*)"}], "Comment"],
			SyntaxBox["a", "PatternVariable", "UndefinedSymbol"],
			SyntaxBox[RowBox[{"(*", "word", "*)"}], "Comment"]
		}]
	}]
	,
	TestID -> "Mixed: a_ \\[RuleDelayed] a"
]

Test[
	RowBox[{
		RowBox[{"(*", "*)"}],
		RowBox[{
			"a",
			RowBox[{"(*", RowBox[{"some", " ", "text"}], " ", "*)"}],
			"/:",
			RowBox[{"(*", " ", "a", " ", "*)"}],
			"a",
			" ",
			RowBox[{"(*", "b", "*)"}],
			"\t",
			"=",
			RowBox[{"(*", " ", "*)"}],
			"a", " "
		}]
	}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox[RowBox[{"(*", "*)"}], "Comment"],
		RowBox[{
			SyntaxBox["a", "UndefinedSymbol"],
			SyntaxBox[
				RowBox[{"(*", RowBox[{"some", " ", "text"}], " ", "*)"}],
				"Comment"
			],
			"/:",
			SyntaxBox[RowBox[{"(*", " ", "a", " ", "*)"}], "Comment"],
			SyntaxBox["a", "UndefinedSymbol"],
			" ",
			SyntaxBox[RowBox[{"(*", "b", "*)"}], "Comment"],
			"\t",
			"=",
			SyntaxBox[RowBox[{"(*", " ", "*)"}], "Comment"],
			SyntaxBox["a", "UndefinedSymbol"],
			" "
		}]
	}]
	,
	TestID -> "Mixed: a /: a = a"
]

Test[
	RowBox[{
		" ",
		RowBox[{
			"Block",
			" ",
			RowBox[{
				"(*", RowBox[{"multiple", RowBox[{" ", "\n"}], "words"}], "*)"
			}],
			"[",
			RowBox[{"(*", "c", "*)"}],
			RowBox[{
				RowBox[{
					"{",
					RowBox[{"(*", "*)"}],
					" ",
					RowBox[{"(*", "word", " ", "*)"}],
					RowBox[{
						"a",
						"\t",
						RowBox[{"(*", "x", "*)"}],
						":=",
						RowBox[{"(*", "*)"}],
						"b"
					}],
					RowBox[{"(*", RowBox[{"(*", "*)"}], "*)"}],
					"}"
				}],
				RowBox[{"(*", " ", "a", " ", "*)"}],
				" ",
				",",
				RowBox[{
					"Module",
					"  ",
					"[",
					RowBox[{"(*", "   ", "*)"}],
					RowBox[{
						RowBox[{"{", "c", RowBox[{"(*", "c", "*)"}], "}"}],
						RowBox[{"(*", RowBox[{"a", " ", "b"}], "*)"}],
						",",
						RowBox[{
							"(*",
							" ",
							RowBox[{"c", "\[IndentingNewLine]", "a"}],
							" ",
							"*)"
						}],
						RowBox[{"a", " ", "b", " ", "c"}]
					}],
					"]"
				}]
			}],
			"]"
		}]
	}] // AnnotateSyntax
	,
	RowBox[{
		" ",
		RowBox[{
			"Block",
			" ",
			SyntaxBox[
				RowBox[{
					"(*",
					RowBox[{"multiple", RowBox[{" ", "\n"}], "words"}],
					"*)"
				}],
				"Comment"
			],
			"[",
			SyntaxBox[RowBox[{"(*", "c", "*)"}], "Comment"],
			RowBox[{
				RowBox[{
					"{",
					SyntaxBox[RowBox[{"(*", "*)"}], "Comment"],
					" ",
					SyntaxBox[RowBox[{"(*", "word", " ", "*)"}], "Comment"],
					RowBox[{
						SyntaxBox[
							"a",
							"FunctionLocalVariable", "UndefinedSymbol"
						],
						"\t",
						SyntaxBox[RowBox[{"(*", "x", "*)"}], "Comment"],
						":=",
						SyntaxBox[RowBox[{"(*", "*)"}], "Comment"],
						SyntaxBox["b", "UndefinedSymbol"]
					}],
					SyntaxBox[
						RowBox[{"(*", RowBox[{"(*", "*)"}], "*)"}],
						"Comment"
					],
					"}"
				}],
				SyntaxBox[RowBox[{"(*", " ", "a", " ", "*)"}], "Comment"],
				" ",
				",",
				RowBox[{
					"Module",
					"  ",
					"[",
					SyntaxBox[RowBox[{"(*", "   ", "*)"}], "Comment"],
					RowBox[{
						RowBox[{
							"{",
							SyntaxBox["c", "LocalVariable", "UndefinedSymbol"],
							SyntaxBox[RowBox[{"(*", "c", "*)"}], "Comment"],
							"}"
						}],
						SyntaxBox[
							RowBox[{"(*", RowBox[{"a", " ", "b"}], "*)"}],
							"Comment"
						],
						",",
						SyntaxBox[
							RowBox[{
								"(*",
								" ",
								RowBox[{"c", "\[IndentingNewLine]", "a"}],
								" ",
								"*)"
							}],
							"Comment"
						],
						RowBox[{
							SyntaxBox[
								"a",
								"FunctionLocalVariable", "UndefinedSymbol"
							],
							" ",
							SyntaxBox["b", "UndefinedSymbol"],
							" ",
							SyntaxBox["c", "LocalVariable", "UndefinedSymbol"]
						}]
					}],
					"]"
				}]
			}],
			"]"
		}]
	}]
	,
	TestID -> "Mixed: Block[{a := b}, Module[{c}, a b c]"
]

Test[
	RowBox[{
		RowBox[{"(*", "x", "*)"}],
		RowBox[{
			RowBox[{"#1", RowBox[{"(*", "#1", "*)"}], "+", "#2"}],
			RowBox[{"(*", RowBox[{"(*", "*)"}], "*)"}],
			"\[IndentingNewLine]",
			"&"
		}]
	}] // AnnotateSyntax
	,
	RowBox[{
		SyntaxBox[RowBox[{"(*", "x", "*)"}], "Comment"],
		RowBox[{
			RowBox[{
				SyntaxBox["#1", "PatternVariable"],
				SyntaxBox[RowBox[{"(*", "#1", "*)"}], "Comment"],
				"+",
				SyntaxBox["#2", "PatternVariable"]
			}],
			SyntaxBox[RowBox[{"(*", RowBox[{"(*", "*)"}], "*)"}], "Comment"],
			"\[IndentingNewLine]",
			"&"
		}]
	}]
	,
	TestID -> "Mixed: #1 + #2 &"
]

Test[
	RowBox[{"Function", "[",
		RowBox[{"(*", "text", "*)"}],
		RowBox[{
			RowBox[{"{",
				RowBox[{"(*", "    ", "*)"}],
				"x",
				RowBox[{"(*", RowBox[{",", "y"}], "*)"}],
			"}"}],
			RowBox[{"(*", "a", " ", "*)"}], ",",
			RowBox[{"(*", "*)"}],
			RowBox[{"x", "  ", "y"}],
			RowBox[{"(*", RowBox[{"x", " ", "y"}], "*)"}], ",", "\n",
			RowBox[{"(*", "com", RowBox[{"(*", "ment", "*)"}], "*)"}],
			"HoldFirst"
		}],
		RowBox[{"(*", "*)"}],
	"]"}] // AnnotateSyntax
	,
	RowBox[{"Function", "[",
		SyntaxBox[RowBox[{"(*", "text", "*)"}], "Comment"],
		RowBox[{
			RowBox[{"{",
				SyntaxBox[RowBox[{"(*", "    ", "*)"}], "Comment"],
				SyntaxBox["x", "PatternVariable", "UndefinedSymbol"],
				SyntaxBox[RowBox[{"(*", RowBox[{",", "y"}], "*)"}], "Comment"],
			"}"}],
			SyntaxBox[RowBox[{"(*", "a", " ", "*)"}], "Comment"], ",",
			SyntaxBox[RowBox[{"(*", "*)"}], "Comment"],
			RowBox[{
				SyntaxBox["x", "PatternVariable", "UndefinedSymbol"],
				"  ",
				SyntaxBox["y", "UndefinedSymbol"]
			}],
			SyntaxBox[
				RowBox[{"(*", RowBox[{"x", " ", "y"}], "*)"}],
				"Comment"
			], ",", "\n",
			SyntaxBox[
				RowBox[{"(*", "com", RowBox[{"(*", "ment", "*)"}], "*)"}],
				"Comment"
			],
			"HoldFirst"
		}],
		SyntaxBox[RowBox[{"(*", "*)"}], "Comment"],
	"]"}]
	,
	TestID -> "Mixed: Function[{x}, x  y, HoldFirst]"
]

Test[
	RowBox[{"Limit", RowBox[{"(*", "Table", "*)"}], "[",
		RowBox[{"(*", "*)"}],
		RowBox[{
			RowBox[{"a", " ", "b", " ", "c", " ", "d"}],
			RowBox[{"(*", "*)"}], " ", "\[IndentingNewLine]",
			",",
			RowBox[{"(*", "a", "*)"}], " ",
			RowBox[{
				"b",
				"->", "\t",
				RowBox[{"(*", "\n", "*)"}],
				"c"
			}]
		}],
		RowBox[{"(*", "d", "*)"}],
	"]"}] // AnnotateSyntax
	,
	RowBox[{
		"Limit",
		SyntaxBox[RowBox[{"(*", "Table", "*)"}], "Comment"],
		"[",
		SyntaxBox[RowBox[{"(*", "*)"}], "Comment"],
		RowBox[{
			RowBox[{
				SyntaxBox["a", "UndefinedSymbol"],
				" ",
				SyntaxBox["b", "FunctionLocalVariable", "UndefinedSymbol"],
				" ",
				SyntaxBox["c", "UndefinedSymbol"],
				" ",
				SyntaxBox["d", "UndefinedSymbol"]
			}],
			SyntaxBox[RowBox[{"(*", "*)"}], "Comment"],
			" ", "\[IndentingNewLine]",
			",",
			SyntaxBox[RowBox[{"(*", "a", "*)"}], "Comment"], " ",
			RowBox[{
				SyntaxBox["b", "FunctionLocalVariable", "UndefinedSymbol"],
				"->", "\t",
				SyntaxBox[RowBox[{"(*", "\n", "*)"}], "Comment"],
				SyntaxBox["c", "UndefinedSymbol"]
			}]
		}],
		SyntaxBox[RowBox[{"(*", "d", "*)"}], "Comment"],
		"]"
	}]
	,
	TestID -> "Mixed: Limit[a b c d, b -> c]"
]

Test[
	RowBox[{
		UnderoverscriptBox[
			"\[Product]",
			RowBox[{
				"a",
				RowBox[{"(*", "y", "*)"}],
				"=",
				RowBox[{"(*", "z", "*)"}],
				"b",
				RowBox[{"(*", "t", "*)"}]
			}],
			RowBox[{
				RowBox[{"(*", "*)"}],
				"c",
				RowBox[{"(*", "x", "*)"}]
			}]
		],
		"   ",
		RowBox[{"(*", RowBox[{"a", " ", "b", " ", "c"}], "*)"}],
		RowBox[{
			"a",
			" ",
			"b",
			" ",
			RowBox[{"(*", "\[IndentingNewLine]", "*)"}],
			"c",
			" ",
			"a"
		}]
	}] // AnnotateSyntax
	,
	RowBox[{
		UnderoverscriptBox[
			"\[Product]",
			RowBox[{
				SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
				SyntaxBox[RowBox[{"(*", "y", "*)"}], "Comment"],
				"=",
				SyntaxBox[RowBox[{"(*", "z", "*)"}], "Comment"],
				SyntaxBox["b", "UndefinedSymbol"],
				SyntaxBox[RowBox[{"(*", "t", "*)"}], "Comment"]
			}],
			RowBox[{
				SyntaxBox[RowBox[{"(*", "*)"}], "Comment"],
				SyntaxBox["c", "UndefinedSymbol"],
				SyntaxBox[RowBox[{"(*", "x", "*)"}], "Comment"]
			}]
		],
		"   ",
		SyntaxBox[
			RowBox[{"(*", RowBox[{"a", " ", "b", " ", "c"}], "*)"}],
			"Comment"
		],
		RowBox[{
			SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"],
			" ",
			SyntaxBox["b", "UndefinedSymbol"],
			" ",
			SyntaxBox[RowBox[{"(*", "\[IndentingNewLine]", "*)"}], "Comment"],
			SyntaxBox["c", "UndefinedSymbol"],
			" ",
			SyntaxBox["a", "FunctionLocalVariable", "UndefinedSymbol"]
		}]
	}]
	,
	TestID -> "Mixed: Product[a b c a, {a, b, c}] (\[Product])"
]


Test[
	"\"\<(*comment*) in string\>\"" // AnnotateSyntax
	,
	SyntaxBox["\"\<(*comment*) in string\>\"", "String"]
	,
	TestID -> "Mixed: comment in String"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
