﻿(* ::Package:: *)

BeginPackage["SyntaxAnnotations`"]


Unprotect["`*"]
ClearAll["`*"]


(* ::Section:: *)
(*Usage messages*)


AnnotateSyntax::usage =
"\
AnnotateSyntax[boxes] \
returns boxes with certain subboxes wrapped with boxes identifying their \
syntactic role."


SyntaxBox::usage =
"\
SyntaxBox[boxes, {type1, subtype1, ...}, {type2, subtype2, ...}, ...] \
represents boxes that in an expression perform sytnax roles of given types."


NormalizeAnnotationTypes::usage =
"\
NormalizeAnnotationTypes[{type1, subtype1, ...}, {type2, subtype2, ...}, ...] \
returns List of strings with names of dominating annotation types from given \
sequence of annotation types."


$BoxesToAnnotationTypes::usage =
"\
$BoxesToAnnotationTypes \
is List of default rules converting boxes to annotation types."


$SyntaxBoxToStyleBox::usage =
"\
$SyntaxBoxToStyleBox \
is List of default rules used to transform syntax boxes to style boxes."


(* ::Section:: *)
(*Implementation*)


Begin["`Private`"]


ClearAll["`*"]


(* ::Subsection:: *)
(*Private symbols usage*)


undefinedSymbolQ::usage =
"\
undefinedSymbolQ[symbol] \
returns True if symbol is undefined. Returns False otherwise.\

undefinedSymbolQ[\"name\"] \
returns True if symbol, with given \"name\", is undefined. Returns False \
otherwise."


symbolNameQ::usage =
"\
symbolNameQ[\"str\"] \
returns True if given String is valid symbol name, returns False otherwise."


whitespaceQ::usage =
"\
whitespaceQ[\"str1\", \"str2\", ...] \
returns True if given strings are empty or contain only whitespace or \
\\[IndentingNewLine] characters, returns False otherwise."


extractSymbolName::usage =
"\
extractSymbolName[\"str\"] \
returns pattern matching name of symbol contained in given string. \
For \"str\" being valid symbol name returned pattern matches this name. \
For box representation of Blank... pattern with head, returned pattern \
matches this head. \
For box representation of pattern with name, returned pattern matches this \
name."


extractLocalVariableNames::usage =
"\
extractLocalVariableNames[\"type\"][boxes1, boxes2, ...] \
returns pattern matching names of all local symbols extracted from given \
boxes. \
\"type\" can be any of types from \"LocalVariables\" property of \
SyntaxInformation, with addition of: \
\"Scoping\" type, applicable to With, Module and Block; \
\"LowerBound\" applicable to \[Sum] and \[Product]; \
\"IntegrateDifferentialD\" applicable to \[Integral]; \
\"PatternName\" for rules and set expressions. \
boxesi are treated as box representation of function arguments, they don't \
need to represent valid mathematica expressions."


extractArgs::usage =
"\
extractArgs[boxes, {imin, imax}] \
returns List of box representations of function arguments, with positions \
from imin to imax, extracted from boxes treated as representation of sequence \
of arguments.\

extractArgs[boxes, {i}] \
returns List containing one element: box representation of function argument, \
with position i.\

extractArgs[boxes, 0] \
returns List containing given boxes.\

boxes don't need to represent valid mathematica expression. \
Second argument of extractArgs accepts same values as second element of \
\"LocalVariables\" property of SyntaxInformation with addition of 0. \
All occurrences of SyntaxBox are stripped."


extendedSyntaxInformation::usage =
"\
extendedSyntaxInformation[\"name\"] \
returns SyntaxInformation for symbol with given \"name\". \
extendedSyntaxInformation for some built-in symbols is enriched with \
properties that are missing from ordinary SyntaxInformation, yet equivalent \
functionality is implemented by other means.\

extendedSyntaxInformation[\"op1\", \"op2\"] \
returns SyntaxInformation for operators composed of two parts."


withLocalVariables::usage =
"\
withLocalVariables[funcName, localVarsPattSym, argumentBoxes, body] \
when used on right hand side of SetDelayed assigns to left hand side given \
body wrapped with condition that causes definition to apply only when given \
funcName has defined extendedSyntaxInformation with \"LocalVariables\" \
element. body can contain symbol given as localVarsPattSym and it will be \
replaced by pattern matching box representation of local variables extracted \
from argumentBoxes."


annotateSyntaxInternal::usage =
"\
annotateSyntaxInternal[boxes, rules] \
returns boxes with certain subboxes wrapped with SyntaxBox identifying their \
syntactic role. Uses given rules as basis for assignment of syntactic roles \
to subboxes."


posExprPosition::usage =
"\
posExprPosition[posExpr] \
returns position encoded in given \"position expression\" posExpr.\

posExprPosition[posExpr, i] \
returns position encoded in posExpr with last i elements droped."


(* ::Subsection:: *)
(*operator groups*)


$patternOperators = "=" | "^=" | "->" | "\[Rule]"

$patternDelayedOperators = ":=" | "^:=" | ":>" | "\[RuleDelayed]"

$assignmentOperators = "=" | ":=" | "^=" | "^:="


(* ::Subsection:: *)
(*symbolNameQ*)


symbolNameQ["Null"] = True

symbolNameQ[str_String] :=
	And[
		StringFreeQ[str, WhitespaceCharacter],
		MatchQ[
			Quiet @ MakeExpression[str, StandardForm],
			HoldComplete[Except[Null | Symbol[___], _Symbol]]
		]
	]

symbolNameQ[_] = False


(* ::Subsection:: *)
(*whitespaceQ*)


whitespaceQ =
	StringMatchQ[
		StringJoin[##],
		(WhitespaceCharacter | "\[IndentingNewLine]")...
	]&


(* ::Subsection:: *)
(*undefinedSymbolQ*)


SetAttributes[undefinedSymbolQ, HoldFirst]


undefinedSymbolQ[
	sym : _String | _Symbol /;
		Quiet[Context[sym], Context::notfound] === "System`"
] := False

undefinedSymbolQ[sym : _String?symbolNameQ | _Symbol] :=
	! MemberQ[Language`ExtendedDefinition[sym][[1, 2, ;; -2, 2]], Except[{}]]

undefinedSymbolQ[_] = False


(* ::Subsection:: *)
(*extractSymbolName*)


extractSymbolName[str_String] :=
	Alternatives @@ Select[StringSplit[str, "_"], symbolNameQ, 1]


(* ::Subsection:: *)
(*extractLocalVariableNames*)


extractLocalVariableNames[_][Repeated[_, {0, 1}]] = Alternatives[]

extractLocalVariableNames[type_][argsBoxes__] :=
	Alternatives @@ extractLocalVariableNames[type] /@ {argsBoxes} // Flatten

extractLocalVariableNames["Table" | "Plot" | "Integrate"][
	RowBox[{"{", RowBox[{name_String?symbolNameQ, ",", ___}], "}"}]
] :=
	name

extractLocalVariableNames["Solve" | "Integrate" | "Function"][
	name_String?symbolNameQ
] := name

extractLocalVariableNames["Solve"][
	RowBox[{"{", RowBox[argBoxes : {__}], "}"}]
] := 
	Alternatives @@ Cases[argBoxes, _String?symbolNameQ] // Flatten

extractLocalVariableNames["Solve"][
	RowBox[{"{", name_String?symbolNameQ, "}"}]
] :=
	name

extractLocalVariableNames["Limit"][
	RowBox[{name_String?symbolNameQ, "\[Rule]" | "->", __}]
] :=
	name

extractLocalVariableNames["Manipulate"][
	RowBox[{"{",
		RowBox[{
			name_String?symbolNameQ |
				RowBox[{"{",
					RowBox[{name_String?symbolNameQ, ",", ___}],
				"}"}],
			",",
			___
		}],
	"}"}]
] :=
	name

extractLocalVariableNames["Function"][
	name_String /;
		StringMatchQ[name,
			("_" .. | "") ~~ LetterCharacter ~~ WordCharacter...
		]
] := extractSymbolName[name]

extractLocalVariableNames["Scoping" | "Function"][
	RowBox[{"{", argBoxes_, "}"}]
] :=
	Alternatives @@ Cases[
		argBoxes
		,
		RowBox[{name_String, $assignmentOperators, __}] :>
			extractSymbolName[name]
		,
		{0, Infinity}
	] |
	Alternatives @@ Cases[
		argBoxes /.
			RowBox[{_String, $assignmentOperators, __}] ->
				RowBox[{}]
		,
		name_String /;
			StringMatchQ[
				name,
				("_" .. | "") ~~ LetterCharacter ~~ WordCharacter...
			] :>
				extractSymbolName[name]
		,
		{0, Infinity}
	] // Flatten

extractLocalVariableNames["LowerBound"][
	RowBox[{name_String?symbolNameQ, $assignmentOperators, ___}]
] :=
	name

extractLocalVariableNames["LowerBound"][boxes_] :=
	Alternatives @@ Cases[boxes, _?symbolNameQ, {2}] // Flatten

extractLocalVariableNames["IntegrateDifferentialD"][
	RowBox[{"\[DifferentialD]", name_String?symbolNameQ}]
] :=
	name

extractLocalVariableNames["PatternName"][boxes_] :=
	Alternatives @@ Cases[
		boxes
		,
		(
			name_String /; 
				StringMatchQ[
					name,
					LetterCharacter ~~ WordCharacter ... ~~ "_" ~~
						(WordCharacter | "_") ...
				]
		) | 
			RowBox[{name_String, ":", __}] :> extractSymbolName[name]
		,
		{0, Infinity}
	] // Flatten


(* ::Subsection:: *)
(*extractArgs*)


extractArgs[SyntaxBox[arg_, __], spec_] := extractArgs[arg, spec]

extractArgs[boxes_, 0] := {boxes} /. SyntaxBox[var_, __] :> var

extractArgs[arg_String, {min_, max_} /; min <= 1 <= max] := {arg}

extractArgs[RowBox[argsBoxes:{___}], {min_Integer, max:_Integer|Infinity}] :=
	Module[{args},
		args = argsBoxes /. SyntaxBox[var_, __] :> var;
		args =
			FixedPoint[
				Replace[#,
					{l___, PatternSequence[",", ","], r___} :>
						{l, ",", "", ",", r}
				]&
				,
				args
			];
		args = DeleteCases[args, ","];
		Take[args, {Max[1, min], Min[Length[args], max]}]
	]

extractArgs[argsBoxes_, {i_}] := extractArgs[argsBoxes, {i, i}]

extractArgs[_, {_, _}] = {}


(* ::Subsection:: *)
(*extendedSyntaxInformation*)


extendedSyntaxInformation[symName : "Block" | "Module" | "With"] :=
	Append[
		SyntaxInformation[Symbol[symName]],
		"LocalVariables" -> {"Scoping", {1}}
	]

extendedSyntaxInformation["Function"] :=
	Append[
		SyntaxInformation["Function"],
		"LocalVariables" -> {"Function", {1}}
	]

extendedSyntaxInformation["\[Sum]" | "\[Product]"] :=
	{"LocalVariables" -> {"LowerBound", 0}}

extendedSyntaxInformation["\[Integral]"] :=
	{"LocalVariables" -> {"IntegrateDifferentialD", {2, \[Infinity]}}}

extendedSyntaxInformation[$patternOperators | $patternDelayedOperators] :=
	{"LocalVariables" -> {"PatternName", {1}}}

extendedSyntaxInformation["/:", $patternOperators | $patternDelayedOperators] :=
	{"LocalVariables" -> {"PatternName", {1, 2}}}

extendedSyntaxInformation[symName_String] :=
	SyntaxInformation[Quiet[Symbol[symName], Symbol::symname]]


(* ::Subsection:: *)
(*$BoxesToAnnotationTypes*)


$BoxesToAnnotationTypes = {
	str_String /; StringMatchQ[str, "\"*\""] -> {"String"},
	_String?undefinedSymbolQ -> {"UndefinedSymbol"}
}


(* ::Subsection:: *)
(*withLocalVariables*)


SetAttributes[withLocalVariables, {HoldAll, SequenceHold}]


withLocalVariables /: Verbatim[SetDelayed][
	lhs_,
	withLocalVariables[
		funcName_, localVarsPattSym_Symbol, argumentBoxes_, body_
	]
] := (
	lhs :=
		With[
			{
				localVariables =
					"LocalVariables" /. extendedSyntaxInformation[funcName]
			}
			,
			With[
				{
					localVarsPattSym =
						extractLocalVariableNames[localVariables[[1]]] @@
							extractArgs[argumentBoxes, localVariables[[2]]]
				}
				,
				body
			] /; Length[localVariables] >= 2
		]
)


(* ::Subsection:: *)
(*annotateSyntaxInternal*)


annotateSyntaxInternal[str_String, rules_] :=
	With[{types = ReplaceList[str, rules]},
		SyntaxBox[str, Sequence @@ types] /; types =!= {}
	]

annotateSyntaxInternal[boxes_?AtomQ, _] := boxes

annotateSyntaxInternal[RowBox[{"::"}], _] = RowBox[{"::"}]

annotateSyntaxInternal[
	RowBox[{sym___, "::", tag___, "::", lang___, "::", excess___}],
	rules_
] :=
	RowBox@Join[
		annotateSyntaxInternal[#, rules] & /@ {sym},
		{If[{sym} === {}, SyntaxBox["::", "SyntaxError"], "::"]},
		SyntaxBox[#, "String"] & /@ {tag},
		{"::"},
		SyntaxBox[#, "String"] & /@ {lang},
		{SyntaxBox["::", "ExcessArgument"]},
		SyntaxBox[#, "ExcessArgument"] & /@ {excess}
	]

annotateSyntaxInternal[
	RowBox[{sym___, "::", tag___, "::", lang___}],
	rules_
] :=
	RowBox@Join[
		annotateSyntaxInternal[#, rules] & /@ {sym},
		{If[{sym} === {}, SyntaxBox["::", "SyntaxError"], "::"]},
		SyntaxBox[#, "String"] & /@ {tag},
		{If[{lang} === {}, SyntaxBox["::", "SyntaxError"], "::"]},
		SyntaxBox[#, "String"] & /@ {lang}
	]

annotateSyntaxInternal[RowBox[{sym___, "::", tag___}], rules_] :=
	RowBox@Join[
		annotateSyntaxInternal[#, rules] & /@ {sym},
		{
			If[{sym} === {} || {tag} === {},
				SyntaxBox["::", "SyntaxError"]
			(* else *),
				"::"
			]
		},
		SyntaxBox[#, "String"] & /@ {tag}
	]

annotateSyntaxInternal[
	RowBox[{
		funcName : "With" | "Module" | "Block" | "Function", "[",
			args:RowBox[{arg1_, ",", restArgs___}],
		"]"
	}]
	,
	rules_
] :=
	withLocalVariables[funcName, localVarsPatt, args,
		With[
			{
				rule =
					localVarsPatt | (
						var_String /;
							StringMatchQ[var,
								(localVarsPatt ~~ "_" ~~ ___) |
								("_" | "__" | "___" ~~localVarsPatt)
							]
					) ->
						{"Scoping", funcName}
			}
			,
			RowBox[{
				funcName
				,
				"["
				,
				RowBox[{
					Internal`InheritedBlock[{annotateSyntaxInternal},
						Unprotect[annotateSyntaxInternal];
						PrependTo[
							DownValues[annotateSyntaxInternal],
							(
								HoldPattern @ annotateSyntaxInternal[
									RowBox[{
										lhs_,
										op:$assignmentOperators,
										rhs_
									}]
									, 
									r_
								] /. OwnValues[$assignmentOperators]
							) :> 
								RowBox[{
									annotateSyntaxInternal[lhs, r],
									op, 
									annotateSyntaxInternal[
										rhs,
										DeleteCases[r, Verbatim[rule]]
									]
								}]
						];
						annotateSyntaxInternal[arg1, Prepend[rules, rule]]
					]
					,
					","
					,
					Sequence @@ annotateSyntaxInternal[
						{restArgs}, Prepend[rules, rule]
					]
				}]
				,
				"]"
			}]
		]
	]

annotateSyntaxInternal[
	RowBox[boxes:{
		PatternSequence["Function", "[", _, "]"] |
		PatternSequence[_, "&"]
	}]
	,
	rules_
] :=
	RowBox @ annotateSyntaxInternal[
		boxes,
		Prepend[
			rules,
			var_String /; StringMatchQ[var, "#" ~~ ___] ->
				{"Scoping", "Function"}
		]
	]

annotateSyntaxInternal[RowBox[{arg1_, "\[Function]", arg2_}], rules_] :=
	withLocalVariables[
		"Function",
		localVarsPatt,
		RowBox[{RowBox[{"{", arg1, "}"}], ",", arg2}]
		,
		With[
			{
				rule =
					localVarsPatt | (
						var_String /;
							StringMatchQ[var,
								(localVarsPatt ~~ "_" ~~ ___) |
								("_" | "__" | "___" ~~localVarsPatt)
							]
					) ->
						{"Scoping", "Function"}
			}
			,
			RowBox[{
				Internal`InheritedBlock[{annotateSyntaxInternal},
					Unprotect[annotateSyntaxInternal];
					PrependTo[
						DownValues[annotateSyntaxInternal],
						(
							HoldPattern @ annotateSyntaxInternal[
								RowBox[{
									lhs_,
									op:$assignmentOperators,
									rhs_
								}]
								, 
								r_
							] /. OwnValues[$assignmentOperators]
						) :> 
							RowBox[{
								annotateSyntaxInternal[lhs, r],
								op, 
								annotateSyntaxInternal[
									rhs,
									DeleteCases[r, Verbatim[rule]]
								]
							}]
					];
					annotateSyntaxInternal[arg1, Prepend[rules, rule]]
				]
				,
				"\[Function]"
				,
				annotateSyntaxInternal[arg2, Prepend[rules, rule]]
			}]
		]
	]

annotateSyntaxInternal[
	RowBox @ boxes:{
		PatternSequence[funcName_String, "[", args_, "]"] |
		PatternSequence[
			SubsuperscriptBox[funcName : "\[Integral]", _, _] |
				funcName : "\[Integral]"
			,
			args_
		] |
		PatternSequence[
			UnderoverscriptBox[funcName:"\[Sum]" | "\[Product]", args_, _],
			_
		]
	},
	rules_
] :=
	withLocalVariables[funcName, localVarsPatt, args,
		RowBox[
			annotateSyntaxInternal[
				#,
				Prepend[
					rules,
					localVarsPatt | (
						var_String /; StringMatchQ[
							var,
							(localVarsPatt ~~ "_" ~~ ___) |
							("_" | "__" | "___" ~~ localVarsPatt)
						]
					) -> {"FunctionLocalVariable", funcName}
				]
			]& /@
				boxes
		]
	]

annotateSyntaxInternal[
	boxes : RowBox[{
		PatternSequence[tag_, tagSep:"/:"] | PatternSequence[],
		lhs_,
		funcName:$patternOperators | $patternDelayedOperators,
		rhs_
	}]
	,
	rules_
] :=
	withLocalVariables[
		Sequence[tagSep, funcName],
		localVarsPatt,
		boxes /. "/:" | $patternOperators | $patternDelayedOperators -> ","
		,
		RowBox[{
			Sequence @@ ReplaceAll[
				{tag, tagSep, lhs}
				,
				str_String :>
					annotateSyntaxInternal[
						str,
						Prepend[
							rules
							,
							var_String /;
								StringMatchQ[var, localVarsPatt ~~ "_" ~~ ___] ->
									{"PatternVariable", funcName, "LHS"}
						]
					]
			]
			,
			annotateSyntaxInternal[funcName, rules]
			,
			If[MatchQ[funcName, $patternDelayedOperators],
				annotateSyntaxInternal[
					rhs
					,
					Join[
						{
							var_String /;
								StringMatchQ[var, localVarsPatt ~~ "_" ~~ ___] ->
									{"PatternVariable", funcName, "RHS", "patt"}
							,
							localVarsPatt | (
								var_String /;
									StringMatchQ[var, ("_" | "__" | "___" ~~ localVarsPatt)]) ->
										{"PatternVariable", funcName, "RHS", "var"}
						},
						rules
					]
				]
			(* else *),
				annotateSyntaxInternal[rhs, rules]
			]
		}]
	]

annotateSyntaxInternal[boxes_, rules_] :=
	annotateSyntaxInternal[#, rules] & /@ boxes


(* ::Subsection:: *)
(*posExprPosition*)


posExprPosition[expr_] := posExprPosition[expr, 0]

posExprPosition[pos_List, i_] := Drop[pos, -i]

posExprPosition[head_[___], i_] := posExprPosition[head, i + 1]


(* ::Subsection:: *)
(*NormalizeAnnotationTypes*)


NormalizeAnnotationTypes[types___] :=
	Module[{newTypes = {types}, localScopeConflict = False},
		newTypes =
			Replace[
				newTypes,
				{
					types1___,
					lv:{type:"Scoping" | "FunctionLocalVariable", ___},
					types2___,
					rhs:{"PatternVariable", _, "RHS", ___},
					types3___
				} :> {
					If[MatchQ[type, "Scoping"],
						localScopeConflict = True;
						Unevaluated[Sequence[]]
					(* else *),
						rhs
					],
					types1,
					lv,
					Sequence @@ DeleteCases[{types2, types3}, {"PatternVariable", _, "RHS", ___}]
				}
			];
		If[!localScopeConflict && Count[newTypes, {"Scoping", __}] > 1,
			localScopeConflict = True
		(* else *),
			newTypes =
				Replace[
					newTypes, {
						typesInsideScoping___,
						Longest[typesOutsideScoping:Except[{"Scoping", __}]...]
					} :> (
						If[
							Count[{typesOutsideScoping}, {"PatternVariable", ___}] > 1 ||
							MemberQ[{typesInsideScoping}, {"Scoping", __}] &&
								MemberQ[{typesOutsideScoping}, {"RHS", ___}]
							,
							localScopeConflict = True;
						];
						{typesInsideScoping, typesOutsideScoping}
					)
				]
		];
		newTypes =
			Replace[newTypes,
				{
					Longest[types1:Except[{"PatternVariable", ___}]...],
					pattv:{"PatternVariable", ___},
					types2___,
					lv:{"Scoping", __},
					types3___
				} :> {types1, lv, pattv, types2, types3}
			];
		If[localScopeConflict,
			PrependTo[newTypes, "LocalScopeConflict"]
		];
		DeleteDuplicates @ Replace[
			newTypes, {
				{"PatternVariable", _, "RHS", "patt"} -> "LocalScopeConflict",
				{"Scoping", "Function", ___} -> "PatternVariable",
				{"Scoping", "Block", ___} -> "FunctionLocalVariable",
				{"Scoping", ___} -> "LocalVariable",
				{type_, ___} :> type
			},
			{1}
		]
	]


(* ::Subsection:: *)
(*$SyntaxBoxToStyleBox*)


$SyntaxBoxToStyleBox = {
	SyntaxBox[boxes_, types__] :>
		With[{dominatingTypes = NormalizeAnnotationTypes[types]},
			If[dominatingTypes === {},
				boxes
			(* else *),
				StyleBox[
					boxes,
					CurrentValue[{AutoStyleOptions, # <> "Style"}]& /@
						dominatingTypes
				]
			]
		]
	}


(* ::Subsection:: *)
(*AnnotateSyntax*)


Options[AnnotateSyntax] = {
	"BoxRules" :> $SyntaxBoxToStyleBox,
	"BoxesToAnnoattiontypes" :> $BoxesToAnnotationTypes
}


AnnotateSyntax[boxes_, OptionsPattern[]] :=
	Module[
		{
			commentPlaceholder, boxesCommRepl, commPos, boxesComm, ignoredPos,
			boxesClean, boxesCleanParsed, syntaxPosClean, syntaxPos
		},
		boxesCommRepl =
			boxes /. RowBox[{"(*", ___, "*)"}] -> commentPlaceholder;
		commPos =
			Position[boxesCommRepl, commentPlaceholder, {-1}, Heads -> False];
		boxesComm = MapAt[SyntaxBox[#, "Comment"]&, boxes, commPos];
		ignoredPos =
			Join[
				Position[boxesCommRepl,
					_String?whitespaceQ,
					{-1},
					Heads -> False
				],
				commPos
			];
		boxesClean = Delete[boxesCommRepl, ignoredPos];
		If[{boxesClean} === {}, Return[boxesComm, Module]];
		
		boxesCleanParsed =
			annotateSyntaxInternal[
				boxesClean,
				OptionValue["BoxesToAnnoattiontypes"]
			];
		syntaxPosClean =
			Position[boxesCleanParsed, _SyntaxBox, Heads -> False];
		syntaxPos =
			Extract[
				Delete[
					MapIndexed[#2&, boxes, {-1}, Heads -> True],
					ignoredPos
				],
				syntaxPosClean,
				posExprPosition
			];
		ReplacePart[boxesComm,
			MapThread[{#1} -> ReplacePart[#2, 1 -> #3] &, {
				syntaxPos,
				Extract[boxesCleanParsed, syntaxPosClean],
				Extract[boxes, syntaxPos]
			}]
		] /. OptionValue["BoxRules"]
	]


(* ::Section:: *)
(*Package Epilogue*)


End[]


Protect @ Evaluate @ Names[
	"`" ~~ Except["$"] ~~ Repeated[WordCharacter, {0, Infinity}]
]


EndPackage[]
