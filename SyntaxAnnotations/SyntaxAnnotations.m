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


syntaxBox::usage =
"\
syntaxBox[boxes, {type1, subtype1, ...}, {type2, subtype2, ...}, ...] \
represents boxes that in an expression perform sytnax roles of given types."


extractSymbolName::usage =
"\
extractSymbolName[\"str\"] \
returns a List. For \"str\" being valid symbol name returned list conatins \
this name. For box representation of Blank... pattern with head, returned \
list contains this head. For box representation of pattern with name, \
returned list contains this name. If none of above is true returned list is \
empty."


extractLocalVariableNames::usage =
"\
extractLocalVariableNames[\"type\"][boxes1, boxes2, ...] \
returns a List of strings with names of all local symbols extracted from \
given boxes. \
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
All occurrences of syntaxBox are stripped."


extendedSyntaxInformation::usage =
"\
extendedSyntaxInformation[\"name\"] \
returns SyntaxInformation for symbol with given \"name\". \
extendedSyntaxInformation for some built-in symbols is enriched with \
properties that are missing from ordinary SyntaxInformation, yet equivalent \
functionality is implemented by other means.\

extendedSyntaxInformation[\"op1\", \"op2\"] \
returns SyntaxInformation for operators composed of two parts."


$inFunction::usage =
"\
$inFunction \
is True if parser is currently inside box expression representing \
one-argument Function, otherwise it's false."


patternNameTypes::usage =
"\
patternNameTypes[name] \
returns List of syntax types assigned to pattern with given name."


stringBoxTypes::usage =
"\
patternNameTypes[str] \
returns List of syntax types assigned to given box String str."


modifyTypes::usage =
"\
modifyTypes[register, type, symNames] \
adds given type to syntax types of given symbol names symNames in given \
register. Possible registers are patternNameTypes and stringBoxTypes.\

modifyTypes[register, type, symNames, modFunc] \
uses result of modFunc[register[name], type] as new types for given symbol \
name.\

modifyTypes[{register1, register2, ...}, type, symNames] or \
modifyTypes[{register1, register2, ...}, type, symNames, modFunc] \
registers types in all given registers."


withModifiedTypes::usage =
"\
withLocalVariables[{type, symNames}, body] \
evaluates body with with given syntax type added to types, of given symbol \
names symNames, in patternNameTypes and stringBoxTypes registers.\

withLocalVariables[{type, symNames, modFunc}, body] \
uses result of modFunc[register[name], type] as new types for given symbol \
name.\

withLocalVariables[{register1, register2, ...}, {type, symNames, ...}, body] \
registers changes in all given registers."


withLocalVariables::usage =
"\
withLocalVariables[{funcName, localVarsSym, argumentBoxes}, body] \
when used on right hand side of SetDelayed assigns to left hand side given \
body wrapped with condition that causes definition to apply only when given \
funcName has defined extendedSyntaxInformation with \"LocalVariables\" \
element. body can contain symbol given as localVarsSym and it will be \
replaced by list of box representations of local variables extracted from \
argumentBoxes."


parse::usage =
"\
parse[boxes] \
returns boxes with certain subboxes wrapped with syntaxBox identifying their \
syntactic role."


posExprPosition::usage =
"\
posExprPosition[posExpr] \
returns position encoded in given \"position expression\" posExpr.\

posExprPosition[posExpr, i] \
returns position encoded in posExpr with last i elements droped."


normalizeAnnotationTypes::usage =
"\
normalizeAnnotationTypes[{type1, subtype1, ...}, {type2, subtype2, ...}, ...] \
returns List of strings with names of dominating annotation types from given \
sequence of annotation types."


syntaxStyleBox::usage =
"\
syntaxStyleBox[boxes, annotationTypes] \
returns StyleBox containing given boxes with proper style for given List of \
annotationTypes."


$stringBoxToTypes::usage =
"\
$stringBoxToTypes \
is List of default rules converting string boxes to annotation types."


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
	s : _String | _Symbol /; Quiet[Context[s], Context::notfound] === "System`"
] = False

undefinedSymbolQ[name_String /; StringFreeQ[name, WhitespaceCharacter]] :=
	With[{heldSym = Quiet @ MakeExpression[name, StandardForm]},
		undefinedSymbolQ @@ heldSym /;
			MatchQ[heldSym, HoldComplete[Except[Null | Symbol[___], _Symbol]]]
	]

undefinedSymbolQ[sym_Symbol] :=
	! MemberQ[Language`ExtendedDefinition[sym][[1, 2, ;; -2, 2]], Except[{}]]

undefinedSymbolQ[_] = False


(* ::Subsection:: *)
(*extractSymbolName*)


extractSymbolName[str_String] :=
	Select[StringSplit[str, "_"], symbolNameQ, 1]


(* ::Subsection:: *)
(*extractLocalVariableNames*)


extractLocalVariableNames[_][Repeated[_, {0, 1}]] = {}

extractLocalVariableNames[type_][argsBoxes__] :=
	extractLocalVariableNames[type] /@ {argsBoxes} // Flatten

extractLocalVariableNames["Table" | "Plot" | "Integrate"][
	RowBox[{"{", RowBox[{name_String?symbolNameQ, ",", ___}], "}"}]
] :=
	{name}

extractLocalVariableNames["Solve" | "Integrate" | "Function"][
	name_String?symbolNameQ
] :=
	{name}

extractLocalVariableNames["Solve"][
	RowBox[{"{", RowBox[argBoxes : {__}], "}"}]
] := 
	Cases[argBoxes, _String?symbolNameQ] // Flatten

extractLocalVariableNames["Solve"][
	RowBox[{"{", name_String?symbolNameQ, "}"}]
] :=
	{name}

extractLocalVariableNames["Limit"][
	RowBox[{name_String?symbolNameQ, "\[Rule]" | "->", __}]
] :=
	{name}

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
	{name}

extractLocalVariableNames["Function"][
	name_String /; StringMatchQ[name, "_"... ~~ Except["_"]...]
] := extractSymbolName[name]

extractLocalVariableNames["\[Function]"][argBoxes_] :=
	Flatten[{
		Cases[
			argBoxes
			,
			RowBox[{name_String, $assignmentOperators, __}] :>
				extractSymbolName[name]
			,
			{0, Infinity}
		],
		Cases[
			argBoxes /.
				RowBox[{_String, $assignmentOperators, __}] ->
					RowBox[{}]
			,
			name_String /; StringMatchQ[name, "_"... ~~ Except["_"]...] :>
				extractSymbolName[name]
			,
			{0, Infinity}
		]
	}]

extractLocalVariableNames["Scoping" | "Function"][
	RowBox[{"{", argBoxes_, "}"}]
] :=
	extractLocalVariableNames["\[Function]"][argBoxes]

extractLocalVariableNames["LowerBound"][
	RowBox[{name_String?symbolNameQ, $assignmentOperators, ___}]
] :=
	{name}

extractLocalVariableNames["LowerBound"][boxes_] :=
	Cases[boxes, _?symbolNameQ, {2}] // Flatten

extractLocalVariableNames["IntegrateDifferentialD"][
	RowBox[{"\[DifferentialD]", name_String?symbolNameQ}]
] :=
	{name}

extractLocalVariableNames["PatternName"][boxes_] :=
	Cases[
		boxes
		,
		Alternatives[
			name_String /; StringMatchQ[name, Except["_"].. ~~ "_" ~~ ___], 
			RowBox[{name_String, ":", __}]
		] :>
			extractSymbolName[name]
		,
		{0, Infinity}
	] // Flatten


(* ::Subsection:: *)
(*extractArgs*)


extractArgs[syntaxBox[arg_, __], spec_] := extractArgs[arg, spec]

extractArgs[boxes_, 0] := {boxes} /. syntaxBox[var_, __] :> var

extractArgs[arg_String, {min_, max_} /; min <= 1 <= max] := {arg}

extractArgs[RowBox[argsBoxes:{___}], {min_Integer, max:_Integer|Infinity}] :=
	Module[{args},
		args = argsBoxes /. syntaxBox[var_, __] :> var;
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
		SyntaxInformation[Function],
		"LocalVariables" -> {"Function", {1}}
	]

extendedSyntaxInformation["\[Function]"] :=
	Append[
		SyntaxInformation[Function],
		"LocalVariables" -> {"\[Function]", 0}
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
(*$inFunction*)


$inFunction = False


(* ::Subsection:: *)
(*patternNameTypes*)


patternNameTypes[_] = {"NamedPattern"}


(* ::Subsection:: *)
(*stringBoxTypes*)


stringBoxTypes[_] = {}


(* ::Subsection:: *)
(*modifyTypes*)


modifyTypes[registers_List, type_, symNames_List, modFunc_:Prepend] :=
	Scan[modifyTypes[#, type, symNames, modFunc]&, registers]

modifyTypes[stringBoxTypes, type_, symNames_List, modFunc_:Prepend] :=
	Scan[
		(
			stringBoxTypes[#] = modFunc[stringBoxTypes[#], type];
			stringBoxTypes["_" <> #] =
			stringBoxTypes["__" <> #] =
			stringBoxTypes["___" <> #] =
				modFunc[stringBoxTypes["_" <> #], type];
		)&,
		symNames
	]

modifyTypes[patternNameTypes, type_, symNames_List, modFunc_:Prepend] :=
	Scan[(patternNameTypes[#] = modFunc[patternNameTypes[#], type])&, symNames]


(* ::Subsection:: *)
(*withModifiedTypes*)


SetAttributes[withModifiedTypes, HoldRest]

withModifiedTypes[
	heads_List:{patternNameTypes, stringBoxTypes},
	{type_, symNames_List, modFunc_:Prepend},
	body_
] :=
	Internal`InheritedBlock[heads,
		modifyTypes[heads, type, symNames, modFunc];
		
		body
	]


(* ::Subsection:: *)
(*withLocalVariables*)


SetAttributes[withLocalVariables, {HoldAll, SequenceHold}]


withLocalVariables /: Verbatim[SetDelayed][
	lhs_,
	withLocalVariables[{funcName_, localVarsSym_Symbol, argumentBoxes_}, body_]
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
					localVarsSym =
						DeleteDuplicates[
							extractLocalVariableNames[localVariables[[1]]] @@
								extractArgs[argumentBoxes, localVariables[[2]]]
						]
				}
				,
				body
			] /; Length[localVariables] >= 2
		]
)


(* ::Subsection:: *)
(*parse*)


parse[str_String] :=
	With[{types = stringBoxTypes[str]},
		If[types =!= {},
			syntaxBox[str, Sequence @@ types]
		(* else *),
			str
		]
	]

parse[boxes_?AtomQ] := boxes

parse[RowBox[{"::"}]] = RowBox[{"::"}]

parse[RowBox[{sym___, "::", tag___, "::", lang___, "::", excess___}]] :=
	RowBox@Join[
		parse /@ {sym},
		{If[{sym} === {}, syntaxBox["::", "SyntaxError"], "::"]},
		syntaxBox[#, "String"] & /@ {tag},
		{"::"},
		syntaxBox[#, "String"] & /@ {lang},
		{syntaxBox["::", "ExcessArgument"]},
		syntaxBox[#, "ExcessArgument"] & /@ {excess}
	]

parse[RowBox[{sym___, "::", tag___, "::", lang___}]] :=
	RowBox@Join[
		parse /@ {sym},
		{If[{sym} === {}, syntaxBox["::", "SyntaxError"], "::"]},
		syntaxBox[#, "String"] & /@ {tag},
		{If[{lang} === {}, syntaxBox["::", "SyntaxError"], "::"]},
		syntaxBox[#, "String"] & /@ {lang}
	]

parse[RowBox[{sym___, "::", tag___}]] :=
	RowBox@Join[
		parse /@ {sym},
		{
			If[{sym} === {} || {tag} === {},
				syntaxBox["::", "SyntaxError"]
			(* else *),
				"::"
			]
		},
		syntaxBox[#, "String"] & /@ {tag}
	]

parse[
	RowBox[{
		funcName : "With" | "Module" | "Block", "[",
			args:RowBox[{arg1_, ",", restArgs___}],
		"]"
	}]
] :=
	withLocalVariables[{funcName, localVars, args},
		withModifiedTypes[{{"Scoping", funcName}, localVars},
			RowBox[{
				funcName
				,
				"["
				,
				RowBox[{
					(*	Symbols on right hand sides of assignments in first
						argument of scoping constructs are not local variables,
						so we need special parsing rule that takes that into
						account. *)
					Internal`InheritedBlock[{parse},
						PrependTo[
							DownValues[parse],
							(
								HoldPattern @ parse[
									RowBox[{
										lhs_, op:$assignmentOperators, rhs_
									}]
								] /. OwnValues[$assignmentOperators]
							) :> 
								RowBox[{
									parse[lhs],
									op,
									withModifiedTypes[
										{
											{"Scoping", funcName},
											localVars,
											DeleteCases[#1, #2, {1}, 1]&
										}
										,
										parse[rhs]
									]
								}]
						];
						parse[arg1]
					]
					,
					","
					,
					Sequence @@ parse[{restArgs}]
				}]
				,
				"]"
			}]
		]
	]

parse[
	RowBox[boxes : (
		{"Function", "[", Except[RowBox[{_, ",", ___}]], "]"} |
		{_, "&"}
	)]
] :=
	Block[{$inFunction = True},
		RowBox[parse /@ boxes]
	]

parse[
	RowBox[boxes:(
		{funcName_String, "[", args_, "]"} |
		{UnderoverscriptBox[funcName:"\[Sum]" | "\[Product]", args_, _], _} |
		{
			SubsuperscriptBox[funcName : "\[Integral]", _, _] |
				funcName : "\[Integral]"
			,
			args_
		} |
		{args_, funcName : "\[Function]", _}
	)]
] :=
	withLocalVariables[{funcName, localVars, args},
		withModifiedTypes[
			{
				{Replace[funcName, {
					"Function" | "\[Function]" -> "PatternVariable",
					_ -> "FunctionLocalVariable"
				}], funcName},
				localVars
			}
			,
			RowBox[parse /@ boxes]
		]
	]

parse[
	boxes : RowBox[{
		PatternSequence[tag_, tagSep:"/:"] | PatternSequence[],
		lhs_,
		funcName:$patternOperators | $patternDelayedOperators,
		rhs_
	}]
] :=
	withLocalVariables[
		{
			Sequence[tagSep, funcName],
			localVars,
			boxes /. "/:" | $patternOperators | $patternDelayedOperators -> ","
		}
		,
		RowBox[{
			withModifiedTypes[{patternNameTypes},
				{{"PatternVariable", funcName, "LHS"}, localVars}
				,
				(*	Block modifyTypes, so that constructs in LHS of rules and
					assignments don't change syntax roles, since that's the
					observed behavior. *)
				Sequence @@ Block[{modifyTypes},
					parse /@ {tag, tagSep, lhs}
				]
			]
			,
			parse[funcName]
			,
			If[MatchQ[funcName, $patternDelayedOperators],
				withModifiedTypes[
					{{"PatternVariable", funcName, "RHS"}, localVars}
					,
					parse[rhs]
				]
			(* else *),
				parse[rhs]
			]
		}]
	]

parse[boxes_] := parse /@ boxes


(* ::Subsection:: *)
(*posExprPosition*)


posExprPosition[expr_] := posExprPosition[expr, 0]

posExprPosition[pos_List, i_] := Drop[pos, -i]

posExprPosition[head_[___], i_] := posExprPosition[head, i + 1]


(* ::Subsection:: *)
(*normalizeAnnotationTypes*)


normalizeAnnotationTypes[types___] :=
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
		If[Last[newTypes] === "NamedPattern",
			newTypes =
				Replace[Most[newTypes],
					{"PatternVariable", _, "RHS"} -> "LocalScopeConflict",
					{1}
				]
		];
		DeleteDuplicates @ Replace[
			newTypes, {
				{"Scoping", "Block", ___} -> "FunctionLocalVariable",
				{"Scoping", ___} -> "LocalVariable",
				{type_, ___} :> type
			},
			{1}
		]
	]


(* ::Subsection:: *)
(*syntaxStyleBox*)


syntaxStyleBox[boxes_, annotationTypes_] :=
	StyleBox[
		boxes,
		CurrentValue[{AutoStyleOptions, # <> "Style"}]& /@ annotationTypes
	]


(* ::Subsection:: *)
(*$stringBoxToTypes*)


$stringBoxToTypes = {
	str_ /; $inFunction && StringMatchQ[str, "#*"] :>
		{{"PatternVariable", "Function"}}
	,
	str_ /; StringMatchQ[str, "\"*\""] -> {"String"}
	,
	_?undefinedSymbolQ -> {"UndefinedSymbol"}
	,
	str_ :>
		With[{split = StringSplit[str, "_", 2]},
			patternNameTypes @ First[split] /; MatchQ[split, {Except[""], _}]
		]
}


(* ::Subsection:: *)
(*AnnotateSyntax*)


Options[AnnotateSyntax] = {
	"Annotation" -> Automatic,
	"StringBoxToTypes" -> Automatic
}


AnnotateSyntax[boxes_, OptionsPattern[]] :=
	Module[
		{
			annotation =
				Replace[OptionValue["Annotation"], Automatic -> syntaxStyleBox]
			,
			stringBoxToTypes =
				Replace[
					OptionValue["StringBoxToTypes"],
					Automatic -> $stringBoxToTypes,
					{0, 1}
				]
			,
			commentPlaceholder, boxesCommRepl, commPos, boxesComm, ignoredPos,
			boxesClean, boxesCleanParsed, syntaxPosClean, syntaxPos
		},
		Function[{lhs, rhs}, stringBoxTypes[lhs] := rhs, HoldAll] @@@
			stringBoxToTypes;
		
		boxesCommRepl =
			boxes /. RowBox[{"(*", ___, "*)"}] -> commentPlaceholder;
		commPos =
			Position[boxesCommRepl, commentPlaceholder, {-1}, Heads -> False];
		boxesComm = MapAt[annotation[#, {"Comment"}]&, boxes, commPos];
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
		
		boxesCleanParsed = parse[boxesClean];
		syntaxPosClean =
			Position[boxesCleanParsed, _syntaxBox, Heads -> False];
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
			MapThread[
				With[{normalizedTypes = normalizeAnnotationTypes @@ Rest[#2]},
					If[normalizedTypes === {},
						Unevaluated @ Sequence[]
					(* else *),
						{#1} -> annotation[#3, normalizedTypes]
					]
				] &,
				{
					syntaxPos,
					Extract[boxesCleanParsed, syntaxPosClean],
					Extract[boxes, syntaxPos]
				}
			]
		]
	]


(* ::Section:: *)
(*Package Epilogue*)


End[]


Protect @ Evaluate @ Names[
	"`" ~~ Except["$"] ~~ Repeated[WordCharacter, {0, Infinity}]
]


EndPackage[]
