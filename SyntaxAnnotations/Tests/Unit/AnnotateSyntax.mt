(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["SyntaxAnnotations`Tests`Unit`AnnotateSyntax`", {"MUnit`"}]


Get["SyntaxAnnotations`"]

PrependTo[$ContextPath, "SyntaxAnnotations`Private`"]


(* ::Section:: *)
(*Tests*)


Module[
	{
		annotation, type,
		deafultStringBoxTypesDownValues = DownValues[deafultStringBoxTypes],
		stringBoxTypesDownValues = DownValues[stringBoxTypes]
	},
	Test[
		AnnotateSyntax["testStringBox",
			"Annotation" -> annotation,
			"StringBoxToTypes" -> {"testStringBox" -> {type}}
		]
		,
		annotation["testStringBox", {type}]
		,
		TestID -> "AnnotateSyntax: options"
	];
	
	Test[
		DownValues[deafultStringBoxTypes]
		,
		deafultStringBoxTypesDownValues
		,
		TestID -> "deafultStringBoxTypes DownValues"
	];
	Test[
		DownValues[stringBoxTypes]
		,
		stringBoxTypesDownValues
		,
		TestID -> "stringBoxTypes DownValues"
	]
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
