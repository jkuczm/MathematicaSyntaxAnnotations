(* ::Package:: *)


Internal`InheritedBlock[{BeginPackage},
	Unprotect@BeginPackage;
	(*	If package is loaded through NoInstall.m it'll be loaded as package
		with ordinary "top-level" context. *)
	BeginPackage@"`SyntaxAnnotations`" := BeginPackage@"SyntaxAnnotations`";
	Protect@BeginPackage;
	Import@"https://raw.githubusercontent.com/jkuczm/MathematicaSyntaxAnnotations/master/SyntaxAnnotations/SyntaxAnnotations.m"
]
