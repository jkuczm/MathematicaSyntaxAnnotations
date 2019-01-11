(* Mathematica Init File *)

Internal`InheritedBlock[{BeginPackage},
	Unprotect@BeginPackage;
	(*	If package is loaded through init.m it'll be loaded as package with
		ordinary "top-level" context. *)
	BeginPackage@"`SyntaxAnnotations`" := BeginPackage@"SyntaxAnnotations`";
	Protect@BeginPackage;
	Get@"SyntaxAnnotations`SyntaxAnnotations`"
]
