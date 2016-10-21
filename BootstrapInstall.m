(* ::Package:: *)

Get["https://raw.githubusercontent.com/jkuczm/MathematicaBootstrapInstaller/v0.1.1/BootstrapInstaller.m"]


BootstrapInstall[
	"SyntaxAnnotations",
	"https://github.com/jkuczm/MathematicaSyntaxAnnotations/releases/download/v0.2.1/SyntaxAnnotations.zip",
	"AdditionalFailureMessage" ->
		Sequence[
			"You can ",
			Hyperlink[
				"install SyntaxAnnotations package manually",
				"https://github.com/jkuczm/MathematicaSyntaxAnnotations#manual-installation"
			],
			"."
		]
]
