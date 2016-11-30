(* Mathematica Package *)
(* Created by Mathematica plugin for IntelliJ IDEA *)

(* :Title: PackageTools *)
(* :Context: PackageTools` *)
(* :Author: szhorvat *)
(* :Date: 2016-11-19 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2016 szhorvat *)
(* :Keywords: *)
(* :Discussion: *)

If[$VersionNumber < 10, Print["PackageTools requires Mathematica 10.0 or later."]; Abort[]]

BeginPackage["PackageTools`"]
(* Exported symbols added here with SymbolName::usage *)

$MKernels::usage = "$MKernels";

MKernel::usage = "MKernel[...]";

MKernelQ::usage = "MKernelQ[mkernel]";

DetectMVersions::usage = "DetectMVersions[]";

FindMVersions::usage = "FindMVersions[version]";

MRun::usage = "\
MRun[MCode[code]]
MRun[MCode[code], mkernel]
MRun[MCode[code], version]\
";

MCode::usage = "MCode[code]";


Begin["`Flags`"]

If[Not@TrueQ[$MSlave],
  $MSlave = False
]

AppendTo[$ContextPath, $Context]
End[] (* `Flags` *)


Begin["`Private`"]


$packageFile      = $InputFileName;
$packageDirectory = DirectoryName[$InputFileName];
$packagePath      = DirectoryName[$packageDirectory];

$result;


MKernelQ[MKernel[asc_?AssociationQ]] := Sort@Keys[asc] === Sort[{"Executable", "Version", "InstallationDirectory"}]
MKernelQ[_] := False


MKernel[asc_][key_] := asc[key]

MKernel /: MakeBoxes[mk : (MKernel[_]?MKernelQ), form : (StandardForm|TraditionalForm)] :=
    With[{boxes = RowBox[{"MKernel", "[", ToBoxes[Panel[mk["Version"], FrameMargins -> 3], form], "]"}]},
      InterpretationBox[
        boxes,
        mk
      ]
    ]

Format[mk : (MKernel[_]?MKernelQ), OutputForm] := StringForm["MKernel[<``>]", mk["Version"]]


print[expr_String, label_] :=
    If[$Notebooks,
      CellPrint@Cell[expr, "Print", ShowCellLabel -> True, CellLabel -> label],
      Print[expr]
    ]

(*
  The Block[{$ContextPath = {}}, ...] is to force sending symbols through the link with full context
  information. Otherwise the context is nor prepended, and may change on the other side of the link.

  We don't want to evaluate code in an environment with an unusual $ContextPath, so we evaluate normally,
  and assign the result to $result. Then we send back the contents of $result while ensuring that it
  does not get evaluated a second time.
*)
linkEval[link_, label_][HoldComplete[code_]] :=
    Module[{result = $Failed},
      Block[{$ContextPath={}, $Context = "PackageTools`Empty`"},
        LinkWrite[link,
          Unevaluated@EvaluatePacket[
            $result = code;
            Block[{$ContextPath={}, $Context = "PackageTools`Empty`"},
              OwnValues[$result]
            ]
          ]
        ]
      ];
      While[True,
        result = LinkRead[link];
        Switch[
          result,
          _ReturnPacket, Break[],
          _TextPacket, print[First[result], label],
          _, Null (* Print[result] *)
        ]
      ];
      Replace[result, ReturnPacket[{_ :> expr_}] :> HoldComplete[expr]]
    ]

runInKernel[list : {__HoldComplete}, executable_String, label_ : None] :=
    Module[{path, link, result},
      If[Not@FileType[executable] === File, Return[$Failed]];
      path = AbsoluteFileName[executable];
      link = LinkLaunch["\"" <> path <> "\" -mathlink"];
      If[Not@LinkReadyQ[link, 10 (* timeout *)],
        LinkClose[link];
        Return[$Failed]
      ];
      LinkRead[link];
      result = Last[linkEval[link, label] /@ list];

      (* Sometimes the slave kernel won't properly quit on LinkClose[]
         if a Quit[] command isn't sent. *)
      LinkWrite[link, Unevaluated@EvaluatePacket[Quit[]]];
      LinkClose[link];
      result
    ]


DetectMVersions[] := detectMVersions[$OperatingSystem]

detectMVersions["MacOSX"] :=
    Module[{apps, kernels, res, cell},
      If[$Notebooks,
        cell = PrintTemporary@Labeled[
          ProgressIndicator[Appearance -> "Necklace"],
          Text["Detecting kernels..."], Right],
        Print["Detecting kernels..."]
      ];
      apps = FileNames["Mathematica*.app", "/Applications"];
      kernels = First@FileNames["MathKernel"|"WolframKernel", #, 3]& /@ apps;
      res = {#, runInKernel[
        List@HoldComplete[
          {First@StringSplit@SystemInformation["Kernel", "ReleaseID"], $InstallationDirectory}
        ],
        #
      ]}& /@ kernels;
      If[$Notebooks, NotebookDelete[cell], Print["Done."]];
      Cases[res,
        {k_String, HoldComplete[{v_String, id_String}]} :>
            MKernel[<|"Executable" -> k, "Version" -> v, "InstallationDirectory" -> id|>]
      ]
    ]


$MKernels := $MKernels = DetectMVersions[];


parseVersion[ver_String] := ToExpression /@ StringSplit[StringTrim[ver], "."]

matchVersion[ver_String, patt_String] :=
    With[{v = parseVersion[ver], p = parseVersion[patt]}, Take[v, Length[p]] == p]

FindMVersions[ver_String : ""] :=
    Reverse@SortBy[
      Select[$MKernels, matchVersion[#["Version"], ver]&],
      parseVersion[#["Version"]]&
    ]


SetAttributes[MCode, HoldAllComplete]


MRun[MCode[code_], kernel_?MKernelQ] :=
    With[{$packagePath = $packagePath},
      runInKernel[
        {
          HoldComplete[
            PrependTo[$Path, $packagePath];
            $MSlave = True;
            Needs["PackageTools`"];
          ],
          HoldComplete[code]
        },
        kernel["Executable"], kernel["Version"]]
    ]

MRun[code : _MCode, ver_String : ""] :=
    Module[{versions},
      versions = FindMVersions[ver];
      If[versions === {}, Return[$Failed]];
      MRun[code, First[versions]]
    ]


End[] (* `Private` *)

EndPackage[]