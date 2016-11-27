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

MRun::usage = "MRun[]";

MCode::usage = "MCode[]";

Begin["`Private`"]

MKernelQ[MKernel[asc_?AssociationQ]] := Sort@Keys[asc] === {"Executable", "Version"}
MKernelQ[_] := False


MKernel[asc_][key_] := asc[key]

MKernel /: MakeBoxes[mk : (MKernel[_]?MKernelQ), StandardForm | TraditionalForm] :=
    With[{boxes = ToBoxes@Panel[StringForm["M``", mk["Version"]], FrameMargins -> 2]},
      InterpretationBox[
        boxes,
        mk
      ]
    ]

Format[mk : (MKernel[_]?MKernelQ), OutputForm] := StringForm["MKernel[<``>]", mk["Version"]]


runInKernel[HoldComplete[code_], executable_String] :=
    Module[{path, link, result = $Failed},
      If[Not@FileType[executable] === File, Return[$Failed]];
      path = AbsoluteFileName[executable];
      link = LinkLaunch["\"" <> path <> "\" -mathlink"];
      If[Not@LinkReadyQ[link, 10 (* timeout *)],
        LinkClose[link];
        Return[$Failed]
      ];
      LinkRead[link];
      LinkWrite[link, Unevaluated@EvaluatePacket[code]];
      While[result = LinkRead[link, HoldComplete]; Not@MatchQ[result, HoldComplete[_ReturnPacket]]];
      LinkClose[link];
      Replace[result, HoldComplete[ReturnPacket[expr_]] :> HoldComplete[expr]]
    ]


DetectMVersions[] := detectMVersions[$OperatingSystem]

detectMVersions["MacOSX"] :=
    Module[{apps, kernels, res},
      If[$Notebooks,
        PrintTemporary@Labeled[
          ProgressIndicator[Appearance -> "Necklace"],
          Text["Detecting kernels..."], Right],
        Print["Detecting kernels..."]
      ];
      apps = FileNames["Mathematica*.app", "/Applications"];
      kernels = First@FileNames["MathKernel"|"WolframKernel", #, 3]& /@ apps;
      res = {#, runInKernel[
        HoldComplete[
          First@StringSplit@SystemInformation["Kernel", "ReleaseID"]
        ],
        #
      ]}& /@ kernels;
      Cases[res, {k_String, HoldComplete[v_String]} :> MKernel[<|"Executable" -> k, "Version" -> v|>]]
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


SetAttributes[MRun, HoldAllComplete]

MRun[MCode[code_], rest___] := MRun[code]

MRun[code_, kernel_?MKernelQ] :=
    runInKernel[HoldComplete[code], kernel["Executable"]]

MRun[code_, ver_String : ""] :=
    Module[{versions},
      versions = FindMVersions[ver];
      If[versions === {}, Return[$Failed]];
      MRun[code, First[versions]]
    ]


End[] (* `Private` *)

EndPackage[]