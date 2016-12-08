(* Mathematica Package *)
(* Created by Mathematica plugin for IntelliJ IDEA *)

(* :Title: PackageTools *)
(* :Context: PackageTools` *)
(* :Author: szhorvat *)
(* :Date: 2016-11-19 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 10.0 *)
(* :Copyright: (c) 2016 szhorvat *)
(* :Keywords: *)
(* :Discussion: *)

If[$VersionNumber < 10, Print["PackageTools requires Mathematica 10.0 or later."]; Abort[]]

BeginPackage["PackageTools`", {"PacletManager`"}]

(* Kernel control *)

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

(* Notebook processing *)

RewriteNotebook::usage =
    "RewriteNotebook[f][file]\n" <>
    "RewriteNotebook[f][infile, outfile]";

NBImport::usage = "NBImport[]";
NBExport::usage = "NBExport[]";

NBHideInput::usage = "NBHideInput[nb]";
NBDeleteOutputByTag::usage = "NBDeleteOutputByTag[nb]";
NBRemoveChangeTimes::usage = "NBRemoveChangeTimes[nb]";
NBRemoveURL::usage = "NBRemoveURL[nb]";
NBResetWindow::usage = "NBResetWindow[nb]";
NBSetOptions::usage = "NBSetOptions[opt -> val][nb]";
NBDisableSpellCheck::usage = "NBDisableSpellCheck[nb]";
NBDeleteCellTags::usage = "NBDeleteCellTags[tags][nb]";
NBRemoveOptions::usage = "NBRemoveOptions[{opts}][nb]";
NBRemoveCellOptions::usage = "NBRemoveCellOptions[{opts}][nb]";
NBFEProcess::usage = "NBFEProcess[f][nb]";


(* Flags to signal slave kernels *)
Begin["`Flags`"]

If[Not@TrueQ[$MSlave],
  $MSlave = False
]

AppendTo[$ContextPath, $Context]
End[] (* `Flags` *)


Begin["`Private`"]

(********** Evaluating using slave kernels **********)


$packageFile      = $InputFileName;
$packageDirectory = DirectoryName[$InputFileName];
$packagePath      = DirectoryName[$packageDirectory];

$result; (* slave kernels assign the result to this symbol before sending it back *)


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


(********** Notebook processing **********)

fileQ[_String | File[_String]] = True
fileQ[_] = False

NBImport[file_?fileQ] :=
    Block[{$Context = "System`"},
      Import[file, "NB"]
    ]

NBExport[file_?fileQ, nb_] :=
    UsingFrontEnd@Module[{res},
      CurrentValue[$FrontEndSession, DynamicUpdating] = False;
      res = Export[file, nb, "NB"];
      CurrentValue[$FrontEndSession, DynamicUpdating] = Inherited;
      res
    ]

RewriteNotebook[f_][file_?fileQ] := RewriteNotebook[f][file, file]

RewriteNotebook[f_][infile_?fileQ, outfile_?fileQ] :=
    Module[{nb},
      nb = NBImport[infile];
      nb = f[nb];
      NBExport[outfile, nb]
    ]


(********** Notebook expression transformations **********)

(* Is it an option *)
optQ[_Symbol -> _] = True
optQ[_Symbol :> _] = True
optQ[_String -> _] = True
optQ[_String :> _] = True
optQ[_] = False

(* make sure this won't be accidentally interpreted as a rule by functions like Replace *)
optPattern[sym_Symbol] := (Rule|RuleDelayed)[sym|SymbolName[sym], _]
optPattern[str_String] := (Rule|RuleDelayed)[Symbol[str]|str, _]

(* experimental *)
NBFEProcess[f_][nb_] :=
    UsingFrontEnd@Module[{handle, res},
      CurrentValue[$FrontEndSession, DynamicUpdating] = False;
      handle = NotebookPut[nb, Visible -> False];
      f[handle];
      Block[
        {$Context = "System`"},
        res = NotebookGet[handle];
      ];
      NotebookClose[handle];
      CurrentValue[$FrontEndSession, DynamicUpdating] = Inherited;
      res
    ]


NBHideInput[nb_] :=
    ReplaceAll[
      nb,
      CellGroupData[
        cells : {
          Cell[___, CellTags -> (tags_ /; Not@FreeQ[tags, "HideInput"]), ___],
          Cell[___]
        }, _ | PatternSequence[]] :> CellGroupData[cells, {2}]
    ]


NBDeleteOutputByTag[nb_, tag_ : "DeleteOutput"] :=
    ReplaceAll[
      nb,
      Cell@CellGroupData[
        {
          in : Cell[___, CellTags -> (tags_ /; Not@FreeQ[tags, tag]), ___],
          Cell[___]
        }, _ | PatternSequence[]] :> in
    ]


NBRemoveCellOptions[opt : _Symbol|_String] := NBRemoveCellOptions[{opt}]
NBRemoveCellOptions[opts: {(_Symbol|_String)..}][nb_] :=
    ReplaceAll[nb,
      Cell[start___, Alternatives @@ (optPattern /@ opts), end___] :> Cell[start, end]
    ]


NBRemoveChangeTimes = NBRemoveCellOptions[CellChangeTimes]


NBRemoveURL[nb_] :=
    ReplaceAll[
      nb,
      {start___, Delimiter, "\"Copy web URL\"" :> _, "\"Go to web URL\"" :> _} :> {start}
    ]


NBRemoveOptions[optname : _Symbol|_String] := NBRemoveOptions[{optname}]
NBRemoveOptions[optnames : {(_Symbol|_String)..}][nb_] := DeleteCases[nb, Alternatives @@ (optPattern /@ optnames)]


NBResetWindow = NBRemoveOptions[{WindowMargins, WindowSize}]


NBSetOptions[opt_?optQ][nb_] :=
    Module[{},
      If[Not@MemberQ[Keys@Options[nb], First[opt]],
        Append[nb, opt],
        Replace[nb, optPattern@First[opt] -> opt,  {1}]
      ]
    ]
NBSetOptions[opts : {(_?optQ)..}][nb_] := Fold[NBSetOptions[#2][#1]&, nb, opts]


NBDisableSpellCheck = NBSetOptions[System`ShowAutoSpellCheck -> False]


NBDeleteCellTags[tags : {___String}][nb_] := Fold[NBDeleteCellTags[#2][#1]&, nb, tags]
NBDeleteCellTags[tag_String][nb_] :=
    ReplaceAll[
      DeleteCases[nb, CellTags -> (tag | {tag}), Infinity],
      (CellTags -> (tags_List /; MemberQ[tags, tag])) :> (CellTags -> DeleteCases[tags, tag])
    ]


End[] (* `Private` *)

EndPackage[]
