(*
	Copyright 2017 Tom Dela Haije

	Licensed under the Apache License, Version 2.0 (the "License");
	you may not use this file except in compliance with the License.
	You may obtain a copy of the License at

		http://www.apache.org/licenses/LICENSE-2.0

	Unless required by applicable law or agreed to in writing, software
	distributed under the License is distributed on an "AS IS" BASIS,
	WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
	See the License for the specific language governing permissions and
	limitations under the License.
*)

(*RawArrayDelete*)
(*Add support for streams*)
(*Add support for ByteOrdering*)
(*RawArrayPart messages, modify unpacking behavior?*)
(*Fix BinaryRead/Write messages*)

(*Fix for random malformed usage messages*)
System`Dump`fixmessagestring[System`Dump`s_] := ToString@InputForm@System`Dump`s;

(*Begin package*)
BeginPackage["RawArrayTools`", {"GeneralUtilities`"}];

(*Unprotect variables*)
Unprotect[
	BinaryReadRawArray,
	BinaryWriteRawArray,
	RawArrayAppend,
	RawArrayDeflatten,
	RawArrayExtract,
	RawArrayFirst,
	RawArrayLast,
	RawArrayMost,
	RawArrayPart,
	RawArrayPrepend,
	(*RawArrayReplacePart,*)
	RawArrayRest,
	RawArrayReverse,
	RawArrayTranspose
];

(*Usage*)
SetUsage[BinaryReadRawArray, 
	"BinaryReadRawArray[file$] returns a RawArray object representing the binary information in file$.", 
	"BinaryReadRawArray[file$, dims$] returns a RawArray object of dimensions dims$ representing binary information in file$.",
	"BinaryReadRawArray[file$, dims$, type$] returns a RawArray object of type type$ and dimensions dims$ representing binary information in file$."
];

SetUsage[BinaryWriteRawArray, 
	"BinaryWriteRawArray[file$, raw$] writes the information in the RawArray object raw$ to file$."
];

SetUsage[RawArrayAppend, 
	"RawArrayAppend[raw$, elem$] gives raw$ with elem$ appended.",
	"RawArrayAppend[elem$] is an operator form of RawArrayAppend that can be applied to an expression."
];

SetUsage[RawArrayDeflatten, 
	"RawArrayDeflatten[raw$, dims$] rearranges the elements of raw$ into a rectangular array with dimensions dims$."
];

SetUsage[RawArrayExtract, 
	"RawArrayExtract[raw$, pos$] extracts the part of raw$ specified by pos$.",
	"RawArrayExtract[raw$, {pos$1, pos$2, $$}] extracts a list of parts of raw$ specified by pos$.",
	"RawArrayExtract[raw$, pos$, h$] extracts parts of raw$, wrapping each of them with head h$ before evaluation.",
	"RawArrayExtract[pos$] represents an operator form of RawArrayExtract that can be applied to an expression."
];

SetUsage[RawArrayFirst, 
	"RawArrayFirst[raw$] gives the first element in raw$."
];

SetUsage[RawArrayLast, 
	"RawArrayLast[raw$] gives the last element in raw$."
];

SetUsage[RawArrayMost, 
	"RawArrayMost[raw$] gives raw$ with the last element removed."
];

SetUsage[RawArrayPart, 
	"RawArrayPart[raw$, i$] gives the i$-th element of raw$.",
	"RawArrayPart[raw$, Minus[i$]] counts from the end.",
	"RawArrayPart[raw$, i$, j$, $$] is equivalent to $$[RawArrayPart[RawArrayPart[raw$, i$], j$], $$].",
	"RawArrayPart[raw$, {i$1, i$2, $$}] gives a list of the parts i$1, i$2, $$ of raw$.",
	"RawArrayPart[raw$, Span[m$, n$]] gives parts m$ through n$.",
	"RawArrayPart[raw$, Span[m$, n$, s$]] gives parts m$ through n$ in steps s$."
];

SetUsage[RawArrayPrepend, 
	"RawArrayPrepend[raw$, elem$] gives raw$ with elem$ prepended.",
	"RawArrayPrepend[elem$] is an operator form of RawArrayPrepend that can be applied to an expression."
];

(*SetUsage[RawArrayReplacePart,
	"RawArrayReplacePart[raw$, i$ -> new$] yields an expression in which the i$-th part of raw$ is replaced by new$.",
	"RawArrayReplacePart[raw$, {i$1 -> new$1, i$2 -> new$2, $$}] replaces part at position i$n by new$n.",
	"RawArrayReplacePart[raw$, {i$, j$, $$} -> new$] replaces the part at position {i$, j$, $$}.",
	"RawArrayReplacePart[raw$, {{i$1, j$1, $$} -> new$1, $$}] replaces parts at position {i$n, j$n, $$} by new$n.",
	"RawArrayReplacePart[raw$, {{i$1, j$1, $$}, $$} -> new$] replaces all parts at positions {in$, j$n, $$} by new$.",
	"RawArrayReplacePart[i$ -> new$] represents an operator form of RawArrayReplacePart that can be applied to an expression."
];*)

SetUsage[RawArrayRest, 
	"RawArrayRest[raw$] gives raw$ with the first element removed."
];

SetUsage[RawArrayReverse,
	"RawArrayReverse[raw$] reverses the order of the elements in raw$.",
	"RawArrayReverse[raw$, n$] reverses elements at level n$ in raw$.",
	"RawArrayReverse[raw$, {n$1, n$2, $$}] reverses elements at level n$1, n$2, $$ in raw$."
];

SetUsage[RawArrayTranspose,
	"RawArrayTranspose[raw$] transposes the first two levels in raw$.",
	"RawArrayTranspose[raw$, {n$1, n$2, $$}] transposes raw$ such that the k$-th level in raw$ is the n$k-th level in the result." 
];

$RawArrayToolsLibrary::usage = "$RawArrayToolsLibrary is the full path to the RawArrayTools library loaded by RawArrayTools.";
$RawArrayToolsVersion::usage = "$RawArrayToolsVersion gives the version number of the RawArrayTools library.";

(*Messages*)
General::rawarg = "RawArray object expected at position `2` of `1`.";
RawArrayDeflatten::dims = "List of positive integers compatible with the dimensions of `3` expected at position `2` of `1`.";
RawArrayReverse::transp = "Argument `1` should be a positive integer or list of positive integers denoting a valid transposition.";

(*Options*)(*ByteOffset is a temporary option, which will become obsolete when support for streams is included*)
Options[BinaryReadRawArray] = {"AllowIncomplete" -> False, "ByteOffset" -> 0, ByteOrdering :> $ByteOrdering, Path :> $Path};
Options[BinaryWriteRawArray] = {ByteOrdering :> $ByteOrdering};

(*Syntax*)
SyntaxInformation[BinaryReadRawArray] = {"ArgumentsPattern" -> {_, _., _., OptionsPattern[]}};
SyntaxInformation[BinaryWriteRawArray] = {"ArgumentsPattern" -> {_, _, OptionsPattern[]}};
SyntaxInformation[RawArrayAppend] = {"ArgumentsPattern" -> {_, _.}};
SyntaxInformation[RawArrayDeflatten] = {"ArgumentsPattern" -> {_, _}};
SyntaxInformation[RawArrayExtract] = {"ArgumentsPattern" -> {_, _., _.}};
SyntaxInformation[RawArrayFirst] = {"ArgumentsPattern" -> {_}};
SyntaxInformation[RawArrayLast] = {"ArgumentsPattern" -> {_}};
SyntaxInformation[RawArrayMost] = {"ArgumentsPattern" -> {_}};
SyntaxInformation[RawArrayPrepend] = {"ArgumentsPattern" -> {_, _.}};
SyntaxInformation[RawArrayPart] = {"ArgumentsPattern" -> {__}};
(*SyntaxInformation[RawArrayReplacePart] = {"ArgumentsPattern" -> {_, _.}};*)
SyntaxInformation[RawArrayRest] = {"ArgumentsPattern" -> {_}};
SyntaxInformation[RawArrayReverse] = {"ArgumentsPattern" -> {_, _.}};
SyntaxInformation[RawArrayTranspose] = {"ArgumentsPattern" -> {_, _.}};

Macros`SetArgumentCount[BinaryReadRawArray, {1, Infinity}];
Macros`SetArgumentCount[BinaryWriteRawArray, {2, Infinity}];
Macros`SetArgumentCount[RawArrayAppend, {1, 2}];
Macros`SetArgumentCount[RawArrayDeflatten, 2];
Macros`SetArgumentCount[RawArrayExtract, {1, 3}];
Macros`SetArgumentCount[RawArrayFirst, 1];
Macros`SetArgumentCount[RawArrayLast, 1];
Macros`SetArgumentCount[RawArrayMost, 1];
Macros`SetArgumentCount[RawArrayPart, {1, Infinity}];
Macros`SetArgumentCount[RawArrayPrepend, {1, 2}];
(*Macros`SetArgumentCount[RawArrayReplacePart, {1, 2}];*)
Macros`SetArgumentCount[RawArrayRest, 1];
Macros`SetArgumentCount[RawArrayReverse, {1, 2}];
Macros`SetArgumentCount[RawArrayTranspose, {1, 2}];

Begin["`Private`"];
	
	$RawArrayToolsVersion = "0.1.0";
	$RawArrayToolsLibrary = FindLibrary["RawArrayTools"];
	
	$RawArrayToolsInstallationDirectory = DirectoryName[$InputFileName];

	$TypeIdentifier = <|
	   	"Byte" -> 2,
	   	"Character8" -> 2,
	   	"Character16" -> 4,
	   	"Complex64" -> 11,
	   	"Complex128" -> 12,
	   	"Integer8" -> 1,
	   	"Integer16" -> 3,
	   	"Integer32" -> 5,
	   	"Integer64" -> 7,
	   	"Real32" -> 9,
	   	"Real64" -> 10,
	   	"UnsignedInteger8" -> 2,
	   	"UnsignedInteger16" -> 4,
	   	"UnsignedInteger32" -> 6,
		"UnsignedInteger64" -> 8
	|>;

	$TypeSize = <|
	   	"Byte" -> 1,
	   	"Character8" -> 1,
	   	"Character16" -> 2,
	   	"Complex64" -> 8,
	   	"Complex128" -> 16,
	   	"Integer8" -> 1,
	   	"Integer16" -> 2,
	   	"Integer32" -> 4,
	   	"Integer64" -> 8,
	   	"Real32" -> 4,
	   	"Real64" -> 8,
	   	"UnsignedInteger8" -> 1,
	   	"UnsignedInteger16" -> 2,
	   	"UnsignedInteger32" -> 4,
		"UnsignedInteger64" -> 8
	|>;

	rawArrayDeflatten := rawArrayDeflatten = LibraryFunctionLoad[$RawArrayToolsLibrary, "rawArrayDeflatten", {{LibraryDataType[RawArray], "Constant"}, {Integer, 1, "Constant"}}, LibraryDataType[RawArray]];
	rawArrayExtract := rawArrayExtract = LibraryFunctionLoad[$RawArrayToolsLibrary, "rawArrayExtract", {{LibraryDataType[RawArray], "Constant"}, {Integer, 2, "Constant"}, {Integer, 1, "Constant"}}, LibraryDataType[RawArray]];
	rawArrayGetPart := rawArrayGetPart = LibraryFunctionLoad[$RawArrayToolsLibrary, "rawArrayGetPart", {{LibraryDataType[RawArray], "Constant"}, {Integer, 2}}, LibraryDataType[RawArray]];
	rawArrayRead := rawArrayRead = LibraryFunctionLoad[$RawArrayToolsLibrary, "rawArrayRead", {"UTF8String", Integer, {Integer, 1}, Integer, Integer}, LibraryDataType[RawArray]];
	rawArrayReplacePart := rawArrayReplacePart = LibraryFunctionLoad[$RawArrayToolsLibrary, "rawArrayReplacePart", {{LibraryDataType[RawArray], "Shared"}, {Integer, 2, "Constant"}, {LibraryDataType[RawArray], "Constant"}}, "Void"];
	rawArraySetPart := rawArraySetPart = LibraryFunctionLoad[$RawArrayToolsLibrary, "rawArraySetPart", {{LibraryDataType[RawArray], "Shared"}, {Integer, 2, "Constant"}, {LibraryDataType[RawArray], "Constant"}}, "Void"];
	rawArrayTranspose := rawArrayTranspose = LibraryFunctionLoad[$RawArrayToolsLibrary, "rawArrayTranspose", {{LibraryDataType[RawArray], "Constant"}, {Integer, 1, "Constant"}}, LibraryDataType[RawArray]];
	rawArrayWrite := rawArrayWrite = LibraryFunctionLoad[$RawArrayToolsLibrary, "rawArrayWrite", {"UTF8String", {LibraryDataType[RawArray], "Constant"}, Integer}, "Void"];
		
	TestIntegerQ[x_ ? NumberQ] := Internal`TestIntegerQ[x][[2]];
	TestIntegerQ[___] := False;

	NormalizeSpan[span_ ? TestIntegerQ, length_ ? Internal`PositiveMachineIntegerQ] := normalizeSpan[Span[span, span, 0], length];
	NormalizeSpan[span : All, length_ ? Internal`PositiveMachineIntegerQ] := {1, length, 1};
	NormalizeSpan[span : Span[RepeatedNull[_ ? TestIntegerQ | All, 3]], length_?Internal`PositiveMachineIntegerQ] := normalizeSpan[span, length];
	NormalizeSpan[___] := $Failed;
	
	normalizeSpan[Span[], length_] := {1, length, 1};
	normalizeSpan[Span[m_], length_] := normalizeSpan[Span[m, m, 1], length];
	normalizeSpan[Span[m_, n_], length_] := normalizeSpan[Span[m, n, 1], length];
	normalizeSpan[Span[All, n_, s_], length_] := normalizeSpan[Span[length, n, s], length];
	normalizeSpan[Span[m_, All, s_], length_] := normalizeSpan[Span[m, length, s], length];
	normalizeSpan[Span[m_, n_, All], length_] := normalizeSpan[Span[m, n, 1], length];
	normalizeSpan[Span[m_, n_, _?Positive], length_] /; (m == -length && n == -(length + 1)) := {0};
	normalizeSpan[Span[m_, 0, _?Positive], length_] /; m == -length := {0};
	normalizeSpan[Span[1, n_, _?Positive], length_] /; n == -(length + 1) := {0};
	normalizeSpan[Span[1, 0, _?Positive], length_] := {0};
	normalizeSpan[Span[0, -1, _?Positive], length_] := {0};
	normalizeSpan[Span[0, n_, _?Positive], length_] /; n == length := {0};
	normalizeSpan[Span[m_, -1, _?Positive], length_] /; m == length + 1 := {0};
	normalizeSpan[Span[m_, n_, _?Positive], length_] /; (m == length + 1 && n == length) := {0};
	normalizeSpan[Span[m_, n_, s_], length_] /; -length <= m < 0 := normalizeSpan[Span[length + m + 1, n, s], length];
	normalizeSpan[Span[m_, n_, s_], length_] /; -length <= n < 0 := normalizeSpan[Span[m, length + n + 1, s], length];
	normalizeSpan[Span[m_, n_, s_], length_] /; (0 < m <= n <= length && s > 0) || (0 < n <= m <= length && s < 0) || (m == n && s == 0) := {m, n, s};
	normalizeSpan[___] := $Failed;

	ComputePoints[points : {___ ? TestIntegerQ}, length_ ? Internal`PositiveMachineIntegerQ] := computePoints[points, length];
	ComputePoints[All, length_ ? Internal`PositiveMachineIntegerQ] := Range[length];
	ComputePoints[point_ ? TestIntegerQ, length_ ? Internal`PositiveMachineIntegerQ] /; (0 < point <= length) := point;
	ComputePoints[point_ ? TestIntegerQ, length_ ? Internal`PositiveMachineIntegerQ] /; (- length <= point < 0) := length + point + 1;
	ComputePoints[span : (_Span | _ ? TestIntegerQ), length_ ? Internal`PositiveMachineIntegerQ] := Module[{list = NormalizeSpan[span, length]},
		
		Range @@ list /; list =!= $Failed
		
	];
	ComputePoints[___] := $Failed;
	
	computePoints[points_, length_] /; VectorQ[points, (0 < # <= length) &] := points;
	computePoints[points_, length_] /; VectorQ[points, (0 < Abs[#] <= length) &] := points + (length + 1) UnitStep[-points]; 
	computePoints[___] := $Failed;

	IteratorDimensions[{m_, n_, s_}] := Floor[(n - m)/s] + 1;
	IteratorDimensions[{0}] := Nothing;
	
	BinaryReadRawArray[(_String | _File), type : ("Byte" | "Character8" | "Character16" | "Complex64" | "Complex128"| "Integer8" | "Integer16" | "Integer32" | "Integer64" | "Real32" | "Real64" | "UnsignedInteger8" | "UnsignedInteger16" | "UnsignedInteger32" | "UnsignedInteger64") : "Byte", {} | 0, OptionsPattern[]] := {};
	BinaryReadRawArray[channel : (_String | _File | _InputStream), type : ("Byte" | "Character8" | "Character16" | "Complex64" | "Complex128"| "Integer8" | "Integer16" | "Integer32" | "Integer64" | "Real32" | "Real64" | "UnsignedInteger8" | "UnsignedInteger16" | "UnsignedInteger32" | "UnsignedInteger64") : "Byte", dim_ ? Internal`PositiveMachineIntegerQ, opts : OptionsPattern[]] := BinaryReadRawArray[channel, type, {dim}, opts];
	BinaryReadRawArray[file : (_String | _File), type : ("Byte" | "Character8" | "Character16" | "Complex64" | "Complex128"| "Integer8" | "Integer16" | "Integer32" | "Integer64" | "Real32" | "Real64" | "UnsignedInteger8" | "UnsignedInteger16" | "UnsignedInteger32" | "UnsignedInteger64") : "Byte", dim : (Automatic | {_?Internal`PositiveMachineIntegerQ ..}) : Automatic, OptionsPattern[]] := Module[
  		{
   			byteordering = OptionValue[ByteOrdering],
   			offset = OptionValue["ByteOffset"],(*This can be removed when streams are supported*)
   			path = OptionValue[Path],
   			fullpath, fulldim, position, output
   		},
  
  		If[
  			!MatchQ[path, _String | {_String...}],
  			Message[BinaryReadRawArray::opstl, Path, path];
  			Return[$Failed, Module]
  		];
  		
  		If[
   			byteordering != 1 && byteordering != -1,
   			Message[BinaryReadRawArray::byteord, byteordering];
   			Return[$Failed, Module]
   		];
  
  		(*This can be removed when streams are supported*)
  		If[
   			!Internal`NonNegativeMachineIntegerQ[offset],
   			Message[BinaryReadRawArray::iopnm, "Offset", offset];
   			Return[$Failed, Module]
   		];
  
  		Block[{$Path = path},
   			If[
   				(fullpath = FindFile[file]) === $Failed,	
   				Message[BinaryReadRawArray::fnfnd, file]; 
   				Return[$Failed, Module]
   			]
   		];
	  	
	  	If[
  			dim === Automatic,
  			position = 0;
  			fulldim = {0},
  			position = offset + (Times @@ dim) $TypeSize[type];
  			If[FileByteCount[fullpath] < position, Return[$Failed, Module]];
  			fulldim = dim;
  		];
  
	  	AbortProtect[
  			
  			output = rawArrayRead[
		   		fullpath,
		   		$TypeIdentifier[type],
		   		fulldim,
		   		offset,
		   		byteordering
	   		];
   		
  			If[
	  	
	  			MatchQ[output, _LibraryFunctionError],
	   		
	   			$Failed,
	   			
	   			SetStreamPosition[OpenRead[fullpath, BinaryFormat -> True], position];
	   			output
		   		
	  		]
	  		
  		]
  
  	];
  	BinaryReadRawArray[stream_InputStream, type : ("Byte" | "Character8" | "Character16" | "Complex64" | "Complex128"| "Integer8" | "Integer16" | "Integer32" | "Integer64" | "Real32" | "Real64" | "UnsignedInteger8" | "UnsignedInteger16" | "UnsignedInteger32" | "UnsignedInteger64") : "Byte", {} | 0, OptionsPattern[]] := $Failed;
  	BinaryReadRawArray[stream_InputStream, type : ("Byte" | "Character8" | "Character16" | "Complex64" | "Complex128"| "Integer8" | "Integer16" | "Integer32" | "Integer64" | "Real32" | "Real64" | "UnsignedInteger8" | "UnsignedInteger16" | "UnsignedInteger32" | "UnsignedInteger64") : "Byte", dim : (Automatic | {_?Internal`PositiveMachineIntegerQ ..}) : Automatic, OptionsPattern[]] := $Failed; 
  	
  	BinaryWriteRawArray[channels : {(_String | _File | _OutputStream) ...}, raw : (_ ? Developer`RawArrayQ | {}), opts : OptionsPattern[]] := BinaryWriteRawArray[#, raw, opts] & /@ channels;  
  	BinaryWriteRawArray[file : (_String | _File), {}, opts : OptionsPattern[]] := (OpenAppend[file]; file);
  	BinaryWriteRawArray[file : (_String | _File), raw_ ? Developer`RawArrayQ, OptionsPattern[]] := Module[
  		{
   			byteordering = OptionValue[ByteOrdering],
   			fullpath = ExpandFileName[file]
   		},
  
  		If[
   			byteordering != 1 && byteordering != -1,
   			Message[BinaryWriteRawArray::byteord, byteordering];
   			Return[$Failed, Module]
   		];
  
  		AbortProtect[
  
  			If[
	  	
	  			MatchQ[rawArrayWrite[
			   		fullpath,
			   		raw,
			   		byteordering
		   		], _LibraryFunctionError],
	   		
	   			$Failed,
	   			
	   			OpenAppend[fullpath, BinaryFormat -> True];
	   			fullpath
		   		
	  		]
	  		
  		]
  
  	];
  	BinaryWriteRawArray[stream_OutputStream, {}, opts : OptionsPattern[]] := $Failed;
  	BinaryWriteRawArray[stream_OutpuStream, raw_ ? Developer`RawArrayQ, OptionsPattern[]] := $Failed;

	RawArrayAppend[raw_ ? Developer`RawArrayQ, elem_ ? Developer`RawArrayQ] /; (Dimensions[raw][[2 ;;]] === Dimensions[elem]) := Join[raw, rawArrayDeflatten[elem, Prepend[Dimensions[elem], 1]]];
	RawArrayAppend[(raw_ ? Developer`RawArrayQ) /; (ArrayDepth[raw] === 1), elem_] := Module[{appendix = RawArray[Developer`RawArrayType[raw], {elem}]},
		
		Join[raw, appendix] /; Developer`RawArrayQ[appendix]
		
	];
	input : RawArrayAppend[raw_ ? Developer`RawArrayQ, elem_] := (Message[RawArrayAppend::unpack, HoldForm[input]]; Append[Developer`FromRawArray[raw], elem]);
	RawArrayAppend[elem_] := (RawArrayAppend[#, elem] &);
	
	RawArrayDeflatten[raw_ ? Developer`RawArrayQ, dims : {_?Internal`PositiveMachineIntegerQ ..}] /; (Times @@ Dimensions[raw]) == (Times @@ dims) := rawArrayDeflatten[raw, dims];
	RawArrayDeflatten[raw_ ? Developer`RawArrayQ, dims : {}] /; (Times @@ Dimensions[raw]) == 1 := First @ Flatten @ Developer`FromRawArray @ raw;
	input : RawArrayDeflatten[raw_ ? Developer`RawArrayQ, _] := Null /; Message[RawArrayDeflatten::dims, 2, HoldForm[input], raw];
	input : RawArrayDeflatten[Repeated[_, {1, 2}]] := Null /; Message[RawArrayDeflatten::rawarg, 1, HoldForm[input]];
	
	RawArrayExtract[pos : (_ ? TestIntegerQ | {_ ? TestIntegerQ ...} | {(_ ? TestIntegerQ | {_ ? TestIntegerQ ...}) ...})] := RawArrayExtract[#, pos] &;
	RawArrayExtract[raw_ ? Developer`RawArrayQ, pos : (_ ? TestIntegerQ | {_ ? TestIntegerQ ...})] := RawArrayPart[raw, ##] & @@ Flatten[{pos}];
	RawArrayExtract[raw_ ? Developer`RawArrayQ, pos : {(_ ? TestIntegerQ | {_ ? TestIntegerQ ...}) ...}] := RawArrayExtract[raw, #] & /@ pos; 
	input : RawArrayExtract[raw_ ? Developer`RawArrayQ, pos : (_ ? TestIntegerQ | {_ ? TestIntegerQ ...} | {(_ ? TestIntegerQ | {_ ? TestIntegerQ ...}) ...}), h_] := (Message[RawArrayExtract::unpack, HoldForm[input]]; Extract[Developer`FromRawArray @ raw, pos, h]);
	input : RawArrayExtract[raw_ ? Developer`RawArrayQ, pos_, RepeatedNull[_, 1]] := Null /; Message[RawArrayExtract::psl1, pos, HoldForm[input]];
	input : RawArrayExtract[_, Repeated[_, {1, 2}]] := Null /; Message[RawArrayExtract::rawarg, 1, HoldForm[input]];
	input : RawArrayExtract[pos_] := Null /; Message[RawArrayExtract::psl1, pos, HoldForm[input]];
	
	RawArrayFirst[raw_ ? Developer`RawArrayQ] := RawArrayPart[raw, 1];
	input : RawArrayFirst[_] := Null /; Message[RawArrayFirst::rawarg, 1, HoldForm[input]];
	
	RawArrayLast[raw_ ? Developer`RawArrayQ] := RawArrayPart[raw, -1];
	input : RawArrayLast[_] := Null /; Message[RawArrayLast::rawarg, 1, HoldForm[input]];
	
	RawArrayMost[raw_ ? Developer`RawArrayQ] := RawArrayPart[raw, ;; -2];
	input : RawArrayMost[_] := Null /; Message[RawArrayMost::rawarg, 1, HoldForm[input]];
	
	RawArrayAppend[raw_ ? Developer`RawArrayQ, elem_ ? Developer`RawArrayQ] /; (Dimensions[raw][[2 ;;]] === Dimensions[elem]) := Join[raw, rawArrayDeflatten[elem, Prepend[Dimensions[elem], 1]]];
	RawArrayAppend[(raw_ ? Developer`RawArrayQ) /; (ArrayDepth[raw] === 1), elem_] := Module[{appendix = RawArray[Developer`RawArrayType[raw], {elem}]},
		
		Join[raw, appendix] /; Developer`RawArrayQ[appendix]
		
	];
	
	RawArrayPrepend[raw_ ? Developer`RawArrayQ, elem_ ? Developer`RawArrayQ] /; (Dimensions[raw][[2 ;;]] === Dimensions[elem]) := Join[rawArrayDeflatten[elem, Prepend[Dimensions[elem], 1]], raw];
	RawArrayPrepend[(raw_ ? Developer`RawArrayQ) /; (ArrayDepth[raw] === 1), elem_] := Module[{prependix = RawArray[Developer`RawArrayType[raw], {elem}]},
		
		Join[prependix, raw] /; Developer`RawArrayQ[prependix]
		
	];
	input : RawArrayPrepend[raw_ ? Developer`RawArrayQ, elem_] := (Message[RawArrayPrepend::unpack, HoldForm[input]]; Prepend[Developer`FromRawArray[raw], elem]);
	RawArrayPrepend[elem_] := (RawArrayPrepend[#, elem] &);
	
	RawArrayPart[raw_ ? Developer`RawArrayQ, 0] := RawArray;
	RawArrayPart[raw_ ? Developer`RawArrayQ] := raw;
	input : RawArrayPart[raw_ ? Developer`RawArrayQ, parts : ((_ ? TestIntegerQ | _Span | {_ ? TestIntegerQ ...}) ..)] /; Length[{parts}] <= ArrayDepth[raw] := Module[{iterator},

		iterator = MapThread[NormalizeSpan, {PadRight[{parts}, ArrayDepth[raw], All], Dimensions[raw]}];

		If[
			
			FreeQ[iterator, $Failed] && MatchQ[iterator, {{_, _, _} ..}],
			
			If[
						
				MatchQ[iterator, {{_, _, 0} ..}], 
				
				First @ Developer`FromRawArray @ rawArrayGetPart[raw, iterator], 
				
				rawArrayGetPart[raw, iterator]
				
			],
			
			iterator = MapThread[ComputePoints, {PadRight[{parts}, ArrayDepth[raw], All], Dimensions[raw]}];
		
			If[
				
				FreeQ[iterator, $Failed], 
			
				Module[{points = Distribute[iterator, List]},
				
					If[
						
						points == {},
							
						Outer[List, ##] & @@ Cases[iterator, _List],
						
						rawArrayExtract[raw, points, DeleteCases[Length /@ iterator, 0]]
						
					]
					
				],
				
				Message[RawArrayPart::unpack, HoldForm[input]];
				Part[Developer`FromRawArray @ raw, parts]
		
			]
			
		]
		
	];
	
	(*RawArrayPart /: Set[RawArrayPart[raw_ ? Developer`RawArrayQ, 0], value_] := (Message[RawArrayPart::unpack, HoldForm[input]]; value @@ Developer`FromPackedArray[raw]);
	RawArrayPart /: Set[RawArrayPart[raw_ ? Developer`RawArrayQ], value_ ? Developer`RawArrayQ] /; (Dimensions[value] === Dimensions[raw] && Developer`RawArrayType[value] === Developer`RawArrayType[raw]) := (rawArraySetPart[raw, Thread[{1, Dimensions[raw], 1}], value]; value);	
	RawArrayPart /: Set[RawArrayPart[raw_ ? Developer`RawArrayQ, parts : ((_ ? TestIntegerQ | _Span | {_ ? TestIntegerQ ...}) ..)] /; Length[{parts}] <= ArrayDepth[raw], value_ ? Developer`RawArrayQ] /; (Developer`RawArrayType[value] === Developer`RawArrayType[raw]) := Module[{iterator, error},

		iterator = MapThread[NormalizeSpan, {PadRight[{parts}, ArrayDepth[raw], All], Dimensions[raw]}];

		error = If[
			
			FreeQ[iterator, $Failed] && MatchQ[iterator, {{_, _, _} ..}],
			
			If[
						
				MatchQ[iterator, {{_, _, 0} ..}], 
				
				Message[Set::nnradims, HoldForm[value], 2];
				True, 
				
				If[
						
					(IteratorDimensions /@ iterator)[[- ArrayDepth[value] ;;]] =!= Dimensions[value],
					
					Message[Set::nnradims, HoldForm[value], 2];
					True,
					
					rawArraySetPart[raw, iterator, value]
					
				]
				
			],
			
			iterator = MapThread[ComputePoints, {PadRight[{parts}, ArrayDepth[raw], All], Dimensions[raw]}];
		
			If[
				
				FreeQ[iterator, $Failed], 
			
				Module[{points = Distribute[iterator, List]},
				
					If[
						
						points == {},
							
						Outer[List, ##] & @@ Cases[iterator, _List],
						
						rawArrayExtract[raw, points, DeleteCases[Length /@ iterator, 0]]
						
					]
					
				],
				
				Message[RawArrayPart::unpack, HoldForm[input]];
				Part[Developer`FromRawArray @ raw, parts]
		
			]
			
		];
		
		value /; !error
		
		If[
			
			FreeQ[{parts}, _List, 1],
			
			iterator = MapThread[NormalizeSpan, {PadRight[{parts}, ArrayDepth[raw], All], Dimensions[raw]}];
			
			If[
				
				FreeQ[iterator, $Failed],
				
				If[
					
					MatchQ[iterator, {{_, _, 0} ..}], 
					
					Message[Set::nnradims, HoldForm[value], 2];
					error = True, 
					
					If[
						
						(IteratorDimensions /@ iterator) =!= Dimensions[value],
						
						Message[Set::nnradims, HoldForm[value], 2];
						error = True,
						
						If[ 
							
							Developer`RawArrayType[value] =!= Developer`RawArrayType[raw], 

							Message[Set::nnvalist];											
							error = True,
						
							rawArraySetPart[raw, iterator, value]
							
						]
						
					]
					
				],
				
				error = True
				
			],
			
			iterator = MapThread[ComputePoints, {PadRight[{parts}, ArrayDepth[raw], All], Dimensions[raw]}];
	
			If[
				
				FreeQ[iterator, $Failed],
	
				Module[{points = Outer[List, ##] & @@ iterator},
				
					If[
						
						Last[Dimensions[points]] == 0,
							
						error = True,
						
						If[
							
							Dimensions[value] =!= Dimensions[points, ArrayDepth[points] - 1], 
							
							Message[Set::nnradims, HoldForm[value], 2];
							error = True, 
					
							If[
								
								Developer`RawArrayType[value] =!= Developer`RawArrayType[raw],
								
								Message[Set::nnvalist];											
								error = True,
						
								rawArrayReplacePart[raw, Flatten[points, ArrayDepth[raw] - 1], value]
								
							]
							
						]
						
					]
					
				],
				
				error = True
				
			] 
			
		];
		
		If[error, Message[Set::modfl, HoldForm[input]]];
		
		value /; !error
		
	];*)
	(*RawArrayPart /: (input : Set[RawArrayPart[raw_ ? Developer`RawArrayQ, parts : ((_ ? TestIntegerQ | _Span | {_ ? TestIntegerQ ...}) ..)] /; Length[{parts}] <= ArrayDepth[raw], value_ ? Developer`RawArrayQ]) := Module[{iterator = {}, error = False},

		If[
			
			FreeQ[Flatten[{parts}], 0],
		
			If[
				
				FreeQ[{parts}, _List, 1],
				
				iterator = MapThread[NormalizeSpan, {PadRight[{parts}, ArrayDepth[raw], All], Dimensions[raw]}];
				
				If[
					
					FreeQ[iterator, $Failed],
					
					If[
						
						MatchQ[iterator, {{_, _, 0} ..}], 
						
						Message[Set::nnradims, HoldForm[value], 2];
						error = True, 
						
						If[
							
							(IteratorDimensions /@ iterator) =!= Dimensions[value],
							
							Message[Set::nnradims, HoldForm[value], 2];
							error = True,
							
							If[ 
								
								Developer`RawArrayType[value] =!= Developer`RawArrayType[raw], 

								Message[Set::nnvalist];											
								error = True,
							
								rawArraySetPart[raw, iterator, value]
								
							]
							
						]
						
					],
					
					error = True
					
				],
				
				iterator = MapThread[ComputePoints, {PadRight[{parts}, ArrayDepth[raw], All], Dimensions[raw]}];
		
				If[
					
					FreeQ[iterator, $Failed],
		
					Module[{points = Outer[List, ##] & @@ iterator},
					
						If[
							
							Last[Dimensions[points]] == 0,
								
							error = True,
							
							If[
								
								Dimensions[value] =!= Dimensions[points, ArrayDepth[points] - 1], 
								
								Message[Set::nnradims, HoldForm[value], 2];
								error = True, 
						
								If[
									
									Developer`RawArrayType[value] =!= Developer`RawArrayType[raw],
									
									Message[Set::nnvalist];											
									error = True,
							
									rawArrayReplacePart[raw, Flatten[points, ArrayDepth[raw] - 1], value]
									
								]
								
							]
							
						]
						
					],
					
					error = True
					
				] 
				
			],
			
			error = True
			
		]; 
		
		If[error, Message[Set::modfl, HoldForm[input]]];
		
		value /; !error
		
	];*)(*OBSOLETE*)
	(*RawArrayPart /: (input : Set[RawArrayPart[raw_ ? Developer`RawArrayQ], value_ ? NumberQ]) := Module[{dim = Dimensions[raw], array, output, error},
		
		array = RawArray[Developer`RawArrayType[raw], ConstantArray[value, dim]];
		
		error = If[Developer`RawArrayQ[array], output = rawArraySetPart[raw, Thread[{1, dim, 1}], array]; False, Message[Set::modfl, HoldForm[input]]; True];
		
		output /; !error  
		
	];*)
	(*RawArrayPart /: (input : Set[RawArrayPart[raw_ ? Developer`RawArrayQ, parts : ((_ ? TestIntegerQ | _Span | {_ ? TestIntegerQ ...}) ..)] /; Length[{parts}] <= ArrayDepth[raw], value_ ? NumberQ]) := Module[{iterator = {}, error = False},

		If[
			
			FreeQ[Flatten[{parts}], 0],
		
			If[
				
				FreeQ[{parts}, _List, 1],
				
				iterator = MapThread[NormalizeSpan, {PadRight[{parts}, ArrayDepth[raw], All], Dimensions[raw]}];
				
				If[
					
					FreeQ[iterator, $Failed],
					
					If[
						
						MatchQ[iterator, {{_, _, 0} ..}], 
						
						rawArraySetPart[raw, iterator, RawArray[Developer`RawArrayType[raw], {value}]], 
						
						rawArraySetPart[raw, iterator, RawArray[Developer`RawArrayType[raw], ConstantArray[value, IteratorDimensions /@ iterator]]]
						
					],
					
					error = True
					
				],
				
				iterator = MapThread[ComputePoints, {PadRight[{parts}, ArrayDepth[raw], All], Dimensions[raw]}];
		
				If[
					
					FreeQ[iterator, $Failed],
		
					Module[{points = Outer[List, ##] & @@ iterator},
					
						If[
							
							Last[Dimensions[points]] == 0,
								
							error = True,
							
							rawArrayReplacePart[raw, Flatten[points, ArrayDepth[raw] - 1], RawArray[Developer`RawArrayType[raw], ConstantArray[value, Dimensions[points, ArrayDepth[points] - 1]]]]
							
						]
						
					],
					
					error = True
					
				] 
				
			],
			
			error = True
			
		]; 
		
		If[error, Message[Set::modfl, HoldForm[input]]];
		
		value /; !error
		
	];*)
	
	(*RawArrayReplacePart[raw_ ? Developer`RawArrayQ, rules : {(_Rule | _RuleDelayed) ..}] := ;
	RawArrayReplacePart[raw_ ? Developer`RawArrayQ, rule : (Rule | RuleDelayed)[{_List ..}, _]] := RawArrayReplacePart[raw, Thread[rule]];
	RawArrayReplacePart[raw_ ? Developer`RawArrayQ, rule : (Rule | RuleDelayed)[_, _]] := RawArrayReplacePart[raw, {rule}];
	RawArrayReplacePart[rule : (_Rule | _RuleDelayed)] := RawArrayReplacePart[#, rule] &;*)
	
	RawArrayRest[raw_ ? Developer`RawArrayQ] := RawArrayPart[raw, 2 ;;];
	input : RawArrayRest[_] := Null /; Message[RawArrayRest::rawarg, 1, HoldForm[input]];
	
	RawArrayReverse[raw_ ? Developer`RawArrayQ, level : (_ ? Internal`PositiveMachineIntegerQ) : 1] := RawArrayReverse[raw, {level}];
	RawArrayReverse[raw_ ? Developer`RawArrayQ, levels : {_ ? Internal`PositiveMachineIntegerQ ...}] := RawArrayPart[raw, ##] & @@ Replace[Range[ArrayDepth[raw]], Append[Thread[Union[levels] -> (-1 ;; 1 ;; -1)], _ -> ;;], 1];
	input : RawArrayReverse[_ ? Developer`RawArrayQ, _] := Null /; Message[RawArrayReverse::ilsmp, 2, HoldForm[input]];
	input : RawArrayReverse[Repeated[_, {1, 2}]] := Null /; Message[RawArrayReverse::rawarg, 1, HoldForm[input]];
	
	RawArrayTranspose[raw_ ? Developer`RawArrayQ] /; ArrayDepth[raw] >= 2 := rawArrayTranspose[raw, {2, 1}];
	RawArrayTranspose[raw_ ? Developer`RawArrayQ, levels : {_?Internal`PositiveMachineIntegerQ ..}] /; With[{max = Max[levels], length = Length[levels], depth = ArrayDepth[raw]}, max <= depth && length <= depth && Union[levels] === Range[max] && VectorQ[Equal @@@ (Dimensions[raw][[#]] & /@ GatherBy[Range[length], levels[[#]] &]), TrueQ]] := rawArrayTranspose[raw, levels];
	RawArrayTranspose[_ ? Developer`RawArrayQ, arg_] := Null /; Message[RawArrayTranspose::transp, arg];
	input : RawArrayTranspose[Repeated[_, {1, 2}]] := Null /; Message[RawArrayTranspose::rawarg, 1, HoldForm[input]];
	
End[];

Protect[
	BinaryReadRawArray,
	BinaryWriteRawArray,
	RawArrayAppend,
	RawArrayDeflatten,
	RawArrayExtract,
	RawArrayFirst,
	RawArrayLast,
	RawArrayMost,
	RawArrayPart,
	RawArrayPrepend,
	(*RawArrayReplacePart,*)
	RawArrayRest,
	RawArrayReverse,
	RawArrayTranspose
];

EndPackage[];