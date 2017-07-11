/*
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
*/

#include "WolframLibrary.h"
#include "WolframRawArrayLibrary.h"
#include <iostream>
#include <fstream>
#include <limits>

//Add comments
//Add consts
//rawArraySetPartValue
//rawArrayReplacePartValue
//Remove rawArrayTypeByteCount?

using namespace std;

EXTERN_C {
	DLLEXPORT mint WolframLibrary_getVersion() ;
	DLLEXPORT mint WolframLibrary_initialize(WolframLibraryData libData);
	DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData);
	DLLEXPORT int rawArrayDeflatten(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);
	DLLEXPORT int rawArrayExtract(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);
	DLLEXPORT int rawArrayGetPart(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);
	DLLEXPORT int rawArrayRead(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);
	DLLEXPORT int rawArrayReplacePart(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);
	DLLEXPORT int rawArraySetPart(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);
	DLLEXPORT int rawArrayTranspose(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);
	DLLEXPORT int rawArrayWrite(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);
}


DLLEXPORT mint WolframLibrary_getVersion()
{
	return WolframLibraryVersion;
}

DLLEXPORT mint WolframLibrary_initialize(WolframLibraryData libData)
{
	return 0;
}

DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData)
{
	return;
}

int rawArrayDataTypeByteCount(MRawArray_Data_Type type)
{
	switch (type) {
		case MRawArray_Type_Bit8:
			return 1;
		case MRawArray_Type_Ubit8:
			return 1;
		case MRawArray_Type_Bit16:
			return 2;
		case MRawArray_Type_Ubit16:
			return 2;
		case MRawArray_Type_Bit32:
			return 4;
		case MRawArray_Type_Ubit32:
			return 4;
		case MRawArray_Type_Bit64:
			return 8;
		case MRawArray_Type_Ubit64:
			return 8;
		case MRawArray_Type_Real32:
			return 4;
		case MRawArray_Type_Real64:
			return 8;
		case MRawArray_Type_Float_Complex:
			return 8;
		case MRawArray_Type_Double_Complex:
			return 16;
		default:
			return 0;
	}
}

DLLEXPORT int rawArrayDeflatten(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{
	WolframRawArrayLibrary_Functions raw = libData -> rawarrayLibraryFunctions;

	MRawArray pArray = MArgument_getMRawArray(Args[0]);
	MTensor pDimensions = MArgument_getMTensor(Args[1]);

	const mint *outputDimensions = libData -> MTensor_getIntegerData(pDimensions);
	const mint outputRank = libData -> MTensor_getFlattenedLength(pDimensions);

	mint outputSize = 1;
	for (unsigned i = 0; i < outputRank; i++) {
		if (outputDimensions[i] > 0) {
			outputSize *= outputDimensions[i];
		} else {
			return LIBRARY_DIMENSION_ERROR;
		}
	}
	if (outputSize != raw -> MRawArray_getFlattenedLength(pArray)) {
		return LIBRARY_DIMENSION_ERROR;
	}

	MRawArray_Data_Type outputType = raw -> MRawArray_getType(pArray);

	MRawArray pOutput;
	int err = (*(raw -> MRawArray_new))(outputType, outputRank, outputDimensions, &pOutput);
	if (err) return err;

	memcpy(raw -> MRawArray_getData(pOutput), raw -> MRawArray_getData(pArray), rawArrayDataTypeByteCount(outputType) * outputSize);

	MArgument_setMRawArray(Res, pOutput);
	return LIBRARY_NO_ERROR;
}

DLLEXPORT int rawArrayExtract(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{
	WolframRawArrayLibrary_Functions raw = libData -> rawarrayLibraryFunctions;

	MRawArray pInput = MArgument_getMRawArray(Args[0]);
	MTensor pCoordinates = MArgument_getMTensor(Args[1]);
	MTensor pDimensions = MArgument_getMTensor(Args[2]);

	const mint *coordinatesDimensions = libData -> MTensor_getDimensions(pCoordinates);
	mint inputRank = libData -> MTensor_getRank(pInput);

	if (coordinatesDimensions[1] != inputRank) {
		return LIBRARY_FUNCTION_ERROR;
	}

	MRawArray_Data_Type inputType = raw -> MRawArray_getType(pInput);
	mint inputTypeSize = rawArrayDataTypeByteCount(inputType);

	MRawArray pOutput;

	mint *dimensionsData = (*libData -> MTensor_getIntegerData)(pDimensions);
	mint dimensionsLength = libData -> MTensor_getFlattenedLength(pDimensions);
	if (dimensionsData[0] == 0) {
		const mint outputDimensions[1] = {coordinatesDimensions[0]};
		int err = (*(raw -> MRawArray_new))(inputType, 1, outputDimensions, &pOutput);
		if (err) return err;
	} else {
		mint outputSize = 1;
		for (unsigned i = 0; i < dimensionsLength; i++) {
			if (dimensionsData[i] > 0) {
				outputSize *= dimensionsData[i];
			} else {
				return LIBRARY_FUNCTION_ERROR;
			}
		}
		if (outputSize != coordinatesDimensions[0]) {
			return LIBRARY_FUNCTION_ERROR;
		}
		int err = (*(raw -> MRawArray_new))(inputType, dimensionsLength, dimensionsData, &pOutput);
		if (err) return err;
	}

	const mint *inputDimensions = raw -> MRawArray_getDimensions(pInput);

	char *inputData = reinterpret_cast<char *>(raw -> MRawArray_getData(pInput));
	mint *coordinatesData = (*libData -> MTensor_getIntegerData)(pCoordinates);
	char *outputData = reinterpret_cast<char *>(raw -> MRawArray_getData(pOutput));

	mint j, k, comp;
	for (mint i = 0; i < coordinatesDimensions[0]; i++) {
		j = i*inputRank;
		k = coordinatesData[j] - 1;
		if (0 <= k && k < inputDimensions[0]) {
			for (mint l = 1; l < inputRank; l++) {
				comp = coordinatesData[j + l] - 1;
				if (0 <= comp && comp < inputDimensions[l]) {
					k = k * inputDimensions[l] + comp;
				} else {
					return LIBRARY_FUNCTION_ERROR;
				}
			}
		} else {
			return LIBRARY_FUNCTION_ERROR;
		}
		memcpy(&outputData[i*inputTypeSize], &inputData[k*inputTypeSize], inputTypeSize);
	}

	MArgument_setMRawArray(Res, pOutput);
	return LIBRARY_NO_ERROR;
}

void getPart(char* outputData, char* inputData, const mint* inputDimensions, mint inputTypeSize, mint* iteratorData, mint iteratorLength, mint* pOutputPosition, mint inputPosition = 0, mint iteratorIndex = 0)
{
	mint initialPosition = inputPosition * inputDimensions[iteratorIndex];

	iteratorIndex++;

	if (iteratorIndex < iteratorLength) {
		for (mint i = iteratorData[iteratorIndex*3 - 3] - 1; (iteratorData[iteratorIndex*3 - 1] > 0) ? (i < iteratorData[iteratorIndex*3 - 2]) : (i >= iteratorData[iteratorIndex*3 - 2] - 1); i += iteratorData[iteratorIndex*3 - 1]) {
			getPart(outputData, inputData, inputDimensions, inputTypeSize, iteratorData, iteratorLength, pOutputPosition, initialPosition + i, iteratorIndex);
		}
	} else {
		for (mint i = iteratorData[iteratorIndex*3 - 3] - 1; (iteratorData[iteratorIndex*3 - 1] > 0) ? (i < iteratorData[iteratorIndex*3 - 2]) : (i >= iteratorData[iteratorIndex*3 - 2] - 1); i += iteratorData[iteratorIndex*3 - 1]) {
			memcpy(&outputData[*pOutputPosition], &inputData[(initialPosition + i)*inputTypeSize], inputTypeSize);
			*pOutputPosition += inputTypeSize;
		}
	}
}

DLLEXPORT int rawArrayGetPart(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{
	WolframRawArrayLibrary_Functions raw = libData -> rawarrayLibraryFunctions;

	MRawArray pInput = MArgument_getMRawArray(Args[0]);
	MTensor pIterator = MArgument_getMTensor(Args[1]);

	const mint *iteratorDims = libData -> MTensor_getDimensions(pIterator);

	if (iteratorDims[0] != (raw -> MRawArray_getRank(pInput)) || iteratorDims[1] != 3) {
		return LIBRARY_FUNCTION_ERROR;
	}

	mint *iteratorData = libData -> MTensor_getIntegerData(pIterator);
	const mint *inputDimensions = raw -> MRawArray_getDimensions(pInput);

	mint* outputDimensions = new mint[iteratorDims[0]];
	mint m, n, s, outputRank = 0;
	for (unsigned i = 0; i < iteratorDims[0]; i++) {
		m = iteratorData[3*i];
		n = iteratorData[3*i + 1];
		s = iteratorData[3*i + 2];
		if ((1 <= m && m <= n && n <= inputDimensions[i] && s >= 0) || (1 <= n && n <= m && m <= inputDimensions[i] && s <= 1)) {
			if (s != 0) {
				outputDimensions[outputRank] = (n - m) / s + 1;
				outputRank++;
			} else if (m == n) {
				iteratorData[3*i + 2] = 1;
			} else {
				delete [] outputDimensions;
				return LIBRARY_FUNCTION_ERROR;
			}
		} else {
			delete [] outputDimensions;
			return LIBRARY_FUNCTION_ERROR;
		}
	}

	if (outputRank == 0) {
		outputRank = 1;
		outputDimensions[0] = 1;
	}

	MRawArray_Data_Type inputType = raw -> MRawArray_getType(pInput);
	mint inputTypeSize = rawArrayDataTypeByteCount(inputType);

	MRawArray pOutput;
	int err = (*(raw -> MRawArray_new))(inputType, outputRank, outputDimensions, &pOutput);
	if (err) {
		delete [] outputDimensions;
		return err;
	}

	char *inputData = reinterpret_cast<char *>(raw -> MRawArray_getData(pInput));
	char *outputData = reinterpret_cast<char *>(raw -> MRawArray_getData(pOutput));

	mint outputPosition = 0;
	getPart(outputData, inputData, inputDimensions, inputTypeSize, iteratorData, iteratorDims[0], &outputPosition);

	MArgument_setMRawArray(Res, pOutput);
	delete [] outputDimensions;
	return LIBRARY_NO_ERROR;
}

DLLEXPORT int rawArrayRead(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{	
	ifstream file(MArgument_getUTF8String(Args[0]), ios::in | ios::binary | ios::ate);
	
	if (file.is_open()) {
		MRawArray_Data_Type type = static_cast<MRawArray_Data_Type>(MArgument_getInteger(Args[1]));
		MTensor tensor = MArgument_getMTensor(Args[2]);
		mint offset = MArgument_getInteger(Args[3]);
		mint endian = MArgument_getInteger(Args[4]);
	
		mint *dimensions = libData -> MTensor_getIntegerData(tensor);
		
		mint filesize = file.tellg();
		mint typesize = rawArrayDataTypeByteCount(type);

		mint targetSize, rank;
		if (dimensions[0] == 0) {
			rank = 1;
			targetSize = filesize - offset;
			dimensions[0] = targetSize / typesize;
		} else {
			rank = libData -> MTensor_getFlattenedLength(tensor);
			targetSize = typesize;
			for (unsigned i = 0; i < rank; i++) {
				if (dimensions[i] <= 0) {
					return LIBRARY_DIMENSION_ERROR;
				}
				targetSize *= dimensions[i];
			}
			if (filesize < targetSize + offset) {
				return LIBRARY_DIMENSION_ERROR;
			}
		}

		WolframRawArrayLibrary_Functions raw = libData -> rawarrayLibraryFunctions;
		
		MRawArray array;
		int err = (*(raw -> MRawArray_new))(type, rank, dimensions, &array);
		if (err) return err;

		char *output = reinterpret_cast<char *>(raw -> MRawArray_getData(array));

		file.seekg(offset, ios::beg);
		file.read(output, targetSize);
		file.close();

		MArgument_setMRawArray(Res, array);
		return LIBRARY_NO_ERROR;
	} else {
		return LIBRARY_FUNCTION_ERROR;
	}
}

DLLEXPORT int rawArrayReplacePart(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{
	WolframRawArrayLibrary_Functions raw = libData -> rawarrayLibraryFunctions;

	MRawArray pInput = MArgument_getMRawArray(Args[0]);
	MTensor pCoordinates = MArgument_getMTensor(Args[1]);
	MRawArray pOutput = MArgument_getMRawArray(Args[2]);

	const mint *coordinatesDimensions = libData -> MTensor_getDimensions(pCoordinates);
	mint inputRank = libData -> MTensor_getRank(pInput);

	if (coordinatesDimensions[1] != inputRank) {
		libData -> MTensor_disown(pInput);
		return LIBRARY_FUNCTION_ERROR;
	}

	if (coordinatesDimensions[0] != raw -> MRawArray_getFlattenedLength(pOutput)) {
		libData -> MTensor_disown(pInput);
		return LIBRARY_FUNCTION_ERROR;
	}

	MRawArray_Data_Type inputType = raw -> MRawArray_getType(pInput);
	mint inputTypeSize = rawArrayDataTypeByteCount(inputType);

	if (inputType != raw -> MRawArray_getType(pOutput)) {
		libData -> MTensor_disown(pInput);
		return LIBRARY_FUNCTION_ERROR;
	}

	const mint *inputDimensions = raw -> MRawArray_getDimensions(pInput);

	char *inputData = reinterpret_cast<char *>(raw -> MRawArray_getData(pInput));
	mint *coordinatesData = (*libData -> MTensor_getIntegerData)(pCoordinates);
	char *outputData = reinterpret_cast<char *>(raw -> MRawArray_getData(pOutput));

	mint j, k, comp;
	for (mint i = 0; i < coordinatesDimensions[0]; i++) {
		j = i*inputRank;
		k = coordinatesData[j] - 1;
		if (0 <= k && k < inputDimensions[0]) {
			for (mint l = 1; l < inputRank; l++) {
				comp = coordinatesData[j + l] - 1;
				if (0 <= comp && comp < inputDimensions[l]) {
					k = k * inputDimensions[l] + comp;
				} else {
					libData -> MTensor_disown(pInput);
					return LIBRARY_FUNCTION_ERROR;
				}
			}
		} else {
			return LIBRARY_FUNCTION_ERROR;
		}
		memcpy(&inputData[k*inputTypeSize], &outputData[i*inputTypeSize], inputTypeSize);
	}

	libData -> MTensor_disown(pInput);
	return LIBRARY_NO_ERROR;
}

void setPart(char* outputData, char* inputData, const mint* inputDimensions, mint inputTypeSize, mint* iteratorData, mint iteratorLength, mint* pOutputPosition, mint inputPosition = 0, mint iteratorIndex = 0)
{
	mint initialPosition = inputPosition * inputDimensions[iteratorIndex];

	iteratorIndex++;

	if (iteratorIndex < iteratorLength) {
		for (mint i = iteratorData[iteratorIndex*3 - 3] - 1; (iteratorData[iteratorIndex*3 - 1] > 0) ? (i < iteratorData[iteratorIndex*3 - 2]) : (i >= iteratorData[iteratorIndex*3 - 2] - 1); i += iteratorData[iteratorIndex*3 - 1]) {
			setPart(outputData, inputData, inputDimensions, inputTypeSize, iteratorData, iteratorLength, pOutputPosition, initialPosition + i, iteratorIndex);
		}
	} else {
		for (mint i = iteratorData[iteratorIndex*3 - 3] - 1; (iteratorData[iteratorIndex*3 - 1] > 0) ? (i < iteratorData[iteratorIndex*3 - 2]) : (i >= iteratorData[iteratorIndex*3 - 2] - 1); i += iteratorData[iteratorIndex*3 - 1]) {
			memcpy(&inputData[(initialPosition + i)*inputTypeSize], &outputData[*pOutputPosition], inputTypeSize);
			*pOutputPosition += inputTypeSize;
		}
	}
}

DLLEXPORT int rawArraySetPart(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{
	WolframRawArrayLibrary_Functions raw = libData -> rawarrayLibraryFunctions;

	MRawArray pInput = MArgument_getMRawArray(Args[0]);
	MTensor pIterator = MArgument_getMTensor(Args[1]);

	const mint *iteratorDims = libData -> MTensor_getDimensions(pIterator);

	if (iteratorDims[0] != (raw -> MRawArray_getRank(pInput)) || iteratorDims[1] != 3) {
		libData -> MTensor_disown(pInput);
		return LIBRARY_FUNCTION_ERROR;
	}

	mint *iteratorData = libData -> MTensor_getIntegerData(pIterator);
	const mint *inputDimensions = raw -> MRawArray_getDimensions(pInput);

	mint* outputDimensions = new mint[iteratorDims[0]];
	mint m, n, s, outputRank = 0;
	for (unsigned i = 0; i < iteratorDims[0]; i++) {
		m = iteratorData[3*i];
		n = iteratorData[3*i + 1];
		s = iteratorData[3*i + 2];
		if ((1 <= m && m <= n && n <= inputDimensions[i] && s >= 0) || (1 <= n && n <= m && m <= inputDimensions[i] && s <= 1)) {
			if (s != 0) {
				outputDimensions[outputRank] = (n - m) / s + 1;
				outputRank++;
			} else if (m == n) {
				iteratorData[3*i + 2] = 1;
			} else {
				libData -> MTensor_disown(pInput);
				delete [] outputDimensions;
				return LIBRARY_FUNCTION_ERROR;
			}
		} else {
			libData -> MTensor_disown(pInput);
			delete [] outputDimensions;
			return LIBRARY_FUNCTION_ERROR;
		}
	}

	if (outputRank == 0) {
		outputRank = 1;
		outputDimensions[0] = 1;
	}

	MRawArray_Data_Type inputType = raw -> MRawArray_getType(pInput);
	mint inputTypeSize = rawArrayDataTypeByteCount(inputType);

	MRawArray pOutput = MArgument_getMRawArray(Args[2]);
	const mint *givenOutputDims = raw -> MRawArray_getDimensions(pOutput);
	//rankDiscrepancy = outputRank - (raw -> MRawArray_getRank(pOutput));
	//rankDiscrepancy < 0
	if (outputRank != (raw -> MRawArray_getRank(pOutput)) || inputType != (raw -> MRawArray_getType(pOutput))) {
		libData -> MTensor_disown(pInput);
		delete [] outputDimensions;
		return LIBRARY_FUNCTION_ERROR;
	}
	//for (mint i = rankDiscrepancy (+1?); i < outputRank; i++) {
		//if (givenOutputDims[i - rankDiscrepancy (+1?)] != outputDimensions[i]) {
			//libData -> MTensor_disown(pInput);
			//return LIBRARY_FUNCTION_ERROR;
		//}
	//}
	for (mint i = 0; i < outputRank; i++) {
		if (givenOutputDims[i] != outputDimensions[i]) {
			libData -> MTensor_disown(pInput);
			delete [] outputDimensions;
			return LIBRARY_FUNCTION_ERROR;
		}
	}

	char *inputData = reinterpret_cast<char *>(raw -> MRawArray_getData(pInput));
	char *outputData = reinterpret_cast<char *>(raw -> MRawArray_getData(pOutput));

	mint outputPosition = 0;
	setPart(outputData, inputData, inputDimensions, inputTypeSize, iteratorData, iteratorDims[0], &outputPosition);

	libData -> MTensor_disown(pInput);
	delete [] outputDimensions;
	return LIBRARY_NO_ERROR;
}

void transpose(char* outputData, const mint* outputDimensions, mint outputRank, char* inputData, const mint* inputDimensions, mint inputRank, mint inputTypeSize, mint* transposition, mint* pOutputPosition, mint referencePosition[], mint iteratorIndex = 0)
{
	if (iteratorIndex < outputRank) {
		for (mint i = 0; i < outputDimensions[iteratorIndex]; i++) {
			referencePosition[iteratorIndex] = i;
			transpose(outputData, outputDimensions, outputRank, inputData, inputDimensions, inputRank, inputTypeSize, transposition, pOutputPosition, referencePosition, iteratorIndex + 1);
		}
	} else {
		mint position = referencePosition[transposition[0]];
		for (mint i = 1; i < inputRank; i++) {
			position = position*inputDimensions[i] + referencePosition[transposition[i]];
		}
		memcpy(&outputData[*pOutputPosition], &inputData[position*inputTypeSize], inputTypeSize);
		*pOutputPosition += inputTypeSize;
	}
}

DLLEXPORT int rawArrayTranspose(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{
	WolframRawArrayLibrary_Functions raw = libData -> rawarrayLibraryFunctions;

	MRawArray pInput = MArgument_getMRawArray(Args[0]);
	MTensor pTransposition = MArgument_getMTensor(Args[1]);

	mint transpositionLength = libData -> MTensor_getFlattenedLength(pTransposition);
	mint inputRank = raw -> MRawArray_getRank(pInput);

	if (transpositionLength > inputRank) {
		return LIBRARY_FUNCTION_ERROR;
	}

	mint *transpositionData = libData -> MTensor_getIntegerData(pTransposition);
	const mint *inputDimensions = raw -> MRawArray_getDimensions(pInput);

	mint* outputDimensions = new mint[inputRank];
	bool* levels = new bool[transpositionLength];
	for (unsigned i = 0; i < transpositionLength; i++) {
		levels[i] = false;
	}
	mint* transposition = new mint[inputRank];
	mint outputRank = 0;
	for (unsigned i = 0; i < transpositionLength; i++) {
		if (0 < transpositionData[i] && transpositionData[i] <= transpositionLength) {
			transposition[i] = transpositionData[i] - 1;
			if (levels[transposition[i]]) {
				if (inputDimensions[transposition[i]] != inputDimensions[i]) {
					delete [] outputDimensions;
					delete [] levels;
					delete [] transposition;
					return LIBRARY_DIMENSION_ERROR;
				}
			} else {
				outputDimensions[transposition[i]] = inputDimensions[outputRank];
				levels[transposition[i]] = true;
				outputRank++;
			}
		} else {
			delete [] outputDimensions;
			delete [] levels;
			delete [] transposition;
			return LIBRARY_DIMENSION_ERROR;
		}
	}
	for (unsigned i = 0; i < outputRank; i++) {
		if (!levels[i]) {
			delete [] outputDimensions;
			delete [] levels;
			delete [] transposition;
			return LIBRARY_DIMENSION_ERROR;
		}
	}
	for (unsigned i = transpositionLength; i < inputRank; i++) {
		outputDimensions[outputRank] = inputDimensions[outputRank];
		transposition[i] = outputRank;
		outputRank++;
	}

	MRawArray_Data_Type inputType = raw -> MRawArray_getType(pInput);
	mint inputTypeSize = rawArrayDataTypeByteCount(inputType);

	MRawArray pOutput;
	int err = (*(raw -> MRawArray_new))(inputType, outputRank, outputDimensions, &pOutput);
	if (err) {
		delete [] outputDimensions;
		delete [] levels;
		delete [] transposition;
		return err;
	}

	char *inputData = reinterpret_cast<char *>(raw -> MRawArray_getData(pInput));
	char *outputData = reinterpret_cast<char *>(raw -> MRawArray_getData(pOutput));

	mint* referencePosition = new mint[outputRank];
	mint outputPosition = 0;

	transpose(outputData, outputDimensions, outputRank, inputData, inputDimensions, inputRank, inputTypeSize, transposition, &outputPosition, referencePosition);

	MArgument_setMRawArray(Res, pOutput);
	delete [] outputDimensions;
	delete [] levels;
	delete [] transposition;
	delete [] referencePosition;
	return LIBRARY_NO_ERROR;
}

DLLEXPORT int rawArrayWrite(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{
	ofstream file(MArgument_getUTF8String(Args[0]), ios::out | ios::binary);

	if (file.is_open()) {
		WolframRawArrayLibrary_Functions raw = libData -> rawarrayLibraryFunctions;

		MRawArray pInput = MArgument_getMRawArray(Args[1]);

		const mint *inputDimensions = raw -> MRawArray_getDimensions(pInput);
		mint inputRank = raw -> MRawArray_getRank(pInput);
		MRawArray_Data_Type inputType = raw -> MRawArray_getType(pInput);

		mint targetSize = rawArrayDataTypeByteCount(inputType);
		for (unsigned i = 0; i < inputRank; i++) {
			targetSize *= inputDimensions[i];
		}

		mint endian = MArgument_getInteger(Args[2]);

		char *inputData = reinterpret_cast<char *>(raw -> MRawArray_getData(pInput));

		file.write(inputData, targetSize);
		file.close();

		return LIBRARY_NO_ERROR;
	} else {
		return LIBRARY_FUNCTION_ERROR;
	}
}
