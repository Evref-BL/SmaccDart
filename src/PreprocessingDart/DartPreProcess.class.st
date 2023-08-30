Class {
	#name : #DartPreProcess,
	#superclass : #Object,
	#instVars : [
		'originalFolder',
		'outputFolder'
	],
	#category : #PreprocessingDart
}

{ #category : #'as yet unclassified' }
DartPreProcess >> applyRefactoringOnString: dartCodeAsString [
	|content|
	content := dartCodeAsString.
	
		content := self handleMultiLinesStringDQ: content.
		content := self handleMultiLinesStringSQ: content.
		content := self handleNewKeyword: content.
		content := self handleElseIfKeyword: content.
		
	^ content. 
]

{ #category : #'as yet unclassified' }
DartPreProcess >> duplicateFiles [

	originalFolder copyAllTo: outputFolder. 
]

{ #category : #'as yet unclassified' }
DartPreProcess >> handleElseIfKeyword: aString [

	| reg res substitute |
	reg := '}\s*else\s+if' asMyRegex.
	"the substitute that will be apply in case of a match"
	substitute := String streamContents: [ :str | 
		              str << '}' << OSPlatform current lineEnding
		              << 'else if' ].
	"substitute << '}' << OSPlatform current lineEnding << 'else if'."

	res := String streamContents: [ :out | 
		       aString readStreamDo: [ :stream | 
			       reg
				       copyStream: stream
				       to: out
				       replacingMatchesWith: substitute ] ].
	^ res
]

{ #category : #'as yet unclassified' }
DartPreProcess >> handleMultiLinesStringDQ: aString [

	"transform multi lines Single Quote (SQ) string into sigle lines string "

	| reg res matcheslines TQUOTE currentLines newLines indexFrom indexTo index QUOTE |
	"pour echaper le char $' dans la string, il faut le double (deux fois $')""simple quote"
	QUOTE := '"'.
	"triple simple quote"
	TQUOTE := '"""'.

	currentLines := ''.
	reg := ('=\s*' , TQUOTE , '([\s\S]*)' , TQUOTE , '\s?;') asMyRegex.
	"reg := '\""\""\""([\s\S]*)\""\""\""\s?;' asMyRegex."
	"the substitute that will be apply in case of a match"

	res := aString.
	[ reg search: res ] whileTrue: [ 
		| myRes beginQuoteIndex |
		"Iinitialization"
		myRes := ''.
		newLines := ' = ' , QUOTE , QUOTE.

		matcheslines := reg subexpression: 1.
		indexFrom := (res findString: matcheslines) - 1.

		"index := matcheslines findString: TQUOTE startingAt: TQUOTE size."
		beginQuoteIndex := matcheslines findString: TQUOTE startingAt: 1.

		index := matcheslines
			         findString: TQUOTE
			         startingAt: beginQuoteIndex + TQUOTE size.


		indexTo := indexFrom + index + TQUOTE size.

		"find the first bloc TDQ strings"
		currentLines := matcheslines
			                copyFrom: beginQuoteIndex + TQUOTE size
			                to: index - 1.

	
		"construct a new String declaration without multilines"
		"(currentLines substrings: OSPlatform current lineEnding)"
		(currentLines lines) do: [ 
			:content | 
			newLines := String streamContents: [ :str | 
				            str << newLines << '+' << QUOTE << content << QUOTE
				            << '+' << QUOTE << '\n' << QUOTE ] ].

		"remplace in string the original "
		myRes := (res copyFrom: 1 to: indexFrom) , newLines
		         , (res copyFrom: indexTo to: res size).

		res := myRes ].
	^ res
]

{ #category : #'as yet unclassified' }
DartPreProcess >> handleMultiLinesStringDQInParameters: aString [

	"transform multi lines Single Quote (SQ) string into sigle lines string "

	| reg res matcheslines TQUOTE currentLines newLines indexFrom indexTo index QUOTE |
	
	1 halt.
	"work in progress, do not use"
	
	"pour echaper le char $' dans la string, il faut le double (deux fois $')""simple quote"
	QUOTE := '"'.
	"triple simple quote"
	TQUOTE := '"""'.
	currentLines := ''.


	reg := ('[\(|\,]\s*' , TQUOTE , '([\s\S]*)' , TQUOTE , '\s*[\)|\,]')
		       asMyRegex.
	"[\(|\,]\s*\'\'\'([\s\S]*)\'\'\'\s*[\)|\,]"
	"the substitute that will be apply in case of a match"

	res := aString.
	[ reg search: res ] whileTrue: [ 
		| myRes beginQuoteIndex |
		"Iinitialization"
		myRes := ''.

		"Il faut inite newLines avec les bons symboles $, $( en fonction de la place du parametre dans la liste"

		newLines := ' = ' , QUOTE , QUOTE.

		matcheslines := reg subexpression: 1.
		indexFrom := (res findString: matcheslines) - 1.

		"index := matcheslines findString: TQUOTE startingAt: TQUOTE size."
		beginQuoteIndex := matcheslines findString: TQUOTE startingAt: 1.

		index := matcheslines
			         findString: TQUOTE
			         startingAt: beginQuoteIndex + TQUOTE size.


		indexTo := indexFrom + index + TQUOTE size.

		"find the first bloc TDQ strings"
		currentLines := matcheslines
			                copyFrom: beginQuoteIndex + TQUOTE size
			                to: index - 1.

		"construct a new String declaration without multilines"
		(currentLines substrings: OSPlatform current lineEnding) do: [ 
			:content | 
			newLines := String streamContents: [ :str | 
				            str << newLines << '+' << QUOTE << content << QUOTE
				            << '+' << QUOTE << '\n' << QUOTE ] ].

		"remplace in string the original "
		myRes := (res copyFrom: 1 to: indexFrom) , newLines
		         , (res copyFrom: indexTo to: res size).

		res := myRes ].
	^ res
]

{ #category : #'as yet unclassified' }
DartPreProcess >> handleMultiLinesStringSQ: aString [

	"transform multi lines Single Quote (SQ) string into sigle lines string "

	| reg res matcheslines TQUOTE currentLines newLines indexFrom indexTo index QUOTE |
	"pour echaper le char $' dans la string, il faut le double (deux fois $')""simple quote"
	QUOTE := ''''.
	"triple simple quote"
	TQUOTE := ''''''''.

	currentLines := ''.
	reg := ('=\s*' , TQUOTE , '([\s\S]*)' , TQUOTE , '\s?;') asMyRegex.
	"reg := '\""\""\""([\s\S]*)\""\""\""\s?;' asMyRegex."
	"the substitute that will be apply in case of a match"

	res := aString.
	[ reg search: res ] whileTrue: [ 
		| myRes beginQuoteIndex |
		"Iinitialization"
		myRes := ''.
		newLines := ' = ' , QUOTE , QUOTE.

		matcheslines := reg subexpression: 1.
		indexFrom := (res findString: matcheslines) - 1.

		"index := matcheslines findString: TQUOTE startingAt: TQUOTE size."
		beginQuoteIndex := matcheslines findString: TQUOTE startingAt: 1.

		index := matcheslines
			         findString: TQUOTE
			         startingAt: beginQuoteIndex + TQUOTE size.


		indexTo := indexFrom + index + TQUOTE size.

		"find the first bloc TDQ strings"
		currentLines := matcheslines
			                copyFrom: beginQuoteIndex + TQUOTE size
			                to: index - 1.

		"construct a new String declaration without multilines"
		(currentLines substrings: OSPlatform current lineEnding) do: [ 
			:content | 
			newLines := String streamContents: [ :str | 
				            str << newLines << '+' << QUOTE << content << QUOTE
				            << '+' << QUOTE << '\n' << QUOTE ] ].

		"remplace in string the original "
		myRes := (res copyFrom: 1 to: indexFrom) , newLines
		         , (res copyFrom: indexTo to: res size).

		res := myRes ].
	^ res
]

{ #category : #'as yet unclassified' }
DartPreProcess >> handleNewKeyword: aString [

	| newRegex res |
	newRegex := '(\:\s+)([A-Z][a-zA-Z]+\()' asMyRegex.
	res := String streamContents: [ :out | 
			       aString readStreamDo: [ :stream | 
				       newRegex
					       copyStream: stream
					       to: out
					       replacingMatchesWith: '{1} new {2}' ] ].
	^ res
]

{ #category : #initialization }
DartPreProcess >> initialize [

	| home |
	super initialize.
	originalFolder := FileReference new.
	home := FileLocator home.
	(home / 'outputParserDart') ensureCreateDirectory.
	outputFolder := (home / 'outputParserDart') asFileReference.
	outputFolder children do: [ :f| f deleteAll ].
]

{ #category : #accessing }
DartPreProcess >> originalFolder: aFileLocator [ 
	originalFolder := aFileLocator
]

{ #category : #accessing }
DartPreProcess >> outputFolder [
	^ outputFolder
]

{ #category : #'as yet unclassified' }
DartPreProcess >> preprocessDartFiles [

	| newRegex "elseifRegex" |
	self
		deprecated: 'Use #preprocessingDartFiles instead'
		on: '23 January 2023'
		in:
		'Pharo-10.0.0+build.528.sha.296f1d3342e7e4ade13d9fc8179531388d9e62e3 (64 Bit)'.
	newRegex := '(\:\s+)([A-Z][a-zA-Z]+\()' asMyRegex.
	"elseifRegex := '}\s*else\s+if' asRegex."

	(outputFolder allChildrenMatching: '*.dart') do: [ :file | " create a new code source "
		| res |
		res := String streamContents: [ :out | 
			       file readStreamDo: [ :stream | "  file readStreamDo: [ :stream | 
				       elseifRegex
					       copyStream: stream
					       to: out
					       replacingMatchesWith:
					       '}' , OSPlatform current lineEnding , 'else if' ]"
				       newRegex
					       copyStream: stream
					       to: out
					       replacingMatchesWith: '{1} new {2}' ] ].

		file
			delete;
			writeStreamDo: [ :stream | 
				stream
					<< '// generated code by Pharo';
					<< OSPlatform current lineEnding.
				stream
					<< '// ignore_for_file: unnecessary_new';
					<< OSPlatform current lineEnding.
				stream << res ] ]
]

{ #category : #'as yet unclassified' }
DartPreProcess >> preprocessingDartFiles [

	(outputFolder allChildrenMatching: '*.dart') do: [ :file | " create a new code source "
		| content fileStream |
		"Open a stream for reading"
		fileStream := file readStream.

		"Read the entire stream into a string"
		content := fileStream upToEnd.

		"Close the stream"
		fileStream close.

		content := self handleMultiLinesStringDQ: content.
		content := self handleMultiLinesStringSQ: content.
		content := self handleNewKeyword: content.
		content := self handleElseIfKeyword: content.


		file
			delete;
			writeStreamDo: [ :stream | 
				stream
					<< '// generated code by Pharo';
					<< OSPlatform current lineEnding.
				stream
					<< '// ignore_for_file: unnecessary_new';
					<< OSPlatform current lineEnding.
				stream << content ] ]
]
