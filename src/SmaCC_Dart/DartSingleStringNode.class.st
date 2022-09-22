Class {
	#name : #DartSingleStringNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'rAW_SINGLE_LINE_STRINGToken',
		'rAW_MULTI_LINE_STRINGToken',
		'sINGLE_LINE_STRING_DQ_BEGIN_ENDToken',
		'sINGLE_LINE_STRING_SQ_BEGIN_ENDToken',
		'mULTI_LINE_STRING_DQ_BEGIN_ENDToken',
		'mULTI_LINE_STRING_SQ_BEGIN_ENDToken'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartSingleStringNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitSingleString: self
]

{ #category : #generated }
DartSingleStringNode >> mULTI_LINE_STRING_DQ_BEGIN_ENDToken [

	^ mULTI_LINE_STRING_DQ_BEGIN_ENDToken
]

{ #category : #generated }
DartSingleStringNode >> mULTI_LINE_STRING_DQ_BEGIN_ENDToken: aSmaCCToken [

	mULTI_LINE_STRING_DQ_BEGIN_ENDToken := aSmaCCToken
]

{ #category : #generated }
DartSingleStringNode >> mULTI_LINE_STRING_SQ_BEGIN_ENDToken [

	^ mULTI_LINE_STRING_SQ_BEGIN_ENDToken
]

{ #category : #generated }
DartSingleStringNode >> mULTI_LINE_STRING_SQ_BEGIN_ENDToken: aSmaCCToken [

	mULTI_LINE_STRING_SQ_BEGIN_ENDToken := aSmaCCToken
]

{ #category : #generated }
DartSingleStringNode >> rAW_MULTI_LINE_STRINGToken [

	^ rAW_MULTI_LINE_STRINGToken
]

{ #category : #generated }
DartSingleStringNode >> rAW_MULTI_LINE_STRINGToken: aSmaCCToken [

	rAW_MULTI_LINE_STRINGToken := aSmaCCToken
]

{ #category : #generated }
DartSingleStringNode >> rAW_SINGLE_LINE_STRINGToken [

	^ rAW_SINGLE_LINE_STRINGToken
]

{ #category : #generated }
DartSingleStringNode >> rAW_SINGLE_LINE_STRINGToken: aSmaCCToken [

	rAW_SINGLE_LINE_STRINGToken := aSmaCCToken
]

{ #category : #generated }
DartSingleStringNode >> sINGLE_LINE_STRING_DQ_BEGIN_ENDToken [

	^ sINGLE_LINE_STRING_DQ_BEGIN_ENDToken
]

{ #category : #generated }
DartSingleStringNode >> sINGLE_LINE_STRING_DQ_BEGIN_ENDToken: aSmaCCToken [

	sINGLE_LINE_STRING_DQ_BEGIN_ENDToken := aSmaCCToken
]

{ #category : #generated }
DartSingleStringNode >> sINGLE_LINE_STRING_SQ_BEGIN_ENDToken [

	^ sINGLE_LINE_STRING_SQ_BEGIN_ENDToken
]

{ #category : #generated }
DartSingleStringNode >> sINGLE_LINE_STRING_SQ_BEGIN_ENDToken: aSmaCCToken [

	sINGLE_LINE_STRING_SQ_BEGIN_ENDToken := aSmaCCToken
]

{ #category : #generated }
DartSingleStringNode >> tokenVariables [

	^ #( #rAW_SINGLE_LINE_STRINGToken #rAW_MULTI_LINE_STRINGToken
	     #sINGLE_LINE_STRING_DQ_BEGIN_ENDToken
	     #sINGLE_LINE_STRING_SQ_BEGIN_ENDToken
	     #mULTI_LINE_STRING_DQ_BEGIN_ENDToken
	     #mULTI_LINE_STRING_SQ_BEGIN_ENDToken )
]
