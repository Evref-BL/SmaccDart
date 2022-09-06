Class {
	#name : #DartExportDeclarationNode,
	#superclass : #DartImportExportDeclarationNode,
	#instVars : [
		'_Tokens',
		'periodTokens',
		'newTokens',
		'lessThanTokens',
		'greaterThanTokens',
		'leftParenTokens',
		'rightParenTokens',
		'questionMarkTokens',
		'voidTokens',
		'colonTokens',
		'throwTokens',
		'superTokens',
		'equalTokens',
		'periodPeriodTokens',
		'questionMarkPeriodPeriodTokens',
		'extendsTokens',
		'equalGreaterThanTokens',
		'leftBracketTokens',
		'rightBracketTokens',
		'thisTokens',
		'constTokens',
		'bangTokens',
		'questionMarkPeriodTokens',
		'timesEqualTokens',
		'divideEqualTokens',
		'tildeDivideEqualTokens',
		'percentEqualTokens',
		'plusEqualTokens',
		'minusEqualTokens',
		'lessThanLessThanEqualTokens',
		'andEqualTokens',
		'upArrowEqualTokens',
		'orEqualTokens',
		'questionMarkQuestionMarkEqualTokens',
		'questionMarkQuestionMarkTokens',
		'lbraceTokens',
		'rbraceTokens',
		'statementses',
		'timesTokens',
		'nullTokens',
		'trueTokens',
		'falseTokens',
		'numberTokens',
		'hEX_NUMBERTokens',
		'tildeTokens',
		'orOrTokens',
		'equalEqualTokens',
		'finalTokens',
		'rAW_MULTI_LINE_STRINGTokens',
		'mULTI_LINE_STRING_SQ_BEGIN_ENDTokens',
		'mULTI_LINE_STRING_SQ_BEGIN_MIDTokens',
		'mULTI_LINE_STRING_SQ_MID_ENDTokens',
		'mULTI_LINE_STRING_DQ_BEGIN_ENDTokens',
		'mULTI_LINE_STRING_DQ_BEGIN_MIDTokens',
		'mULTI_LINE_STRING_DQ_MID_ENDTokens',
		'rAW_SINGLE_LINE_STRINGTokens',
		'sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens',
		'sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens',
		'sINGLE_LINE_STRING_SQ_MID_ENDTokens',
		'sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens',
		'sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens',
		'sINGLE_LINE_STRING_DQ_MID_ENDTokens',
		'mULTI_LINE_STRING_SQ_MID_MIDTokens',
		'mULTI_LINE_STRING_DQ_MID_MIDTokens',
		'sINGLE_LINE_STRING_SQ_MID_MIDTokens',
		'sINGLE_LINE_STRING_DQ_MID_MIDTokens',
		'divideTokens',
		'percentTokens',
		'tildeDivideTokens',
		'plusTokens',
		'minusTokens',
		'lessThanLessThanTokens',
		'lessThanEqualTokens',
		'andTokens',
		'upArrowTokens',
		'orTokens',
		'periodPeriodPeriodTokens',
		'periodPeriodPeriodQuestionMarkTokens',
		'ifTokens',
		'elseTokens',
		'forTokens',
		'andAndTokens',
		'varTokens',
		'inTokens',
		'semicolonTokens',
		'bangEqualTokens',
		'isTokens',
		'plusPlusTokens',
		'minusMinusTokens',
		'stringuri'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartExportDeclarationNode >> _Tokens [

	^ _Tokens
]

{ #category : #generated }
DartExportDeclarationNode >> _Tokens: anOrderedCollection [

	_Tokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitExportDeclaration: self
]

{ #category : #generated }
DartExportDeclarationNode >> andAndTokens [

	^ andAndTokens
]

{ #category : #generated }
DartExportDeclarationNode >> andAndTokens: anOrderedCollection [

	andAndTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> andEqualTokens [

	^ andEqualTokens
]

{ #category : #generated }
DartExportDeclarationNode >> andEqualTokens: anOrderedCollection [

	andEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> andTokens [

	^ andTokens
]

{ #category : #generated }
DartExportDeclarationNode >> andTokens: anOrderedCollection [

	andTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> bangEqualTokens [

	^ bangEqualTokens
]

{ #category : #generated }
DartExportDeclarationNode >> bangEqualTokens: anOrderedCollection [

	bangEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> bangTokens [

	^ bangTokens
]

{ #category : #generated }
DartExportDeclarationNode >> bangTokens: anOrderedCollection [

	bangTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> colonTokens [

	^ colonTokens
]

{ #category : #generated }
DartExportDeclarationNode >> colonTokens: anOrderedCollection [

	colonTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> compositeTokenVariables [

	^ #( #_Tokens #periodTokens #newTokens #lessThanTokens
	     #greaterThanTokens #leftParenTokens #rightParenTokens
	     #questionMarkTokens #voidTokens #colonTokens #throwTokens
	     #superTokens #equalTokens #periodPeriodTokens
	     #questionMarkPeriodPeriodTokens #extendsTokens #equalGreaterThanTokens
	     #leftBracketTokens #rightBracketTokens #thisTokens #constTokens
	     #bangTokens #questionMarkPeriodTokens #timesEqualTokens
	     #divideEqualTokens #tildeDivideEqualTokens
	     #percentEqualTokens #plusEqualTokens #minusEqualTokens
	     #lessThanLessThanEqualTokens #andEqualTokens #upArrowEqualTokens
	     #orEqualTokens #questionMarkQuestionMarkEqualTokens
	     #questionMarkQuestionMarkTokens #lbraceTokens #rbraceTokens
	     #timesTokens #nullTokens #trueTokens #falseTokens #numberTokens
	     #hEX_NUMBERTokens #tildeTokens #orOrTokens #equalEqualTokens
	     #finalTokens #andAndTokens #varTokens #rAW_MULTI_LINE_STRINGTokens
	     #mULTI_LINE_STRING_SQ_BEGIN_ENDTokens
	     #mULTI_LINE_STRING_SQ_BEGIN_MIDTokens
	     #mULTI_LINE_STRING_SQ_MID_ENDTokens
	     #mULTI_LINE_STRING_DQ_BEGIN_ENDTokens
	     #mULTI_LINE_STRING_DQ_BEGIN_MIDTokens
	     #mULTI_LINE_STRING_DQ_MID_ENDTokens
	     #rAW_SINGLE_LINE_STRINGTokens #sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens
	     #sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens
	     #sINGLE_LINE_STRING_SQ_MID_ENDTokens
	     #sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens
	     #sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens
	     #sINGLE_LINE_STRING_DQ_MID_ENDTokens
	     #divideTokens #percentTokens #tildeDivideTokens
	     #plusTokens #minusTokens #lessThanLessThanTokens
	     #lessThanEqualTokens #andTokens #upArrowTokens #orTokens
	     #periodPeriodPeriodTokens #periodPeriodPeriodQuestionMarkTokens
	     #ifTokens #elseTokens #forTokens #bangEqualTokens
	     #mULTI_LINE_STRING_SQ_MID_MIDTokens
	     #mULTI_LINE_STRING_DQ_MID_MIDTokens
	     #sINGLE_LINE_STRING_SQ_MID_MIDTokens
	     #sINGLE_LINE_STRING_DQ_MID_MIDTokens
	     #inTokens #semicolonTokens #isTokens #plusPlusTokens
	     #minusMinusTokens #importTokens #showTokens #hideTokens
	     #identifierTokens #asyncTokens #ofTokens #onTokens #syncTokens
	     #awaitTokens #yieldTokens #functionTokens #commaTokens
	     #abstractTokens #asTokens #covariantTokens #deferredTokens
	     #dynamicTokens #exportTokens #extensionTokens #externalTokens
	     #factoryTokens #getTokens #implementsTokens #interfaceTokens
	     #lateTokens #libraryTokens #operatorTokens #mixinTokens
	     #partTokens #requiredTokens #setTokens #staticTokens
	     #typedefTokens )
]

{ #category : #generated }
DartExportDeclarationNode >> constTokens [

	^ constTokens
]

{ #category : #generated }
DartExportDeclarationNode >> constTokens: anOrderedCollection [

	constTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> divideEqualTokens [

	^ divideEqualTokens
]

{ #category : #generated }
DartExportDeclarationNode >> divideEqualTokens: anOrderedCollection [

	divideEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> divideTokens [

	^ divideTokens
]

{ #category : #generated }
DartExportDeclarationNode >> divideTokens: anOrderedCollection [

	divideTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> elseTokens [

	^ elseTokens
]

{ #category : #generated }
DartExportDeclarationNode >> elseTokens: anOrderedCollection [

	elseTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> equalEqualTokens [

	^ equalEqualTokens
]

{ #category : #generated }
DartExportDeclarationNode >> equalEqualTokens: anOrderedCollection [

	equalEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> equalGreaterThanTokens [

	^ equalGreaterThanTokens
]

{ #category : #generated }
DartExportDeclarationNode >> equalGreaterThanTokens: anOrderedCollection [

	equalGreaterThanTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> equalTokens [

	^ equalTokens
]

{ #category : #generated }
DartExportDeclarationNode >> equalTokens: anOrderedCollection [

	equalTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> extendsTokens [

	^ extendsTokens
]

{ #category : #generated }
DartExportDeclarationNode >> extendsTokens: anOrderedCollection [

	extendsTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> falseTokens [

	^ falseTokens
]

{ #category : #generated }
DartExportDeclarationNode >> falseTokens: anOrderedCollection [

	falseTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> finalTokens [

	^ finalTokens
]

{ #category : #generated }
DartExportDeclarationNode >> finalTokens: anOrderedCollection [

	finalTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> forTokens [

	^ forTokens
]

{ #category : #generated }
DartExportDeclarationNode >> forTokens: anOrderedCollection [

	forTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> greaterThanTokens [

	^ greaterThanTokens
]

{ #category : #generated }
DartExportDeclarationNode >> greaterThanTokens: anOrderedCollection [

	greaterThanTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> hEX_NUMBERTokens [

	^ hEX_NUMBERTokens
]

{ #category : #generated }
DartExportDeclarationNode >> hEX_NUMBERTokens: anOrderedCollection [

	hEX_NUMBERTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> ifTokens [

	^ ifTokens
]

{ #category : #generated }
DartExportDeclarationNode >> ifTokens: anOrderedCollection [

	ifTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> inTokens [

	^ inTokens
]

{ #category : #generated }
DartExportDeclarationNode >> inTokens: anOrderedCollection [

	inTokens := anOrderedCollection
]

{ #category : #'generated-initialize-release' }
DartExportDeclarationNode >> initialize [
	super initialize.
	_Tokens := OrderedCollection new: 2.
	periodTokens := OrderedCollection new: 2.
	newTokens := OrderedCollection new: 2.
	lessThanTokens := OrderedCollection new: 2.
	greaterThanTokens := OrderedCollection new: 2.
	leftParenTokens := OrderedCollection new: 2.
	rightParenTokens := OrderedCollection new: 2.
	questionMarkTokens := OrderedCollection new: 2.
	voidTokens := OrderedCollection new: 2.
	colonTokens := OrderedCollection new: 2.
	throwTokens := OrderedCollection new: 2.
	superTokens := OrderedCollection new: 2.
	equalTokens := OrderedCollection new: 2.
	periodPeriodTokens := OrderedCollection new: 2.
	questionMarkPeriodPeriodTokens := OrderedCollection new: 2.
	extendsTokens := OrderedCollection new: 2.
	equalGreaterThanTokens := OrderedCollection new: 2.
	leftBracketTokens := OrderedCollection new: 2.
	rightBracketTokens := OrderedCollection new: 2.
	thisTokens := OrderedCollection new: 2.
	constTokens := OrderedCollection new: 2.
	bangTokens := OrderedCollection new: 2.
	questionMarkPeriodTokens := OrderedCollection new: 2.
	timesEqualTokens := OrderedCollection new: 2.
	divideEqualTokens := OrderedCollection new: 2.
	tildeDivideEqualTokens := OrderedCollection new: 2.
	percentEqualTokens := OrderedCollection new: 2.
	plusEqualTokens := OrderedCollection new: 2.
	minusEqualTokens := OrderedCollection new: 2.
	lessThanLessThanEqualTokens := OrderedCollection new: 2.
	andEqualTokens := OrderedCollection new: 2.
	upArrowEqualTokens := OrderedCollection new: 2.
	orEqualTokens := OrderedCollection new: 2.
	questionMarkQuestionMarkEqualTokens := OrderedCollection new: 2.
	questionMarkQuestionMarkTokens := OrderedCollection new: 2.
	lbraceTokens := OrderedCollection new: 2.
	rbraceTokens := OrderedCollection new: 2.
	statementses := OrderedCollection new: 2.
	timesTokens := OrderedCollection new: 2.
	nullTokens := OrderedCollection new: 2.
	trueTokens := OrderedCollection new: 2.
	falseTokens := OrderedCollection new: 2.
	numberTokens := OrderedCollection new: 2.
	hEX_NUMBERTokens := OrderedCollection new: 2.
	tildeTokens := OrderedCollection new: 2.
	orOrTokens := OrderedCollection new: 2.
	equalEqualTokens := OrderedCollection new: 2.
	finalTokens := OrderedCollection new: 2.
	andAndTokens := OrderedCollection new: 2.
	varTokens := OrderedCollection new: 2.
	rAW_MULTI_LINE_STRINGTokens := OrderedCollection new: 2.
	mULTI_LINE_STRING_SQ_BEGIN_ENDTokens := OrderedCollection new: 2.
	mULTI_LINE_STRING_SQ_BEGIN_MIDTokens := OrderedCollection new: 2.
	mULTI_LINE_STRING_SQ_MID_ENDTokens := OrderedCollection new: 2.
	mULTI_LINE_STRING_DQ_BEGIN_ENDTokens := OrderedCollection new: 2.
	mULTI_LINE_STRING_DQ_BEGIN_MIDTokens := OrderedCollection new: 2.
	mULTI_LINE_STRING_DQ_MID_ENDTokens := OrderedCollection new: 2.
	rAW_SINGLE_LINE_STRINGTokens := OrderedCollection new: 2.
	sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens := OrderedCollection new: 2.
	sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens := OrderedCollection new: 2.
	sINGLE_LINE_STRING_SQ_MID_ENDTokens := OrderedCollection new: 2.
	sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens := OrderedCollection new: 2.
	sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens := OrderedCollection new: 2.
	sINGLE_LINE_STRING_DQ_MID_ENDTokens := OrderedCollection new: 2.
	divideTokens := OrderedCollection new: 2.
	percentTokens := OrderedCollection new: 2.
	tildeDivideTokens := OrderedCollection new: 2.
	plusTokens := OrderedCollection new: 2.
	minusTokens := OrderedCollection new: 2.
	lessThanLessThanTokens := OrderedCollection new: 2.
	lessThanEqualTokens := OrderedCollection new: 2.
	andTokens := OrderedCollection new: 2.
	upArrowTokens := OrderedCollection new: 2.
	orTokens := OrderedCollection new: 2.
	periodPeriodPeriodTokens := OrderedCollection new: 2.
	periodPeriodPeriodQuestionMarkTokens := OrderedCollection new: 2.
	ifTokens := OrderedCollection new: 2.
	elseTokens := OrderedCollection new: 2.
	forTokens := OrderedCollection new: 2.
	bangEqualTokens := OrderedCollection new: 2.
	mULTI_LINE_STRING_SQ_MID_MIDTokens := OrderedCollection new: 2.
	mULTI_LINE_STRING_DQ_MID_MIDTokens := OrderedCollection new: 2.
	sINGLE_LINE_STRING_SQ_MID_MIDTokens := OrderedCollection new: 2.
	sINGLE_LINE_STRING_DQ_MID_MIDTokens := OrderedCollection new: 2.
	inTokens := OrderedCollection new: 2.
	semicolonTokens := OrderedCollection new: 2.
	isTokens := OrderedCollection new: 2.
	plusPlusTokens := OrderedCollection new: 2.
	minusMinusTokens := OrderedCollection new: 2.
]

{ #category : #generated }
DartExportDeclarationNode >> isTokens [

	^ isTokens
]

{ #category : #generated }
DartExportDeclarationNode >> isTokens: anOrderedCollection [

	isTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> lbraceTokens [

	^ lbraceTokens
]

{ #category : #generated }
DartExportDeclarationNode >> lbraceTokens: anOrderedCollection [

	lbraceTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> leftBracketTokens [

	^ leftBracketTokens
]

{ #category : #generated }
DartExportDeclarationNode >> leftBracketTokens: anOrderedCollection [

	leftBracketTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> leftParenTokens [

	^ leftParenTokens
]

{ #category : #generated }
DartExportDeclarationNode >> leftParenTokens: anOrderedCollection [

	leftParenTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> lessThanEqualTokens [

	^ lessThanEqualTokens
]

{ #category : #generated }
DartExportDeclarationNode >> lessThanEqualTokens: anOrderedCollection [

	lessThanEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> lessThanLessThanEqualTokens [

	^ lessThanLessThanEqualTokens
]

{ #category : #generated }
DartExportDeclarationNode >> lessThanLessThanEqualTokens: anOrderedCollection [

	lessThanLessThanEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> lessThanLessThanTokens [

	^ lessThanLessThanTokens
]

{ #category : #generated }
DartExportDeclarationNode >> lessThanLessThanTokens: anOrderedCollection [

	lessThanLessThanTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> lessThanTokens [

	^ lessThanTokens
]

{ #category : #generated }
DartExportDeclarationNode >> lessThanTokens: anOrderedCollection [

	lessThanTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> mULTI_LINE_STRING_DQ_BEGIN_ENDTokens [

	^ mULTI_LINE_STRING_DQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartExportDeclarationNode >> mULTI_LINE_STRING_DQ_BEGIN_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> mULTI_LINE_STRING_DQ_BEGIN_MIDTokens [

	^ mULTI_LINE_STRING_DQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartExportDeclarationNode >> mULTI_LINE_STRING_DQ_BEGIN_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> mULTI_LINE_STRING_DQ_MID_ENDTokens [

	^ mULTI_LINE_STRING_DQ_MID_ENDTokens
]

{ #category : #generated }
DartExportDeclarationNode >> mULTI_LINE_STRING_DQ_MID_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> mULTI_LINE_STRING_DQ_MID_MIDTokens [

	^ mULTI_LINE_STRING_DQ_MID_MIDTokens
]

{ #category : #generated }
DartExportDeclarationNode >> mULTI_LINE_STRING_DQ_MID_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> mULTI_LINE_STRING_SQ_BEGIN_ENDTokens [

	^ mULTI_LINE_STRING_SQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartExportDeclarationNode >> mULTI_LINE_STRING_SQ_BEGIN_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> mULTI_LINE_STRING_SQ_BEGIN_MIDTokens [

	^ mULTI_LINE_STRING_SQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartExportDeclarationNode >> mULTI_LINE_STRING_SQ_BEGIN_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> mULTI_LINE_STRING_SQ_MID_ENDTokens [

	^ mULTI_LINE_STRING_SQ_MID_ENDTokens
]

{ #category : #generated }
DartExportDeclarationNode >> mULTI_LINE_STRING_SQ_MID_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> mULTI_LINE_STRING_SQ_MID_MIDTokens [

	^ mULTI_LINE_STRING_SQ_MID_MIDTokens
]

{ #category : #generated }
DartExportDeclarationNode >> mULTI_LINE_STRING_SQ_MID_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> minusEqualTokens [

	^ minusEqualTokens
]

{ #category : #generated }
DartExportDeclarationNode >> minusEqualTokens: anOrderedCollection [

	minusEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> minusMinusTokens [

	^ minusMinusTokens
]

{ #category : #generated }
DartExportDeclarationNode >> minusMinusTokens: anOrderedCollection [

	minusMinusTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> minusTokens [

	^ minusTokens
]

{ #category : #generated }
DartExportDeclarationNode >> minusTokens: anOrderedCollection [

	minusTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> newTokens [

	^ newTokens
]

{ #category : #generated }
DartExportDeclarationNode >> newTokens: anOrderedCollection [

	newTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> nullTokens [

	^ nullTokens
]

{ #category : #generated }
DartExportDeclarationNode >> nullTokens: anOrderedCollection [

	nullTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> numberTokens [

	^ numberTokens
]

{ #category : #generated }
DartExportDeclarationNode >> numberTokens: anOrderedCollection [

	numberTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> orEqualTokens [

	^ orEqualTokens
]

{ #category : #generated }
DartExportDeclarationNode >> orEqualTokens: anOrderedCollection [

	orEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> orOrTokens [

	^ orOrTokens
]

{ #category : #generated }
DartExportDeclarationNode >> orOrTokens: anOrderedCollection [

	orOrTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> orTokens [

	^ orTokens
]

{ #category : #generated }
DartExportDeclarationNode >> orTokens: anOrderedCollection [

	orTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> otherVariables [

	^ #( #statementses )
]

{ #category : #generated }
DartExportDeclarationNode >> percentEqualTokens [

	^ percentEqualTokens
]

{ #category : #generated }
DartExportDeclarationNode >> percentEqualTokens: anOrderedCollection [

	percentEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> percentTokens [

	^ percentTokens
]

{ #category : #generated }
DartExportDeclarationNode >> percentTokens: anOrderedCollection [

	percentTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> periodPeriodPeriodQuestionMarkTokens [

	^ periodPeriodPeriodQuestionMarkTokens
]

{ #category : #generated }
DartExportDeclarationNode >> periodPeriodPeriodQuestionMarkTokens: anOrderedCollection [

	periodPeriodPeriodQuestionMarkTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> periodPeriodPeriodTokens [

	^ periodPeriodPeriodTokens
]

{ #category : #generated }
DartExportDeclarationNode >> periodPeriodPeriodTokens: anOrderedCollection [

	periodPeriodPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> periodPeriodTokens [

	^ periodPeriodTokens
]

{ #category : #generated }
DartExportDeclarationNode >> periodPeriodTokens: anOrderedCollection [

	periodPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> periodTokens [

	^ periodTokens
]

{ #category : #generated }
DartExportDeclarationNode >> periodTokens: anOrderedCollection [

	periodTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> plusEqualTokens [

	^ plusEqualTokens
]

{ #category : #generated }
DartExportDeclarationNode >> plusEqualTokens: anOrderedCollection [

	plusEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> plusPlusTokens [

	^ plusPlusTokens
]

{ #category : #generated }
DartExportDeclarationNode >> plusPlusTokens: anOrderedCollection [

	plusPlusTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> plusTokens [

	^ plusTokens
]

{ #category : #generated }
DartExportDeclarationNode >> plusTokens: anOrderedCollection [

	plusTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> questionMarkPeriodPeriodTokens [

	^ questionMarkPeriodPeriodTokens
]

{ #category : #generated }
DartExportDeclarationNode >> questionMarkPeriodPeriodTokens: anOrderedCollection [

	questionMarkPeriodPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> questionMarkPeriodTokens [

	^ questionMarkPeriodTokens
]

{ #category : #generated }
DartExportDeclarationNode >> questionMarkPeriodTokens: anOrderedCollection [

	questionMarkPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> questionMarkQuestionMarkEqualTokens [

	^ questionMarkQuestionMarkEqualTokens
]

{ #category : #generated }
DartExportDeclarationNode >> questionMarkQuestionMarkEqualTokens: anOrderedCollection [

	questionMarkQuestionMarkEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> questionMarkQuestionMarkTokens [

	^ questionMarkQuestionMarkTokens
]

{ #category : #generated }
DartExportDeclarationNode >> questionMarkQuestionMarkTokens: anOrderedCollection [

	questionMarkQuestionMarkTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> questionMarkTokens [

	^ questionMarkTokens
]

{ #category : #generated }
DartExportDeclarationNode >> questionMarkTokens: anOrderedCollection [

	questionMarkTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> rAW_MULTI_LINE_STRINGTokens [

	^ rAW_MULTI_LINE_STRINGTokens
]

{ #category : #generated }
DartExportDeclarationNode >> rAW_MULTI_LINE_STRINGTokens: anOrderedCollection [

	rAW_MULTI_LINE_STRINGTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> rAW_SINGLE_LINE_STRINGTokens [

	^ rAW_SINGLE_LINE_STRINGTokens
]

{ #category : #generated }
DartExportDeclarationNode >> rAW_SINGLE_LINE_STRINGTokens: anOrderedCollection [

	rAW_SINGLE_LINE_STRINGTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> rbraceTokens [

	^ rbraceTokens
]

{ #category : #generated }
DartExportDeclarationNode >> rbraceTokens: anOrderedCollection [

	rbraceTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> rightBracketTokens [

	^ rightBracketTokens
]

{ #category : #generated }
DartExportDeclarationNode >> rightBracketTokens: anOrderedCollection [

	rightBracketTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> rightParenTokens [

	^ rightParenTokens
]

{ #category : #generated }
DartExportDeclarationNode >> rightParenTokens: anOrderedCollection [

	rightParenTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens [

	^ sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartExportDeclarationNode >> sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens [

	^ sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartExportDeclarationNode >> sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> sINGLE_LINE_STRING_DQ_MID_ENDTokens [

	^ sINGLE_LINE_STRING_DQ_MID_ENDTokens
]

{ #category : #generated }
DartExportDeclarationNode >> sINGLE_LINE_STRING_DQ_MID_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> sINGLE_LINE_STRING_DQ_MID_MIDTokens [

	^ sINGLE_LINE_STRING_DQ_MID_MIDTokens
]

{ #category : #generated }
DartExportDeclarationNode >> sINGLE_LINE_STRING_DQ_MID_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens [

	^ sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartExportDeclarationNode >> sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens [

	^ sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartExportDeclarationNode >> sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> sINGLE_LINE_STRING_SQ_MID_ENDTokens [

	^ sINGLE_LINE_STRING_SQ_MID_ENDTokens
]

{ #category : #generated }
DartExportDeclarationNode >> sINGLE_LINE_STRING_SQ_MID_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> sINGLE_LINE_STRING_SQ_MID_MIDTokens [

	^ sINGLE_LINE_STRING_SQ_MID_MIDTokens
]

{ #category : #generated }
DartExportDeclarationNode >> sINGLE_LINE_STRING_SQ_MID_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> semicolonTokens [

	^ semicolonTokens
]

{ #category : #generated }
DartExportDeclarationNode >> semicolonTokens: anOrderedCollection [

	semicolonTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> statementses [

	^ statementses
]

{ #category : #generated }
DartExportDeclarationNode >> statementses: anOrderedCollection [

	self setParent: self statementses to: nil.
	statementses := anOrderedCollection.
	self setParent: self statementses to: self
]

{ #category : #generated }
DartExportDeclarationNode >> stringuri [

	^ stringuri
]

{ #category : #generated }
DartExportDeclarationNode >> stringuri: aSmaCCToken [

	stringuri := aSmaCCToken
]

{ #category : #generated }
DartExportDeclarationNode >> superTokens [

	^ superTokens
]

{ #category : #generated }
DartExportDeclarationNode >> superTokens: anOrderedCollection [

	superTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> thisTokens [

	^ thisTokens
]

{ #category : #generated }
DartExportDeclarationNode >> thisTokens: anOrderedCollection [

	thisTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> throwTokens [

	^ throwTokens
]

{ #category : #generated }
DartExportDeclarationNode >> throwTokens: anOrderedCollection [

	throwTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> tildeDivideEqualTokens [

	^ tildeDivideEqualTokens
]

{ #category : #generated }
DartExportDeclarationNode >> tildeDivideEqualTokens: anOrderedCollection [

	tildeDivideEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> tildeDivideTokens [

	^ tildeDivideTokens
]

{ #category : #generated }
DartExportDeclarationNode >> tildeDivideTokens: anOrderedCollection [

	tildeDivideTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> tildeTokens [

	^ tildeTokens
]

{ #category : #generated }
DartExportDeclarationNode >> tildeTokens: anOrderedCollection [

	tildeTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> timesEqualTokens [

	^ timesEqualTokens
]

{ #category : #generated }
DartExportDeclarationNode >> timesEqualTokens: anOrderedCollection [

	timesEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> timesTokens [

	^ timesTokens
]

{ #category : #generated }
DartExportDeclarationNode >> timesTokens: anOrderedCollection [

	timesTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> tokenVariables [

	^ #( #stringuri )
]

{ #category : #generated }
DartExportDeclarationNode >> trueTokens [

	^ trueTokens
]

{ #category : #generated }
DartExportDeclarationNode >> trueTokens: anOrderedCollection [

	trueTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> upArrowEqualTokens [

	^ upArrowEqualTokens
]

{ #category : #generated }
DartExportDeclarationNode >> upArrowEqualTokens: anOrderedCollection [

	upArrowEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> upArrowTokens [

	^ upArrowTokens
]

{ #category : #generated }
DartExportDeclarationNode >> upArrowTokens: anOrderedCollection [

	upArrowTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> varTokens [

	^ varTokens
]

{ #category : #generated }
DartExportDeclarationNode >> varTokens: anOrderedCollection [

	varTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> voidTokens [

	^ voidTokens
]

{ #category : #generated }
DartExportDeclarationNode >> voidTokens: anOrderedCollection [

	voidTokens := anOrderedCollection
]
