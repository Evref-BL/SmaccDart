Class {
	#name : #DartExportDeclarationNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'_Tokens',
		'identifierTokens',
		'dynamicTokens',
		'asyncTokens',
		'hideTokens',
		'ofTokens',
		'onTokens',
		'showTokens',
		'syncTokens',
		'awaitTokens',
		'yieldTokens',
		'periodTokens',
		'functionTokens',
		'newTokens',
		'lessThanTokens',
		'greaterThanTokens',
		'leftParenTokens',
		'rightParenTokens',
		'commaTokens',
		'questionMarkTokens',
		'voidTokens',
		'abstractTokens',
		'asTokens',
		'covariantTokens',
		'deferredTokens',
		'exportTokens',
		'extensionTokens',
		'externalTokens',
		'factoryTokens',
		'getTokens',
		'implementsTokens',
		'importTokens',
		'interfaceTokens',
		'lateTokens',
		'libraryTokens',
		'operatorTokens',
		'mixinTokens',
		'partTokens',
		'requiredTokens',
		'setTokens',
		'staticTokens',
		'typedefTokens',
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
		'equalEqualTokens',
		'orOrTokens',
		'andAndTokens',
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
		'bangEqualTokens',
		'varTokens',
		'mULTI_LINE_STRING_SQ_MID_MIDTokens',
		'mULTI_LINE_STRING_DQ_MID_MIDTokens',
		'sINGLE_LINE_STRING_SQ_MID_MIDTokens',
		'sINGLE_LINE_STRING_DQ_MID_MIDTokens',
		'inTokens',
		'semicolonTokens',
		'isTokens',
		'plusPlusTokens',
		'minusMinusTokens',
		'stringLiteralWithoutInterpolation'
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
DartExportDeclarationNode >> abstractTokens [

	^ abstractTokens
]

{ #category : #generated }
DartExportDeclarationNode >> abstractTokens: anOrderedCollection [

	abstractTokens := anOrderedCollection
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
DartExportDeclarationNode >> asTokens [

	^ asTokens
]

{ #category : #generated }
DartExportDeclarationNode >> asTokens: anOrderedCollection [

	asTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> asyncTokens [

	^ asyncTokens
]

{ #category : #generated }
DartExportDeclarationNode >> asyncTokens: anOrderedCollection [

	asyncTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> awaitTokens [

	^ awaitTokens
]

{ #category : #generated }
DartExportDeclarationNode >> awaitTokens: anOrderedCollection [

	awaitTokens := anOrderedCollection
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
DartExportDeclarationNode >> commaTokens [

	^ commaTokens
]

{ #category : #generated }
DartExportDeclarationNode >> commaTokens: anOrderedCollection [

	commaTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> compositeTokenVariables [

	^ #( #_Tokens #identifierTokens #dynamicTokens #asyncTokens
	     #hideTokens #ofTokens #onTokens #showTokens #syncTokens
	     #awaitTokens #yieldTokens #periodTokens #functionTokens
	     #newTokens #lessThanTokens #greaterThanTokens #leftParenTokens
	     #rightParenTokens #commaTokens #questionMarkTokens
	     #voidTokens #abstractTokens #asTokens #covariantTokens
	     #deferredTokens #exportTokens #extensionTokens #externalTokens
	     #factoryTokens #getTokens #implementsTokens #importTokens
	     #interfaceTokens #lateTokens #libraryTokens #operatorTokens
	     #mixinTokens #partTokens #requiredTokens #setTokens #staticTokens
	     #typedefTokens #colonTokens #throwTokens #superTokens
	     #equalTokens #periodPeriodTokens #questionMarkPeriodPeriodTokens
	     #extendsTokens #equalGreaterThanTokens #leftBracketTokens
	     #rightBracketTokens #thisTokens #constTokens #bangTokens
	     #questionMarkPeriodTokens #timesEqualTokens #divideEqualTokens
	     #tildeDivideEqualTokens #percentEqualTokens #plusEqualTokens
	     #minusEqualTokens #lessThanLessThanEqualTokens
	     #andEqualTokens #upArrowEqualTokens #orEqualTokens
	     #questionMarkQuestionMarkEqualTokens
	     #questionMarkQuestionMarkTokens #lbraceTokens #rbraceTokens
	     #timesTokens #nullTokens #trueTokens #falseTokens #numberTokens
	     #hEX_NUMBERTokens #tildeTokens #equalEqualTokens
	     #orOrTokens #andAndTokens #finalTokens #rAW_MULTI_LINE_STRINGTokens
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
	     #varTokens #mULTI_LINE_STRING_SQ_MID_MIDTokens
	     #mULTI_LINE_STRING_DQ_MID_MIDTokens
	     #sINGLE_LINE_STRING_SQ_MID_MIDTokens
	     #sINGLE_LINE_STRING_DQ_MID_MIDTokens
	     #inTokens #semicolonTokens #isTokens #plusPlusTokens
	     #minusMinusTokens )
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
DartExportDeclarationNode >> covariantTokens [

	^ covariantTokens
]

{ #category : #generated }
DartExportDeclarationNode >> covariantTokens: anOrderedCollection [

	covariantTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> deferredTokens [

	^ deferredTokens
]

{ #category : #generated }
DartExportDeclarationNode >> deferredTokens: anOrderedCollection [

	deferredTokens := anOrderedCollection
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
DartExportDeclarationNode >> dynamicTokens [

	^ dynamicTokens
]

{ #category : #generated }
DartExportDeclarationNode >> dynamicTokens: anOrderedCollection [

	dynamicTokens := anOrderedCollection
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
DartExportDeclarationNode >> exportTokens [

	^ exportTokens
]

{ #category : #generated }
DartExportDeclarationNode >> exportTokens: anOrderedCollection [

	exportTokens := anOrderedCollection
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
DartExportDeclarationNode >> extensionTokens [

	^ extensionTokens
]

{ #category : #generated }
DartExportDeclarationNode >> extensionTokens: anOrderedCollection [

	extensionTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> externalTokens [

	^ externalTokens
]

{ #category : #generated }
DartExportDeclarationNode >> externalTokens: anOrderedCollection [

	externalTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> factoryTokens [

	^ factoryTokens
]

{ #category : #generated }
DartExportDeclarationNode >> factoryTokens: anOrderedCollection [

	factoryTokens := anOrderedCollection
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
DartExportDeclarationNode >> functionTokens [

	^ functionTokens
]

{ #category : #generated }
DartExportDeclarationNode >> functionTokens: anOrderedCollection [

	functionTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> getTokens [

	^ getTokens
]

{ #category : #generated }
DartExportDeclarationNode >> getTokens: anOrderedCollection [

	getTokens := anOrderedCollection
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
DartExportDeclarationNode >> hideTokens [

	^ hideTokens
]

{ #category : #generated }
DartExportDeclarationNode >> hideTokens: anOrderedCollection [

	hideTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> identifierTokens [

	^ identifierTokens
]

{ #category : #generated }
DartExportDeclarationNode >> identifierTokens: anOrderedCollection [

	identifierTokens := anOrderedCollection
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
DartExportDeclarationNode >> implementsTokens [

	^ implementsTokens
]

{ #category : #generated }
DartExportDeclarationNode >> implementsTokens: anOrderedCollection [

	implementsTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> importTokens [

	^ importTokens
]

{ #category : #generated }
DartExportDeclarationNode >> importTokens: anOrderedCollection [

	importTokens := anOrderedCollection
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
	identifierTokens := OrderedCollection new: 2.
	dynamicTokens := OrderedCollection new: 2.
	asyncTokens := OrderedCollection new: 2.
	hideTokens := OrderedCollection new: 2.
	ofTokens := OrderedCollection new: 2.
	onTokens := OrderedCollection new: 2.
	showTokens := OrderedCollection new: 2.
	syncTokens := OrderedCollection new: 2.
	awaitTokens := OrderedCollection new: 2.
	yieldTokens := OrderedCollection new: 2.
	periodTokens := OrderedCollection new: 2.
	functionTokens := OrderedCollection new: 2.
	newTokens := OrderedCollection new: 2.
	lessThanTokens := OrderedCollection new: 2.
	greaterThanTokens := OrderedCollection new: 2.
	leftParenTokens := OrderedCollection new: 2.
	rightParenTokens := OrderedCollection new: 2.
	commaTokens := OrderedCollection new: 2.
	questionMarkTokens := OrderedCollection new: 2.
	voidTokens := OrderedCollection new: 2.
	abstractTokens := OrderedCollection new: 2.
	asTokens := OrderedCollection new: 2.
	covariantTokens := OrderedCollection new: 2.
	deferredTokens := OrderedCollection new: 2.
	exportTokens := OrderedCollection new: 2.
	extensionTokens := OrderedCollection new: 2.
	externalTokens := OrderedCollection new: 2.
	factoryTokens := OrderedCollection new: 2.
	getTokens := OrderedCollection new: 2.
	implementsTokens := OrderedCollection new: 2.
	importTokens := OrderedCollection new: 2.
	interfaceTokens := OrderedCollection new: 2.
	lateTokens := OrderedCollection new: 2.
	libraryTokens := OrderedCollection new: 2.
	operatorTokens := OrderedCollection new: 2.
	mixinTokens := OrderedCollection new: 2.
	partTokens := OrderedCollection new: 2.
	requiredTokens := OrderedCollection new: 2.
	setTokens := OrderedCollection new: 2.
	staticTokens := OrderedCollection new: 2.
	typedefTokens := OrderedCollection new: 2.
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
	equalEqualTokens := OrderedCollection new: 2.
	orOrTokens := OrderedCollection new: 2.
	andAndTokens := OrderedCollection new: 2.
	finalTokens := OrderedCollection new: 2.
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
	varTokens := OrderedCollection new: 2.
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
DartExportDeclarationNode >> interfaceTokens [

	^ interfaceTokens
]

{ #category : #generated }
DartExportDeclarationNode >> interfaceTokens: anOrderedCollection [

	interfaceTokens := anOrderedCollection
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
DartExportDeclarationNode >> lateTokens [

	^ lateTokens
]

{ #category : #generated }
DartExportDeclarationNode >> lateTokens: anOrderedCollection [

	lateTokens := anOrderedCollection
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
DartExportDeclarationNode >> libraryTokens [

	^ libraryTokens
]

{ #category : #generated }
DartExportDeclarationNode >> libraryTokens: anOrderedCollection [

	libraryTokens := anOrderedCollection
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
DartExportDeclarationNode >> mixinTokens [

	^ mixinTokens
]

{ #category : #generated }
DartExportDeclarationNode >> mixinTokens: anOrderedCollection [

	mixinTokens := anOrderedCollection
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
DartExportDeclarationNode >> ofTokens [

	^ ofTokens
]

{ #category : #generated }
DartExportDeclarationNode >> ofTokens: anOrderedCollection [

	ofTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> onTokens [

	^ onTokens
]

{ #category : #generated }
DartExportDeclarationNode >> onTokens: anOrderedCollection [

	onTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> operatorTokens [

	^ operatorTokens
]

{ #category : #generated }
DartExportDeclarationNode >> operatorTokens: anOrderedCollection [

	operatorTokens := anOrderedCollection
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
DartExportDeclarationNode >> partTokens [

	^ partTokens
]

{ #category : #generated }
DartExportDeclarationNode >> partTokens: anOrderedCollection [

	partTokens := anOrderedCollection
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
DartExportDeclarationNode >> requiredTokens [

	^ requiredTokens
]

{ #category : #generated }
DartExportDeclarationNode >> requiredTokens: anOrderedCollection [

	requiredTokens := anOrderedCollection
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
DartExportDeclarationNode >> setTokens [

	^ setTokens
]

{ #category : #generated }
DartExportDeclarationNode >> setTokens: anOrderedCollection [

	setTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> showTokens [

	^ showTokens
]

{ #category : #generated }
DartExportDeclarationNode >> showTokens: anOrderedCollection [

	showTokens := anOrderedCollection
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
DartExportDeclarationNode >> staticTokens [

	^ staticTokens
]

{ #category : #generated }
DartExportDeclarationNode >> staticTokens: anOrderedCollection [

	staticTokens := anOrderedCollection
]

{ #category : #generated }
DartExportDeclarationNode >> stringLiteralWithoutInterpolation [

	^ stringLiteralWithoutInterpolation
]

{ #category : #generated }
DartExportDeclarationNode >> stringLiteralWithoutInterpolation: aSmaCCToken [

	stringLiteralWithoutInterpolation := aSmaCCToken
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
DartExportDeclarationNode >> syncTokens [

	^ syncTokens
]

{ #category : #generated }
DartExportDeclarationNode >> syncTokens: anOrderedCollection [

	syncTokens := anOrderedCollection
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

	^ #( #stringLiteralWithoutInterpolation )
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
DartExportDeclarationNode >> typedefTokens [

	^ typedefTokens
]

{ #category : #generated }
DartExportDeclarationNode >> typedefTokens: anOrderedCollection [

	typedefTokens := anOrderedCollection
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

{ #category : #generated }
DartExportDeclarationNode >> yieldTokens [

	^ yieldTokens
]

{ #category : #generated }
DartExportDeclarationNode >> yieldTokens: anOrderedCollection [

	yieldTokens := anOrderedCollection
]
