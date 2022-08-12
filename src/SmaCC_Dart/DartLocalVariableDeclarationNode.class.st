Class {
	#name : #DartLocalVariableDeclarationNode,
	#superclass : #DartStatementNode,
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
		'orOrTokens',
		'equalEqualTokens',
		'finalTokens',
		'andAndTokens',
		'varTokens',
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
		'mULTI_LINE_STRING_SQ_MID_MIDTokens',
		'mULTI_LINE_STRING_DQ_MID_MIDTokens',
		'sINGLE_LINE_STRING_SQ_MID_MIDTokens',
		'sINGLE_LINE_STRING_DQ_MID_MIDTokens',
		'inTokens',
		'semicolonTokens',
		'isTokens',
		'plusPlusTokens',
		'minusMinusTokens'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartLocalVariableDeclarationNode >> _Tokens [

	^ _Tokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> _Tokens: anOrderedCollection [

	_Tokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> abstractTokens [

	^ abstractTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> abstractTokens: anOrderedCollection [

	abstractTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitLocalVariableDeclaration: self
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> andAndTokens [

	^ andAndTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> andAndTokens: anOrderedCollection [

	andAndTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> andEqualTokens [

	^ andEqualTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> andEqualTokens: anOrderedCollection [

	andEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> andTokens [

	^ andTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> andTokens: anOrderedCollection [

	andTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> asTokens [

	^ asTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> asTokens: anOrderedCollection [

	asTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> asyncTokens [

	^ asyncTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> asyncTokens: anOrderedCollection [

	asyncTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> awaitTokens [

	^ awaitTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> awaitTokens: anOrderedCollection [

	awaitTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> bangEqualTokens [

	^ bangEqualTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> bangEqualTokens: anOrderedCollection [

	bangEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> bangTokens [

	^ bangTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> bangTokens: anOrderedCollection [

	bangTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> colonTokens [

	^ colonTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> colonTokens: anOrderedCollection [

	colonTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> commaTokens [

	^ commaTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> commaTokens: anOrderedCollection [

	commaTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> compositeTokenVariables [

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
	     #hEX_NUMBERTokens #tildeTokens #orOrTokens #equalEqualTokens
	     #finalTokens #andAndTokens #varTokens #rAW_MULTI_LINE_STRINGTokens
	     #mULTI_LINE_STRING_SQ_BEGIN_ENDTokens
	     #mULTI_LINE_STRING_SQ_BEGIN_MIDTokens
	     #mULTI_LINE_STRING_SQ_MID_MIDTokens
	     #mULTI_LINE_STRING_SQ_MID_ENDTokens
	     #mULTI_LINE_STRING_DQ_BEGIN_ENDTokens
	     #mULTI_LINE_STRING_DQ_BEGIN_MIDTokens
	     #mULTI_LINE_STRING_DQ_MID_MIDTokens
	     #mULTI_LINE_STRING_DQ_MID_ENDTokens
	     #rAW_SINGLE_LINE_STRINGTokens #sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens
	     #sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens
	     #sINGLE_LINE_STRING_SQ_MID_MIDTokens
	     #sINGLE_LINE_STRING_SQ_MID_ENDTokens
	     #sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens
	     #sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens
	     #sINGLE_LINE_STRING_DQ_MID_MIDTokens
	     #sINGLE_LINE_STRING_DQ_MID_ENDTokens
	     #divideTokens #percentTokens #tildeDivideTokens
	     #plusTokens #minusTokens #lessThanLessThanTokens
	     #lessThanEqualTokens #andTokens #upArrowTokens #orTokens
	     #periodPeriodPeriodTokens #periodPeriodPeriodQuestionMarkTokens
	     #ifTokens #elseTokens #forTokens #inTokens #semicolonTokens
	     #bangEqualTokens #isTokens #plusPlusTokens #minusMinusTokens )
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> constTokens [

	^ constTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> constTokens: anOrderedCollection [

	constTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> covariantTokens [

	^ covariantTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> covariantTokens: anOrderedCollection [

	covariantTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> deferredTokens [

	^ deferredTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> deferredTokens: anOrderedCollection [

	deferredTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> divideEqualTokens [

	^ divideEqualTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> divideEqualTokens: anOrderedCollection [

	divideEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> divideTokens [

	^ divideTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> divideTokens: anOrderedCollection [

	divideTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> dynamicTokens [

	^ dynamicTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> dynamicTokens: anOrderedCollection [

	dynamicTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> elseTokens [

	^ elseTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> elseTokens: anOrderedCollection [

	elseTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> equalEqualTokens [

	^ equalEqualTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> equalEqualTokens: anOrderedCollection [

	equalEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> equalGreaterThanTokens [

	^ equalGreaterThanTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> equalGreaterThanTokens: anOrderedCollection [

	equalGreaterThanTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> equalTokens [

	^ equalTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> equalTokens: anOrderedCollection [

	equalTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> exportTokens [

	^ exportTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> exportTokens: anOrderedCollection [

	exportTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> extendsTokens [

	^ extendsTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> extendsTokens: anOrderedCollection [

	extendsTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> extensionTokens [

	^ extensionTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> extensionTokens: anOrderedCollection [

	extensionTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> externalTokens [

	^ externalTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> externalTokens: anOrderedCollection [

	externalTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> factoryTokens [

	^ factoryTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> factoryTokens: anOrderedCollection [

	factoryTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> falseTokens [

	^ falseTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> falseTokens: anOrderedCollection [

	falseTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> finalTokens [

	^ finalTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> finalTokens: anOrderedCollection [

	finalTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> forTokens [

	^ forTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> forTokens: anOrderedCollection [

	forTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> functionTokens [

	^ functionTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> functionTokens: anOrderedCollection [

	functionTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> getTokens [

	^ getTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> getTokens: anOrderedCollection [

	getTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> greaterThanTokens [

	^ greaterThanTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> greaterThanTokens: anOrderedCollection [

	greaterThanTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> hEX_NUMBERTokens [

	^ hEX_NUMBERTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> hEX_NUMBERTokens: anOrderedCollection [

	hEX_NUMBERTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> hideTokens [

	^ hideTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> hideTokens: anOrderedCollection [

	hideTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> identifierTokens [

	^ identifierTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> identifierTokens: anOrderedCollection [

	identifierTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> ifTokens [

	^ ifTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> ifTokens: anOrderedCollection [

	ifTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> implementsTokens [

	^ implementsTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> implementsTokens: anOrderedCollection [

	implementsTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> importTokens [

	^ importTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> importTokens: anOrderedCollection [

	importTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> inTokens [

	^ inTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> inTokens: anOrderedCollection [

	inTokens := anOrderedCollection
]

{ #category : #'generated-initialize-release' }
DartLocalVariableDeclarationNode >> initialize [
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
	orOrTokens := OrderedCollection new: 2.
	equalEqualTokens := OrderedCollection new: 2.
	finalTokens := OrderedCollection new: 2.
	andAndTokens := OrderedCollection new: 2.
	varTokens := OrderedCollection new: 2.
	rAW_MULTI_LINE_STRINGTokens := OrderedCollection new: 2.
	mULTI_LINE_STRING_SQ_BEGIN_ENDTokens := OrderedCollection new: 2.
	mULTI_LINE_STRING_SQ_BEGIN_MIDTokens := OrderedCollection new: 2.
	mULTI_LINE_STRING_SQ_MID_MIDTokens := OrderedCollection new: 2.
	mULTI_LINE_STRING_SQ_MID_ENDTokens := OrderedCollection new: 2.
	mULTI_LINE_STRING_DQ_BEGIN_ENDTokens := OrderedCollection new: 2.
	mULTI_LINE_STRING_DQ_BEGIN_MIDTokens := OrderedCollection new: 2.
	mULTI_LINE_STRING_DQ_MID_MIDTokens := OrderedCollection new: 2.
	mULTI_LINE_STRING_DQ_MID_ENDTokens := OrderedCollection new: 2.
	rAW_SINGLE_LINE_STRINGTokens := OrderedCollection new: 2.
	sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens := OrderedCollection new: 2.
	sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens := OrderedCollection new: 2.
	sINGLE_LINE_STRING_SQ_MID_MIDTokens := OrderedCollection new: 2.
	sINGLE_LINE_STRING_SQ_MID_ENDTokens := OrderedCollection new: 2.
	sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens := OrderedCollection new: 2.
	sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens := OrderedCollection new: 2.
	sINGLE_LINE_STRING_DQ_MID_MIDTokens := OrderedCollection new: 2.
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
	inTokens := OrderedCollection new: 2.
	semicolonTokens := OrderedCollection new: 2.
	bangEqualTokens := OrderedCollection new: 2.
	isTokens := OrderedCollection new: 2.
	plusPlusTokens := OrderedCollection new: 2.
	minusMinusTokens := OrderedCollection new: 2.
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> interfaceTokens [

	^ interfaceTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> interfaceTokens: anOrderedCollection [

	interfaceTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> isTokens [

	^ isTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> isTokens: anOrderedCollection [

	isTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> lateTokens [

	^ lateTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> lateTokens: anOrderedCollection [

	lateTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> lbraceTokens [

	^ lbraceTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> lbraceTokens: anOrderedCollection [

	lbraceTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> leftBracketTokens [

	^ leftBracketTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> leftBracketTokens: anOrderedCollection [

	leftBracketTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> leftParenTokens [

	^ leftParenTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> leftParenTokens: anOrderedCollection [

	leftParenTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> lessThanEqualTokens [

	^ lessThanEqualTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> lessThanEqualTokens: anOrderedCollection [

	lessThanEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> lessThanLessThanEqualTokens [

	^ lessThanLessThanEqualTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> lessThanLessThanEqualTokens: anOrderedCollection [

	lessThanLessThanEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> lessThanLessThanTokens [

	^ lessThanLessThanTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> lessThanLessThanTokens: anOrderedCollection [

	lessThanLessThanTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> lessThanTokens [

	^ lessThanTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> lessThanTokens: anOrderedCollection [

	lessThanTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> libraryTokens [

	^ libraryTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> libraryTokens: anOrderedCollection [

	libraryTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> mULTI_LINE_STRING_DQ_BEGIN_ENDTokens [

	^ mULTI_LINE_STRING_DQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> mULTI_LINE_STRING_DQ_BEGIN_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> mULTI_LINE_STRING_DQ_BEGIN_MIDTokens [

	^ mULTI_LINE_STRING_DQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> mULTI_LINE_STRING_DQ_BEGIN_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> mULTI_LINE_STRING_DQ_MID_ENDTokens [

	^ mULTI_LINE_STRING_DQ_MID_ENDTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> mULTI_LINE_STRING_DQ_MID_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> mULTI_LINE_STRING_DQ_MID_MIDTokens [

	^ mULTI_LINE_STRING_DQ_MID_MIDTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> mULTI_LINE_STRING_DQ_MID_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> mULTI_LINE_STRING_SQ_BEGIN_ENDTokens [

	^ mULTI_LINE_STRING_SQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> mULTI_LINE_STRING_SQ_BEGIN_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> mULTI_LINE_STRING_SQ_BEGIN_MIDTokens [

	^ mULTI_LINE_STRING_SQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> mULTI_LINE_STRING_SQ_BEGIN_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> mULTI_LINE_STRING_SQ_MID_ENDTokens [

	^ mULTI_LINE_STRING_SQ_MID_ENDTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> mULTI_LINE_STRING_SQ_MID_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> mULTI_LINE_STRING_SQ_MID_MIDTokens [

	^ mULTI_LINE_STRING_SQ_MID_MIDTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> mULTI_LINE_STRING_SQ_MID_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> minusEqualTokens [

	^ minusEqualTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> minusEqualTokens: anOrderedCollection [

	minusEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> minusMinusTokens [

	^ minusMinusTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> minusMinusTokens: anOrderedCollection [

	minusMinusTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> minusTokens [

	^ minusTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> minusTokens: anOrderedCollection [

	minusTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> mixinTokens [

	^ mixinTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> mixinTokens: anOrderedCollection [

	mixinTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> newTokens [

	^ newTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> newTokens: anOrderedCollection [

	newTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> nullTokens [

	^ nullTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> nullTokens: anOrderedCollection [

	nullTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> numberTokens [

	^ numberTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> numberTokens: anOrderedCollection [

	numberTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> ofTokens [

	^ ofTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> ofTokens: anOrderedCollection [

	ofTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> onTokens [

	^ onTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> onTokens: anOrderedCollection [

	onTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> operatorTokens [

	^ operatorTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> operatorTokens: anOrderedCollection [

	operatorTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> orEqualTokens [

	^ orEqualTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> orEqualTokens: anOrderedCollection [

	orEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> orOrTokens [

	^ orOrTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> orOrTokens: anOrderedCollection [

	orOrTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> orTokens [

	^ orTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> orTokens: anOrderedCollection [

	orTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> otherVariables [

	^ #( #statementses )
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> partTokens [

	^ partTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> partTokens: anOrderedCollection [

	partTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> percentEqualTokens [

	^ percentEqualTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> percentEqualTokens: anOrderedCollection [

	percentEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> percentTokens [

	^ percentTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> percentTokens: anOrderedCollection [

	percentTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> periodPeriodPeriodQuestionMarkTokens [

	^ periodPeriodPeriodQuestionMarkTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> periodPeriodPeriodQuestionMarkTokens: anOrderedCollection [

	periodPeriodPeriodQuestionMarkTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> periodPeriodPeriodTokens [

	^ periodPeriodPeriodTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> periodPeriodPeriodTokens: anOrderedCollection [

	periodPeriodPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> periodPeriodTokens [

	^ periodPeriodTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> periodPeriodTokens: anOrderedCollection [

	periodPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> periodTokens [

	^ periodTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> periodTokens: anOrderedCollection [

	periodTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> plusEqualTokens [

	^ plusEqualTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> plusEqualTokens: anOrderedCollection [

	plusEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> plusPlusTokens [

	^ plusPlusTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> plusPlusTokens: anOrderedCollection [

	plusPlusTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> plusTokens [

	^ plusTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> plusTokens: anOrderedCollection [

	plusTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> questionMarkPeriodPeriodTokens [

	^ questionMarkPeriodPeriodTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> questionMarkPeriodPeriodTokens: anOrderedCollection [

	questionMarkPeriodPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> questionMarkPeriodTokens [

	^ questionMarkPeriodTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> questionMarkPeriodTokens: anOrderedCollection [

	questionMarkPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> questionMarkQuestionMarkEqualTokens [

	^ questionMarkQuestionMarkEqualTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> questionMarkQuestionMarkEqualTokens: anOrderedCollection [

	questionMarkQuestionMarkEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> questionMarkQuestionMarkTokens [

	^ questionMarkQuestionMarkTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> questionMarkQuestionMarkTokens: anOrderedCollection [

	questionMarkQuestionMarkTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> questionMarkTokens [

	^ questionMarkTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> questionMarkTokens: anOrderedCollection [

	questionMarkTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> rAW_MULTI_LINE_STRINGTokens [

	^ rAW_MULTI_LINE_STRINGTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> rAW_MULTI_LINE_STRINGTokens: anOrderedCollection [

	rAW_MULTI_LINE_STRINGTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> rAW_SINGLE_LINE_STRINGTokens [

	^ rAW_SINGLE_LINE_STRINGTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> rAW_SINGLE_LINE_STRINGTokens: anOrderedCollection [

	rAW_SINGLE_LINE_STRINGTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> rbraceTokens [

	^ rbraceTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> rbraceTokens: anOrderedCollection [

	rbraceTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> requiredTokens [

	^ requiredTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> requiredTokens: anOrderedCollection [

	requiredTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> rightBracketTokens [

	^ rightBracketTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> rightBracketTokens: anOrderedCollection [

	rightBracketTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> rightParenTokens [

	^ rightParenTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> rightParenTokens: anOrderedCollection [

	rightParenTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens [

	^ sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens [

	^ sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> sINGLE_LINE_STRING_DQ_MID_ENDTokens [

	^ sINGLE_LINE_STRING_DQ_MID_ENDTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> sINGLE_LINE_STRING_DQ_MID_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> sINGLE_LINE_STRING_DQ_MID_MIDTokens [

	^ sINGLE_LINE_STRING_DQ_MID_MIDTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> sINGLE_LINE_STRING_DQ_MID_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens [

	^ sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens [

	^ sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> sINGLE_LINE_STRING_SQ_MID_ENDTokens [

	^ sINGLE_LINE_STRING_SQ_MID_ENDTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> sINGLE_LINE_STRING_SQ_MID_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> sINGLE_LINE_STRING_SQ_MID_MIDTokens [

	^ sINGLE_LINE_STRING_SQ_MID_MIDTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> sINGLE_LINE_STRING_SQ_MID_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> semicolonTokens [

	^ semicolonTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> semicolonTokens: anOrderedCollection [

	semicolonTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> setTokens [

	^ setTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> setTokens: anOrderedCollection [

	setTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> showTokens [

	^ showTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> showTokens: anOrderedCollection [

	showTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> statementses [

	^ statementses
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> statementses: anOrderedCollection [

	self setParent: self statementses to: nil.
	statementses := anOrderedCollection.
	self setParent: self statementses to: self
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> staticTokens [

	^ staticTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> staticTokens: anOrderedCollection [

	staticTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> superTokens [

	^ superTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> superTokens: anOrderedCollection [

	superTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> syncTokens [

	^ syncTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> syncTokens: anOrderedCollection [

	syncTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> thisTokens [

	^ thisTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> thisTokens: anOrderedCollection [

	thisTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> throwTokens [

	^ throwTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> throwTokens: anOrderedCollection [

	throwTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> tildeDivideEqualTokens [

	^ tildeDivideEqualTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> tildeDivideEqualTokens: anOrderedCollection [

	tildeDivideEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> tildeDivideTokens [

	^ tildeDivideTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> tildeDivideTokens: anOrderedCollection [

	tildeDivideTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> tildeTokens [

	^ tildeTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> tildeTokens: anOrderedCollection [

	tildeTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> timesEqualTokens [

	^ timesEqualTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> timesEqualTokens: anOrderedCollection [

	timesEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> timesTokens [

	^ timesTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> timesTokens: anOrderedCollection [

	timesTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> trueTokens [

	^ trueTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> trueTokens: anOrderedCollection [

	trueTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> typedefTokens [

	^ typedefTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> typedefTokens: anOrderedCollection [

	typedefTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> upArrowEqualTokens [

	^ upArrowEqualTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> upArrowEqualTokens: anOrderedCollection [

	upArrowEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> upArrowTokens [

	^ upArrowTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> upArrowTokens: anOrderedCollection [

	upArrowTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> varTokens [

	^ varTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> varTokens: anOrderedCollection [

	varTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> voidTokens [

	^ voidTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> voidTokens: anOrderedCollection [

	voidTokens := anOrderedCollection
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> yieldTokens [

	^ yieldTokens
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> yieldTokens: anOrderedCollection [

	yieldTokens := anOrderedCollection
]
