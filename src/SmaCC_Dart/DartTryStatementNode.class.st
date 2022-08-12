Class {
	#name : #DartTryStatementNode,
	#superclass : #DartStatementNode,
	#instVars : [
		'tryToken',
		'block',
		'catchTokens',
		'leftParenTokens',
		'identifierTokens',
		'abstractTokens',
		'asTokens',
		'covariantTokens',
		'deferredTokens',
		'dynamicTokens',
		'exportTokens',
		'extensionTokens',
		'externalTokens',
		'factoryTokens',
		'functionTokens',
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
		'asyncTokens',
		'hideTokens',
		'ofTokens',
		'onTokens',
		'showTokens',
		'syncTokens',
		'awaitTokens',
		'yieldTokens',
		'rightParenTokens',
		'commaTokens',
		'lbraceTokens',
		'statementses',
		'rbraceTokens',
		'_Tokens',
		'periodTokens',
		'newTokens',
		'lessThanTokens',
		'greaterThanTokens',
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
		'minusMinusTokens',
		'finallyToken'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartTryStatementNode >> _Tokens [

	^ _Tokens
]

{ #category : #generated }
DartTryStatementNode >> _Tokens: anOrderedCollection [

	_Tokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> abstractTokens [

	^ abstractTokens
]

{ #category : #generated }
DartTryStatementNode >> abstractTokens: anOrderedCollection [

	abstractTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitTryStatement: self
]

{ #category : #generated }
DartTryStatementNode >> andAndTokens [

	^ andAndTokens
]

{ #category : #generated }
DartTryStatementNode >> andAndTokens: anOrderedCollection [

	andAndTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> andEqualTokens [

	^ andEqualTokens
]

{ #category : #generated }
DartTryStatementNode >> andEqualTokens: anOrderedCollection [

	andEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> andTokens [

	^ andTokens
]

{ #category : #generated }
DartTryStatementNode >> andTokens: anOrderedCollection [

	andTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> asTokens [

	^ asTokens
]

{ #category : #generated }
DartTryStatementNode >> asTokens: anOrderedCollection [

	asTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> asyncTokens [

	^ asyncTokens
]

{ #category : #generated }
DartTryStatementNode >> asyncTokens: anOrderedCollection [

	asyncTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> awaitTokens [

	^ awaitTokens
]

{ #category : #generated }
DartTryStatementNode >> awaitTokens: anOrderedCollection [

	awaitTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> bangEqualTokens [

	^ bangEqualTokens
]

{ #category : #generated }
DartTryStatementNode >> bangEqualTokens: anOrderedCollection [

	bangEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> bangTokens [

	^ bangTokens
]

{ #category : #generated }
DartTryStatementNode >> bangTokens: anOrderedCollection [

	bangTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> block [

	^ block
]

{ #category : #generated }
DartTryStatementNode >> block: aDartBlockNode [

	self block notNil ifTrue: [ self block parent: nil ].
	block := aDartBlockNode.
	self block notNil ifTrue: [ self block parent: self ]
]

{ #category : #generated }
DartTryStatementNode >> catchTokens [

	^ catchTokens
]

{ #category : #generated }
DartTryStatementNode >> catchTokens: anOrderedCollection [

	catchTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> colonTokens [

	^ colonTokens
]

{ #category : #generated }
DartTryStatementNode >> colonTokens: anOrderedCollection [

	colonTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> commaTokens [

	^ commaTokens
]

{ #category : #generated }
DartTryStatementNode >> commaTokens: anOrderedCollection [

	commaTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> compositeTokenVariables [

	^ #( #catchTokens #leftParenTokens #identifierTokens
	     #abstractTokens #asTokens #covariantTokens #deferredTokens
	     #dynamicTokens #exportTokens #extensionTokens #externalTokens
	     #factoryTokens #functionTokens #getTokens #implementsTokens
	     #importTokens #interfaceTokens #lateTokens #libraryTokens
	     #operatorTokens #mixinTokens #partTokens #requiredTokens
	     #setTokens #staticTokens #typedefTokens #asyncTokens
	     #hideTokens #ofTokens #onTokens #showTokens #syncTokens
	     #awaitTokens #yieldTokens #rightParenTokens #commaTokens
	     #lbraceTokens #rbraceTokens #_Tokens #periodTokens #newTokens
	     #lessThanTokens #greaterThanTokens #questionMarkTokens
	     #voidTokens #colonTokens #throwTokens #superTokens #equalTokens
	     #periodPeriodTokens #questionMarkPeriodPeriodTokens
	     #extendsTokens #equalGreaterThanTokens #leftBracketTokens
	     #rightBracketTokens #thisTokens #constTokens #bangTokens
	     #questionMarkPeriodTokens #timesEqualTokens #divideEqualTokens
	     #tildeDivideEqualTokens #percentEqualTokens #plusEqualTokens
	     #minusEqualTokens #lessThanLessThanEqualTokens
	     #andEqualTokens #upArrowEqualTokens #orEqualTokens
	     #questionMarkQuestionMarkEqualTokens
	     #questionMarkQuestionMarkTokens #timesTokens #nullTokens
	     #trueTokens #falseTokens #numberTokens #hEX_NUMBERTokens
	     #tildeTokens #orOrTokens #equalEqualTokens #finalTokens
	     #rAW_MULTI_LINE_STRINGTokens #mULTI_LINE_STRING_SQ_BEGIN_ENDTokens
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
	     #ifTokens #elseTokens #forTokens #andAndTokens #varTokens
	     #mULTI_LINE_STRING_SQ_MID_MIDTokens
	     #mULTI_LINE_STRING_DQ_MID_MIDTokens
	     #sINGLE_LINE_STRING_SQ_MID_MIDTokens
	     #sINGLE_LINE_STRING_DQ_MID_MIDTokens
	     #inTokens #semicolonTokens #bangEqualTokens #isTokens #plusPlusTokens
	     #minusMinusTokens )
]

{ #category : #generated }
DartTryStatementNode >> constTokens [

	^ constTokens
]

{ #category : #generated }
DartTryStatementNode >> constTokens: anOrderedCollection [

	constTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> covariantTokens [

	^ covariantTokens
]

{ #category : #generated }
DartTryStatementNode >> covariantTokens: anOrderedCollection [

	covariantTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> deferredTokens [

	^ deferredTokens
]

{ #category : #generated }
DartTryStatementNode >> deferredTokens: anOrderedCollection [

	deferredTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> divideEqualTokens [

	^ divideEqualTokens
]

{ #category : #generated }
DartTryStatementNode >> divideEqualTokens: anOrderedCollection [

	divideEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> divideTokens [

	^ divideTokens
]

{ #category : #generated }
DartTryStatementNode >> divideTokens: anOrderedCollection [

	divideTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> dynamicTokens [

	^ dynamicTokens
]

{ #category : #generated }
DartTryStatementNode >> dynamicTokens: anOrderedCollection [

	dynamicTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> elseTokens [

	^ elseTokens
]

{ #category : #generated }
DartTryStatementNode >> elseTokens: anOrderedCollection [

	elseTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> equalEqualTokens [

	^ equalEqualTokens
]

{ #category : #generated }
DartTryStatementNode >> equalEqualTokens: anOrderedCollection [

	equalEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> equalGreaterThanTokens [

	^ equalGreaterThanTokens
]

{ #category : #generated }
DartTryStatementNode >> equalGreaterThanTokens: anOrderedCollection [

	equalGreaterThanTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> equalTokens [

	^ equalTokens
]

{ #category : #generated }
DartTryStatementNode >> equalTokens: anOrderedCollection [

	equalTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> exportTokens [

	^ exportTokens
]

{ #category : #generated }
DartTryStatementNode >> exportTokens: anOrderedCollection [

	exportTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> extendsTokens [

	^ extendsTokens
]

{ #category : #generated }
DartTryStatementNode >> extendsTokens: anOrderedCollection [

	extendsTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> extensionTokens [

	^ extensionTokens
]

{ #category : #generated }
DartTryStatementNode >> extensionTokens: anOrderedCollection [

	extensionTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> externalTokens [

	^ externalTokens
]

{ #category : #generated }
DartTryStatementNode >> externalTokens: anOrderedCollection [

	externalTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> factoryTokens [

	^ factoryTokens
]

{ #category : #generated }
DartTryStatementNode >> factoryTokens: anOrderedCollection [

	factoryTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> falseTokens [

	^ falseTokens
]

{ #category : #generated }
DartTryStatementNode >> falseTokens: anOrderedCollection [

	falseTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> finalTokens [

	^ finalTokens
]

{ #category : #generated }
DartTryStatementNode >> finalTokens: anOrderedCollection [

	finalTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> finallyToken [

	^ finallyToken
]

{ #category : #generated }
DartTryStatementNode >> finallyToken: aSmaCCToken [

	finallyToken := aSmaCCToken
]

{ #category : #generated }
DartTryStatementNode >> forTokens [

	^ forTokens
]

{ #category : #generated }
DartTryStatementNode >> forTokens: anOrderedCollection [

	forTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> functionTokens [

	^ functionTokens
]

{ #category : #generated }
DartTryStatementNode >> functionTokens: anOrderedCollection [

	functionTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> getTokens [

	^ getTokens
]

{ #category : #generated }
DartTryStatementNode >> getTokens: anOrderedCollection [

	getTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> greaterThanTokens [

	^ greaterThanTokens
]

{ #category : #generated }
DartTryStatementNode >> greaterThanTokens: anOrderedCollection [

	greaterThanTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> hEX_NUMBERTokens [

	^ hEX_NUMBERTokens
]

{ #category : #generated }
DartTryStatementNode >> hEX_NUMBERTokens: anOrderedCollection [

	hEX_NUMBERTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> hideTokens [

	^ hideTokens
]

{ #category : #generated }
DartTryStatementNode >> hideTokens: anOrderedCollection [

	hideTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> identifierTokens [

	^ identifierTokens
]

{ #category : #generated }
DartTryStatementNode >> identifierTokens: anOrderedCollection [

	identifierTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> ifTokens [

	^ ifTokens
]

{ #category : #generated }
DartTryStatementNode >> ifTokens: anOrderedCollection [

	ifTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> implementsTokens [

	^ implementsTokens
]

{ #category : #generated }
DartTryStatementNode >> implementsTokens: anOrderedCollection [

	implementsTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> importTokens [

	^ importTokens
]

{ #category : #generated }
DartTryStatementNode >> importTokens: anOrderedCollection [

	importTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> inTokens [

	^ inTokens
]

{ #category : #generated }
DartTryStatementNode >> inTokens: anOrderedCollection [

	inTokens := anOrderedCollection
]

{ #category : #'generated-initialize-release' }
DartTryStatementNode >> initialize [
	super initialize.
	catchTokens := OrderedCollection new: 2.
	leftParenTokens := OrderedCollection new: 2.
	identifierTokens := OrderedCollection new: 2.
	abstractTokens := OrderedCollection new: 2.
	asTokens := OrderedCollection new: 2.
	covariantTokens := OrderedCollection new: 2.
	deferredTokens := OrderedCollection new: 2.
	dynamicTokens := OrderedCollection new: 2.
	exportTokens := OrderedCollection new: 2.
	extensionTokens := OrderedCollection new: 2.
	externalTokens := OrderedCollection new: 2.
	factoryTokens := OrderedCollection new: 2.
	functionTokens := OrderedCollection new: 2.
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
	asyncTokens := OrderedCollection new: 2.
	hideTokens := OrderedCollection new: 2.
	ofTokens := OrderedCollection new: 2.
	onTokens := OrderedCollection new: 2.
	showTokens := OrderedCollection new: 2.
	syncTokens := OrderedCollection new: 2.
	awaitTokens := OrderedCollection new: 2.
	yieldTokens := OrderedCollection new: 2.
	rightParenTokens := OrderedCollection new: 2.
	commaTokens := OrderedCollection new: 2.
	lbraceTokens := OrderedCollection new: 2.
	statementses := OrderedCollection new: 2.
	rbraceTokens := OrderedCollection new: 2.
	_Tokens := OrderedCollection new: 2.
	periodTokens := OrderedCollection new: 2.
	newTokens := OrderedCollection new: 2.
	lessThanTokens := OrderedCollection new: 2.
	greaterThanTokens := OrderedCollection new: 2.
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
	andAndTokens := OrderedCollection new: 2.
	varTokens := OrderedCollection new: 2.
	mULTI_LINE_STRING_SQ_MID_MIDTokens := OrderedCollection new: 2.
	mULTI_LINE_STRING_DQ_MID_MIDTokens := OrderedCollection new: 2.
	sINGLE_LINE_STRING_SQ_MID_MIDTokens := OrderedCollection new: 2.
	sINGLE_LINE_STRING_DQ_MID_MIDTokens := OrderedCollection new: 2.
	inTokens := OrderedCollection new: 2.
	semicolonTokens := OrderedCollection new: 2.
	bangEqualTokens := OrderedCollection new: 2.
	isTokens := OrderedCollection new: 2.
	plusPlusTokens := OrderedCollection new: 2.
	minusMinusTokens := OrderedCollection new: 2.
]

{ #category : #generated }
DartTryStatementNode >> interfaceTokens [

	^ interfaceTokens
]

{ #category : #generated }
DartTryStatementNode >> interfaceTokens: anOrderedCollection [

	interfaceTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> isTokens [

	^ isTokens
]

{ #category : #generated }
DartTryStatementNode >> isTokens: anOrderedCollection [

	isTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> lateTokens [

	^ lateTokens
]

{ #category : #generated }
DartTryStatementNode >> lateTokens: anOrderedCollection [

	lateTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> lbraceTokens [

	^ lbraceTokens
]

{ #category : #generated }
DartTryStatementNode >> lbraceTokens: anOrderedCollection [

	lbraceTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> leftBracketTokens [

	^ leftBracketTokens
]

{ #category : #generated }
DartTryStatementNode >> leftBracketTokens: anOrderedCollection [

	leftBracketTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> leftParenTokens [

	^ leftParenTokens
]

{ #category : #generated }
DartTryStatementNode >> leftParenTokens: anOrderedCollection [

	leftParenTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> lessThanEqualTokens [

	^ lessThanEqualTokens
]

{ #category : #generated }
DartTryStatementNode >> lessThanEqualTokens: anOrderedCollection [

	lessThanEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> lessThanLessThanEqualTokens [

	^ lessThanLessThanEqualTokens
]

{ #category : #generated }
DartTryStatementNode >> lessThanLessThanEqualTokens: anOrderedCollection [

	lessThanLessThanEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> lessThanLessThanTokens [

	^ lessThanLessThanTokens
]

{ #category : #generated }
DartTryStatementNode >> lessThanLessThanTokens: anOrderedCollection [

	lessThanLessThanTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> lessThanTokens [

	^ lessThanTokens
]

{ #category : #generated }
DartTryStatementNode >> lessThanTokens: anOrderedCollection [

	lessThanTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> libraryTokens [

	^ libraryTokens
]

{ #category : #generated }
DartTryStatementNode >> libraryTokens: anOrderedCollection [

	libraryTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> mULTI_LINE_STRING_DQ_BEGIN_ENDTokens [

	^ mULTI_LINE_STRING_DQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartTryStatementNode >> mULTI_LINE_STRING_DQ_BEGIN_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> mULTI_LINE_STRING_DQ_BEGIN_MIDTokens [

	^ mULTI_LINE_STRING_DQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartTryStatementNode >> mULTI_LINE_STRING_DQ_BEGIN_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> mULTI_LINE_STRING_DQ_MID_ENDTokens [

	^ mULTI_LINE_STRING_DQ_MID_ENDTokens
]

{ #category : #generated }
DartTryStatementNode >> mULTI_LINE_STRING_DQ_MID_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> mULTI_LINE_STRING_DQ_MID_MIDTokens [

	^ mULTI_LINE_STRING_DQ_MID_MIDTokens
]

{ #category : #generated }
DartTryStatementNode >> mULTI_LINE_STRING_DQ_MID_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> mULTI_LINE_STRING_SQ_BEGIN_ENDTokens [

	^ mULTI_LINE_STRING_SQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartTryStatementNode >> mULTI_LINE_STRING_SQ_BEGIN_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> mULTI_LINE_STRING_SQ_BEGIN_MIDTokens [

	^ mULTI_LINE_STRING_SQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartTryStatementNode >> mULTI_LINE_STRING_SQ_BEGIN_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> mULTI_LINE_STRING_SQ_MID_ENDTokens [

	^ mULTI_LINE_STRING_SQ_MID_ENDTokens
]

{ #category : #generated }
DartTryStatementNode >> mULTI_LINE_STRING_SQ_MID_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> mULTI_LINE_STRING_SQ_MID_MIDTokens [

	^ mULTI_LINE_STRING_SQ_MID_MIDTokens
]

{ #category : #generated }
DartTryStatementNode >> mULTI_LINE_STRING_SQ_MID_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> minusEqualTokens [

	^ minusEqualTokens
]

{ #category : #generated }
DartTryStatementNode >> minusEqualTokens: anOrderedCollection [

	minusEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> minusMinusTokens [

	^ minusMinusTokens
]

{ #category : #generated }
DartTryStatementNode >> minusMinusTokens: anOrderedCollection [

	minusMinusTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> minusTokens [

	^ minusTokens
]

{ #category : #generated }
DartTryStatementNode >> minusTokens: anOrderedCollection [

	minusTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> mixinTokens [

	^ mixinTokens
]

{ #category : #generated }
DartTryStatementNode >> mixinTokens: anOrderedCollection [

	mixinTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> newTokens [

	^ newTokens
]

{ #category : #generated }
DartTryStatementNode >> newTokens: anOrderedCollection [

	newTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> nodeVariables [

	^ #( #block )
]

{ #category : #generated }
DartTryStatementNode >> nullTokens [

	^ nullTokens
]

{ #category : #generated }
DartTryStatementNode >> nullTokens: anOrderedCollection [

	nullTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> numberTokens [

	^ numberTokens
]

{ #category : #generated }
DartTryStatementNode >> numberTokens: anOrderedCollection [

	numberTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> ofTokens [

	^ ofTokens
]

{ #category : #generated }
DartTryStatementNode >> ofTokens: anOrderedCollection [

	ofTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> onTokens [

	^ onTokens
]

{ #category : #generated }
DartTryStatementNode >> onTokens: anOrderedCollection [

	onTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> operatorTokens [

	^ operatorTokens
]

{ #category : #generated }
DartTryStatementNode >> operatorTokens: anOrderedCollection [

	operatorTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> orEqualTokens [

	^ orEqualTokens
]

{ #category : #generated }
DartTryStatementNode >> orEqualTokens: anOrderedCollection [

	orEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> orOrTokens [

	^ orOrTokens
]

{ #category : #generated }
DartTryStatementNode >> orOrTokens: anOrderedCollection [

	orOrTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> orTokens [

	^ orTokens
]

{ #category : #generated }
DartTryStatementNode >> orTokens: anOrderedCollection [

	orTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> otherVariables [

	^ #( #statementses )
]

{ #category : #generated }
DartTryStatementNode >> partTokens [

	^ partTokens
]

{ #category : #generated }
DartTryStatementNode >> partTokens: anOrderedCollection [

	partTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> percentEqualTokens [

	^ percentEqualTokens
]

{ #category : #generated }
DartTryStatementNode >> percentEqualTokens: anOrderedCollection [

	percentEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> percentTokens [

	^ percentTokens
]

{ #category : #generated }
DartTryStatementNode >> percentTokens: anOrderedCollection [

	percentTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> periodPeriodPeriodQuestionMarkTokens [

	^ periodPeriodPeriodQuestionMarkTokens
]

{ #category : #generated }
DartTryStatementNode >> periodPeriodPeriodQuestionMarkTokens: anOrderedCollection [

	periodPeriodPeriodQuestionMarkTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> periodPeriodPeriodTokens [

	^ periodPeriodPeriodTokens
]

{ #category : #generated }
DartTryStatementNode >> periodPeriodPeriodTokens: anOrderedCollection [

	periodPeriodPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> periodPeriodTokens [

	^ periodPeriodTokens
]

{ #category : #generated }
DartTryStatementNode >> periodPeriodTokens: anOrderedCollection [

	periodPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> periodTokens [

	^ periodTokens
]

{ #category : #generated }
DartTryStatementNode >> periodTokens: anOrderedCollection [

	periodTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> plusEqualTokens [

	^ plusEqualTokens
]

{ #category : #generated }
DartTryStatementNode >> plusEqualTokens: anOrderedCollection [

	plusEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> plusPlusTokens [

	^ plusPlusTokens
]

{ #category : #generated }
DartTryStatementNode >> plusPlusTokens: anOrderedCollection [

	plusPlusTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> plusTokens [

	^ plusTokens
]

{ #category : #generated }
DartTryStatementNode >> plusTokens: anOrderedCollection [

	plusTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> questionMarkPeriodPeriodTokens [

	^ questionMarkPeriodPeriodTokens
]

{ #category : #generated }
DartTryStatementNode >> questionMarkPeriodPeriodTokens: anOrderedCollection [

	questionMarkPeriodPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> questionMarkPeriodTokens [

	^ questionMarkPeriodTokens
]

{ #category : #generated }
DartTryStatementNode >> questionMarkPeriodTokens: anOrderedCollection [

	questionMarkPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> questionMarkQuestionMarkEqualTokens [

	^ questionMarkQuestionMarkEqualTokens
]

{ #category : #generated }
DartTryStatementNode >> questionMarkQuestionMarkEqualTokens: anOrderedCollection [

	questionMarkQuestionMarkEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> questionMarkQuestionMarkTokens [

	^ questionMarkQuestionMarkTokens
]

{ #category : #generated }
DartTryStatementNode >> questionMarkQuestionMarkTokens: anOrderedCollection [

	questionMarkQuestionMarkTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> questionMarkTokens [

	^ questionMarkTokens
]

{ #category : #generated }
DartTryStatementNode >> questionMarkTokens: anOrderedCollection [

	questionMarkTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> rAW_MULTI_LINE_STRINGTokens [

	^ rAW_MULTI_LINE_STRINGTokens
]

{ #category : #generated }
DartTryStatementNode >> rAW_MULTI_LINE_STRINGTokens: anOrderedCollection [

	rAW_MULTI_LINE_STRINGTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> rAW_SINGLE_LINE_STRINGTokens [

	^ rAW_SINGLE_LINE_STRINGTokens
]

{ #category : #generated }
DartTryStatementNode >> rAW_SINGLE_LINE_STRINGTokens: anOrderedCollection [

	rAW_SINGLE_LINE_STRINGTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> rbraceTokens [

	^ rbraceTokens
]

{ #category : #generated }
DartTryStatementNode >> rbraceTokens: anOrderedCollection [

	rbraceTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> requiredTokens [

	^ requiredTokens
]

{ #category : #generated }
DartTryStatementNode >> requiredTokens: anOrderedCollection [

	requiredTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> rightBracketTokens [

	^ rightBracketTokens
]

{ #category : #generated }
DartTryStatementNode >> rightBracketTokens: anOrderedCollection [

	rightBracketTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> rightParenTokens [

	^ rightParenTokens
]

{ #category : #generated }
DartTryStatementNode >> rightParenTokens: anOrderedCollection [

	rightParenTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens [

	^ sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartTryStatementNode >> sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens [

	^ sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartTryStatementNode >> sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> sINGLE_LINE_STRING_DQ_MID_ENDTokens [

	^ sINGLE_LINE_STRING_DQ_MID_ENDTokens
]

{ #category : #generated }
DartTryStatementNode >> sINGLE_LINE_STRING_DQ_MID_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> sINGLE_LINE_STRING_DQ_MID_MIDTokens [

	^ sINGLE_LINE_STRING_DQ_MID_MIDTokens
]

{ #category : #generated }
DartTryStatementNode >> sINGLE_LINE_STRING_DQ_MID_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens [

	^ sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartTryStatementNode >> sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens [

	^ sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartTryStatementNode >> sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> sINGLE_LINE_STRING_SQ_MID_ENDTokens [

	^ sINGLE_LINE_STRING_SQ_MID_ENDTokens
]

{ #category : #generated }
DartTryStatementNode >> sINGLE_LINE_STRING_SQ_MID_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> sINGLE_LINE_STRING_SQ_MID_MIDTokens [

	^ sINGLE_LINE_STRING_SQ_MID_MIDTokens
]

{ #category : #generated }
DartTryStatementNode >> sINGLE_LINE_STRING_SQ_MID_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> semicolonTokens [

	^ semicolonTokens
]

{ #category : #generated }
DartTryStatementNode >> semicolonTokens: anOrderedCollection [

	semicolonTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> setTokens [

	^ setTokens
]

{ #category : #generated }
DartTryStatementNode >> setTokens: anOrderedCollection [

	setTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> showTokens [

	^ showTokens
]

{ #category : #generated }
DartTryStatementNode >> showTokens: anOrderedCollection [

	showTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> statementses [

	^ statementses
]

{ #category : #generated }
DartTryStatementNode >> statementses: anOrderedCollection [

	self setParent: self statementses to: nil.
	statementses := anOrderedCollection.
	self setParent: self statementses to: self
]

{ #category : #generated }
DartTryStatementNode >> staticTokens [

	^ staticTokens
]

{ #category : #generated }
DartTryStatementNode >> staticTokens: anOrderedCollection [

	staticTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> superTokens [

	^ superTokens
]

{ #category : #generated }
DartTryStatementNode >> superTokens: anOrderedCollection [

	superTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> syncTokens [

	^ syncTokens
]

{ #category : #generated }
DartTryStatementNode >> syncTokens: anOrderedCollection [

	syncTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> thisTokens [

	^ thisTokens
]

{ #category : #generated }
DartTryStatementNode >> thisTokens: anOrderedCollection [

	thisTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> throwTokens [

	^ throwTokens
]

{ #category : #generated }
DartTryStatementNode >> throwTokens: anOrderedCollection [

	throwTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> tildeDivideEqualTokens [

	^ tildeDivideEqualTokens
]

{ #category : #generated }
DartTryStatementNode >> tildeDivideEqualTokens: anOrderedCollection [

	tildeDivideEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> tildeDivideTokens [

	^ tildeDivideTokens
]

{ #category : #generated }
DartTryStatementNode >> tildeDivideTokens: anOrderedCollection [

	tildeDivideTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> tildeTokens [

	^ tildeTokens
]

{ #category : #generated }
DartTryStatementNode >> tildeTokens: anOrderedCollection [

	tildeTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> timesEqualTokens [

	^ timesEqualTokens
]

{ #category : #generated }
DartTryStatementNode >> timesEqualTokens: anOrderedCollection [

	timesEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> timesTokens [

	^ timesTokens
]

{ #category : #generated }
DartTryStatementNode >> timesTokens: anOrderedCollection [

	timesTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> tokenVariables [

	^ #( #tryToken #finallyToken )
]

{ #category : #generated }
DartTryStatementNode >> trueTokens [

	^ trueTokens
]

{ #category : #generated }
DartTryStatementNode >> trueTokens: anOrderedCollection [

	trueTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> tryToken [

	^ tryToken
]

{ #category : #generated }
DartTryStatementNode >> tryToken: aSmaCCToken [

	tryToken := aSmaCCToken
]

{ #category : #generated }
DartTryStatementNode >> typedefTokens [

	^ typedefTokens
]

{ #category : #generated }
DartTryStatementNode >> typedefTokens: anOrderedCollection [

	typedefTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> upArrowEqualTokens [

	^ upArrowEqualTokens
]

{ #category : #generated }
DartTryStatementNode >> upArrowEqualTokens: anOrderedCollection [

	upArrowEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> upArrowTokens [

	^ upArrowTokens
]

{ #category : #generated }
DartTryStatementNode >> upArrowTokens: anOrderedCollection [

	upArrowTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> varTokens [

	^ varTokens
]

{ #category : #generated }
DartTryStatementNode >> varTokens: anOrderedCollection [

	varTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> voidTokens [

	^ voidTokens
]

{ #category : #generated }
DartTryStatementNode >> voidTokens: anOrderedCollection [

	voidTokens := anOrderedCollection
]

{ #category : #generated }
DartTryStatementNode >> yieldTokens [

	^ yieldTokens
]

{ #category : #generated }
DartTryStatementNode >> yieldTokens: anOrderedCollection [

	yieldTokens := anOrderedCollection
]
