Class {
	#name : #DartSwitchStatementNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'switchToken',
		'leftParenTokens',
		'expression',
		'rightParenTokens',
		'lbraceTokens',
		'caseTokens',
		'colonTokens',
		'identifierTokens',
		'asyncTokens',
		'hideTokens',
		'ofTokens',
		'onTokens',
		'showTokens',
		'syncTokens',
		'awaitTokens',
		'yieldTokens',
		'functionTokens',
		'throwTokens',
		'superTokens',
		'equalTokens',
		'periodPeriodTokens',
		'questionMarkPeriodPeriodTokens',
		'questionMarkTokens',
		'commaTokens',
		'lessThanTokens',
		'extendsTokens',
		'greaterThanTokens',
		'equalGreaterThanTokens',
		'leftBracketTokens',
		'rightBracketTokens',
		'periodTokens',
		'thisTokens',
		'constTokens',
		'newTokens',
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
		'statements',
		'rbraceTokens',
		'dynamicTokens',
		'statementses',
		'timesTokens',
		'nullTokens',
		'trueTokens',
		'falseTokens',
		'numberTokens',
		'hEX_NUMBERTokens',
		'_Tokens',
		'tildeTokens',
		'voidTokens',
		'orOrTokens',
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
		'equalEqualTokens',
		'finalTokens',
		'andAndTokens',
		'varTokens',
		'rAW_MULTI_LINE_STRINGTokens',
		'mULTI_LINE_STRING_SQ_BEGIN_ENDTokens',
		'mULTI_LINE_STRING_SQ_BEGIN_MIDTokens',
		'mULTI_LINE_STRING_SQ_MID_MIDTokens',
		'mULTI_LINE_STRING_SQ_MID_ENDTokens',
		'mULTI_LINE_STRING_DQ_BEGIN_ENDTokens',
		'mULTI_LINE_STRING_DQ_BEGIN_MIDTokens',
		'mULTI_LINE_STRING_DQ_MID_MIDTokens',
		'mULTI_LINE_STRING_DQ_MID_ENDTokens',
		'rAW_SINGLE_LINE_STRINGTokens',
		'sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens',
		'sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens',
		'sINGLE_LINE_STRING_SQ_MID_MIDTokens',
		'sINGLE_LINE_STRING_SQ_MID_ENDTokens',
		'sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens',
		'sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens',
		'sINGLE_LINE_STRING_DQ_MID_MIDTokens',
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
		'inTokens',
		'semicolonTokens',
		'bangEqualTokens',
		'isTokens',
		'plusPlusTokens',
		'minusMinusTokens',
		'defaultToken'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartSwitchStatementNode >> _Tokens [

	^ _Tokens
]

{ #category : #generated }
DartSwitchStatementNode >> _Tokens: anOrderedCollection [

	_Tokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> abstractTokens [

	^ abstractTokens
]

{ #category : #generated }
DartSwitchStatementNode >> abstractTokens: anOrderedCollection [

	abstractTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitSwitchStatement: self
]

{ #category : #generated }
DartSwitchStatementNode >> andAndTokens [

	^ andAndTokens
]

{ #category : #generated }
DartSwitchStatementNode >> andAndTokens: anOrderedCollection [

	andAndTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> andEqualTokens [

	^ andEqualTokens
]

{ #category : #generated }
DartSwitchStatementNode >> andEqualTokens: anOrderedCollection [

	andEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> andTokens [

	^ andTokens
]

{ #category : #generated }
DartSwitchStatementNode >> andTokens: anOrderedCollection [

	andTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> asTokens [

	^ asTokens
]

{ #category : #generated }
DartSwitchStatementNode >> asTokens: anOrderedCollection [

	asTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> asyncTokens [

	^ asyncTokens
]

{ #category : #generated }
DartSwitchStatementNode >> asyncTokens: anOrderedCollection [

	asyncTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> awaitTokens [

	^ awaitTokens
]

{ #category : #generated }
DartSwitchStatementNode >> awaitTokens: anOrderedCollection [

	awaitTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> bangEqualTokens [

	^ bangEqualTokens
]

{ #category : #generated }
DartSwitchStatementNode >> bangEqualTokens: anOrderedCollection [

	bangEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> bangTokens [

	^ bangTokens
]

{ #category : #generated }
DartSwitchStatementNode >> bangTokens: anOrderedCollection [

	bangTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> caseTokens [

	^ caseTokens
]

{ #category : #generated }
DartSwitchStatementNode >> caseTokens: anOrderedCollection [

	caseTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> colonTokens [

	^ colonTokens
]

{ #category : #generated }
DartSwitchStatementNode >> colonTokens: anOrderedCollection [

	colonTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> commaTokens [

	^ commaTokens
]

{ #category : #generated }
DartSwitchStatementNode >> commaTokens: anOrderedCollection [

	commaTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> compositeTokenVariables [

	^ #( #leftParenTokens #rightParenTokens #lbraceTokens #caseTokens
	     #colonTokens #identifierTokens #asyncTokens #hideTokens
	     #ofTokens #onTokens #showTokens #syncTokens #awaitTokens
	     #yieldTokens #functionTokens #throwTokens #superTokens
	     #equalTokens #periodPeriodTokens #questionMarkPeriodPeriodTokens
	     #questionMarkTokens #commaTokens #lessThanTokens #extendsTokens
	     #greaterThanTokens #equalGreaterThanTokens
	     #leftBracketTokens #rightBracketTokens #periodTokens
	     #thisTokens #constTokens #newTokens #bangTokens #questionMarkPeriodTokens
	     #timesEqualTokens #divideEqualTokens #tildeDivideEqualTokens
	     #percentEqualTokens #plusEqualTokens #minusEqualTokens
	     #lessThanLessThanEqualTokens #andEqualTokens #upArrowEqualTokens
	     #orEqualTokens #questionMarkQuestionMarkEqualTokens
	     #questionMarkQuestionMarkTokens #rbraceTokens #dynamicTokens
	     #timesTokens #nullTokens #trueTokens #falseTokens #numberTokens
	     #hEX_NUMBERTokens #_Tokens #tildeTokens #voidTokens #orOrTokens
	     #abstractTokens #asTokens #covariantTokens #deferredTokens
	     #exportTokens #extensionTokens #externalTokens #factoryTokens
	     #getTokens #implementsTokens #importTokens #interfaceTokens
	     #lateTokens #libraryTokens #operatorTokens #mixinTokens
	     #partTokens #requiredTokens #setTokens #staticTokens
	     #typedefTokens #equalEqualTokens #finalTokens #rAW_MULTI_LINE_STRINGTokens
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
	     #ifTokens #elseTokens #forTokens #andAndTokens #varTokens
	     #mULTI_LINE_STRING_SQ_MID_MIDTokens
	     #mULTI_LINE_STRING_DQ_MID_MIDTokens
	     #sINGLE_LINE_STRING_SQ_MID_MIDTokens
	     #sINGLE_LINE_STRING_DQ_MID_MIDTokens
	     #inTokens #semicolonTokens #bangEqualTokens #isTokens #plusPlusTokens
	     #minusMinusTokens )
]

{ #category : #generated }
DartSwitchStatementNode >> constTokens [

	^ constTokens
]

{ #category : #generated }
DartSwitchStatementNode >> constTokens: anOrderedCollection [

	constTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> covariantTokens [

	^ covariantTokens
]

{ #category : #generated }
DartSwitchStatementNode >> covariantTokens: anOrderedCollection [

	covariantTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> defaultToken [

	^ defaultToken
]

{ #category : #generated }
DartSwitchStatementNode >> defaultToken: aSmaCCToken [

	defaultToken := aSmaCCToken
]

{ #category : #generated }
DartSwitchStatementNode >> deferredTokens [

	^ deferredTokens
]

{ #category : #generated }
DartSwitchStatementNode >> deferredTokens: anOrderedCollection [

	deferredTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> divideEqualTokens [

	^ divideEqualTokens
]

{ #category : #generated }
DartSwitchStatementNode >> divideEqualTokens: anOrderedCollection [

	divideEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> divideTokens [

	^ divideTokens
]

{ #category : #generated }
DartSwitchStatementNode >> divideTokens: anOrderedCollection [

	divideTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> dynamicTokens [

	^ dynamicTokens
]

{ #category : #generated }
DartSwitchStatementNode >> dynamicTokens: anOrderedCollection [

	dynamicTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> elseTokens [

	^ elseTokens
]

{ #category : #generated }
DartSwitchStatementNode >> elseTokens: anOrderedCollection [

	elseTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> equalEqualTokens [

	^ equalEqualTokens
]

{ #category : #generated }
DartSwitchStatementNode >> equalEqualTokens: anOrderedCollection [

	equalEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> equalGreaterThanTokens [

	^ equalGreaterThanTokens
]

{ #category : #generated }
DartSwitchStatementNode >> equalGreaterThanTokens: anOrderedCollection [

	equalGreaterThanTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> equalTokens [

	^ equalTokens
]

{ #category : #generated }
DartSwitchStatementNode >> equalTokens: anOrderedCollection [

	equalTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> exportTokens [

	^ exportTokens
]

{ #category : #generated }
DartSwitchStatementNode >> exportTokens: anOrderedCollection [

	exportTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> expression [

	^ expression
]

{ #category : #generated }
DartSwitchStatementNode >> expression: anOrderedCollection [

	self setParent: self expression to: nil.
	expression := anOrderedCollection.
	self setParent: self expression to: self
]

{ #category : #generated }
DartSwitchStatementNode >> extendsTokens [

	^ extendsTokens
]

{ #category : #generated }
DartSwitchStatementNode >> extendsTokens: anOrderedCollection [

	extendsTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> extensionTokens [

	^ extensionTokens
]

{ #category : #generated }
DartSwitchStatementNode >> extensionTokens: anOrderedCollection [

	extensionTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> externalTokens [

	^ externalTokens
]

{ #category : #generated }
DartSwitchStatementNode >> externalTokens: anOrderedCollection [

	externalTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> factoryTokens [

	^ factoryTokens
]

{ #category : #generated }
DartSwitchStatementNode >> factoryTokens: anOrderedCollection [

	factoryTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> falseTokens [

	^ falseTokens
]

{ #category : #generated }
DartSwitchStatementNode >> falseTokens: anOrderedCollection [

	falseTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> finalTokens [

	^ finalTokens
]

{ #category : #generated }
DartSwitchStatementNode >> finalTokens: anOrderedCollection [

	finalTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> forTokens [

	^ forTokens
]

{ #category : #generated }
DartSwitchStatementNode >> forTokens: anOrderedCollection [

	forTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> functionTokens [

	^ functionTokens
]

{ #category : #generated }
DartSwitchStatementNode >> functionTokens: anOrderedCollection [

	functionTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> getTokens [

	^ getTokens
]

{ #category : #generated }
DartSwitchStatementNode >> getTokens: anOrderedCollection [

	getTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> greaterThanTokens [

	^ greaterThanTokens
]

{ #category : #generated }
DartSwitchStatementNode >> greaterThanTokens: anOrderedCollection [

	greaterThanTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> hEX_NUMBERTokens [

	^ hEX_NUMBERTokens
]

{ #category : #generated }
DartSwitchStatementNode >> hEX_NUMBERTokens: anOrderedCollection [

	hEX_NUMBERTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> hideTokens [

	^ hideTokens
]

{ #category : #generated }
DartSwitchStatementNode >> hideTokens: anOrderedCollection [

	hideTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> identifierTokens [

	^ identifierTokens
]

{ #category : #generated }
DartSwitchStatementNode >> identifierTokens: anOrderedCollection [

	identifierTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> ifTokens [

	^ ifTokens
]

{ #category : #generated }
DartSwitchStatementNode >> ifTokens: anOrderedCollection [

	ifTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> implementsTokens [

	^ implementsTokens
]

{ #category : #generated }
DartSwitchStatementNode >> implementsTokens: anOrderedCollection [

	implementsTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> importTokens [

	^ importTokens
]

{ #category : #generated }
DartSwitchStatementNode >> importTokens: anOrderedCollection [

	importTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> inTokens [

	^ inTokens
]

{ #category : #generated }
DartSwitchStatementNode >> inTokens: anOrderedCollection [

	inTokens := anOrderedCollection
]

{ #category : #'generated-initialize-release' }
DartSwitchStatementNode >> initialize [
	super initialize.
	leftParenTokens := OrderedCollection new: 2.
	rightParenTokens := OrderedCollection new: 2.
	lbraceTokens := OrderedCollection new: 2.
	caseTokens := OrderedCollection new: 2.
	colonTokens := OrderedCollection new: 2.
	identifierTokens := OrderedCollection new: 2.
	asyncTokens := OrderedCollection new: 2.
	hideTokens := OrderedCollection new: 2.
	ofTokens := OrderedCollection new: 2.
	onTokens := OrderedCollection new: 2.
	showTokens := OrderedCollection new: 2.
	syncTokens := OrderedCollection new: 2.
	awaitTokens := OrderedCollection new: 2.
	yieldTokens := OrderedCollection new: 2.
	functionTokens := OrderedCollection new: 2.
	throwTokens := OrderedCollection new: 2.
	superTokens := OrderedCollection new: 2.
	equalTokens := OrderedCollection new: 2.
	periodPeriodTokens := OrderedCollection new: 2.
	questionMarkPeriodPeriodTokens := OrderedCollection new: 2.
	questionMarkTokens := OrderedCollection new: 2.
	commaTokens := OrderedCollection new: 2.
	lessThanTokens := OrderedCollection new: 2.
	extendsTokens := OrderedCollection new: 2.
	greaterThanTokens := OrderedCollection new: 2.
	equalGreaterThanTokens := OrderedCollection new: 2.
	leftBracketTokens := OrderedCollection new: 2.
	rightBracketTokens := OrderedCollection new: 2.
	periodTokens := OrderedCollection new: 2.
	thisTokens := OrderedCollection new: 2.
	constTokens := OrderedCollection new: 2.
	newTokens := OrderedCollection new: 2.
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
	statements := OrderedCollection new: 2.
	rbraceTokens := OrderedCollection new: 2.
	dynamicTokens := OrderedCollection new: 2.
	statementses := OrderedCollection new: 2.
	timesTokens := OrderedCollection new: 2.
	nullTokens := OrderedCollection new: 2.
	trueTokens := OrderedCollection new: 2.
	falseTokens := OrderedCollection new: 2.
	numberTokens := OrderedCollection new: 2.
	hEX_NUMBERTokens := OrderedCollection new: 2.
	_Tokens := OrderedCollection new: 2.
	tildeTokens := OrderedCollection new: 2.
	voidTokens := OrderedCollection new: 2.
	orOrTokens := OrderedCollection new: 2.
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
DartSwitchStatementNode >> interfaceTokens [

	^ interfaceTokens
]

{ #category : #generated }
DartSwitchStatementNode >> interfaceTokens: anOrderedCollection [

	interfaceTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> isTokens [

	^ isTokens
]

{ #category : #generated }
DartSwitchStatementNode >> isTokens: anOrderedCollection [

	isTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> lateTokens [

	^ lateTokens
]

{ #category : #generated }
DartSwitchStatementNode >> lateTokens: anOrderedCollection [

	lateTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> lbraceTokens [

	^ lbraceTokens
]

{ #category : #generated }
DartSwitchStatementNode >> lbraceTokens: anOrderedCollection [

	lbraceTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> leftBracketTokens [

	^ leftBracketTokens
]

{ #category : #generated }
DartSwitchStatementNode >> leftBracketTokens: anOrderedCollection [

	leftBracketTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> leftParenTokens [

	^ leftParenTokens
]

{ #category : #generated }
DartSwitchStatementNode >> leftParenTokens: anOrderedCollection [

	leftParenTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> lessThanEqualTokens [

	^ lessThanEqualTokens
]

{ #category : #generated }
DartSwitchStatementNode >> lessThanEqualTokens: anOrderedCollection [

	lessThanEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> lessThanLessThanEqualTokens [

	^ lessThanLessThanEqualTokens
]

{ #category : #generated }
DartSwitchStatementNode >> lessThanLessThanEqualTokens: anOrderedCollection [

	lessThanLessThanEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> lessThanLessThanTokens [

	^ lessThanLessThanTokens
]

{ #category : #generated }
DartSwitchStatementNode >> lessThanLessThanTokens: anOrderedCollection [

	lessThanLessThanTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> lessThanTokens [

	^ lessThanTokens
]

{ #category : #generated }
DartSwitchStatementNode >> lessThanTokens: anOrderedCollection [

	lessThanTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> libraryTokens [

	^ libraryTokens
]

{ #category : #generated }
DartSwitchStatementNode >> libraryTokens: anOrderedCollection [

	libraryTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> mULTI_LINE_STRING_DQ_BEGIN_ENDTokens [

	^ mULTI_LINE_STRING_DQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartSwitchStatementNode >> mULTI_LINE_STRING_DQ_BEGIN_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> mULTI_LINE_STRING_DQ_BEGIN_MIDTokens [

	^ mULTI_LINE_STRING_DQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartSwitchStatementNode >> mULTI_LINE_STRING_DQ_BEGIN_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> mULTI_LINE_STRING_DQ_MID_ENDTokens [

	^ mULTI_LINE_STRING_DQ_MID_ENDTokens
]

{ #category : #generated }
DartSwitchStatementNode >> mULTI_LINE_STRING_DQ_MID_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> mULTI_LINE_STRING_DQ_MID_MIDTokens [

	^ mULTI_LINE_STRING_DQ_MID_MIDTokens
]

{ #category : #generated }
DartSwitchStatementNode >> mULTI_LINE_STRING_DQ_MID_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> mULTI_LINE_STRING_SQ_BEGIN_ENDTokens [

	^ mULTI_LINE_STRING_SQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartSwitchStatementNode >> mULTI_LINE_STRING_SQ_BEGIN_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> mULTI_LINE_STRING_SQ_BEGIN_MIDTokens [

	^ mULTI_LINE_STRING_SQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartSwitchStatementNode >> mULTI_LINE_STRING_SQ_BEGIN_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> mULTI_LINE_STRING_SQ_MID_ENDTokens [

	^ mULTI_LINE_STRING_SQ_MID_ENDTokens
]

{ #category : #generated }
DartSwitchStatementNode >> mULTI_LINE_STRING_SQ_MID_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> mULTI_LINE_STRING_SQ_MID_MIDTokens [

	^ mULTI_LINE_STRING_SQ_MID_MIDTokens
]

{ #category : #generated }
DartSwitchStatementNode >> mULTI_LINE_STRING_SQ_MID_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> minusEqualTokens [

	^ minusEqualTokens
]

{ #category : #generated }
DartSwitchStatementNode >> minusEqualTokens: anOrderedCollection [

	minusEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> minusMinusTokens [

	^ minusMinusTokens
]

{ #category : #generated }
DartSwitchStatementNode >> minusMinusTokens: anOrderedCollection [

	minusMinusTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> minusTokens [

	^ minusTokens
]

{ #category : #generated }
DartSwitchStatementNode >> minusTokens: anOrderedCollection [

	minusTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> mixinTokens [

	^ mixinTokens
]

{ #category : #generated }
DartSwitchStatementNode >> mixinTokens: anOrderedCollection [

	mixinTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> newTokens [

	^ newTokens
]

{ #category : #generated }
DartSwitchStatementNode >> newTokens: anOrderedCollection [

	newTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> nullTokens [

	^ nullTokens
]

{ #category : #generated }
DartSwitchStatementNode >> nullTokens: anOrderedCollection [

	nullTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> numberTokens [

	^ numberTokens
]

{ #category : #generated }
DartSwitchStatementNode >> numberTokens: anOrderedCollection [

	numberTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> ofTokens [

	^ ofTokens
]

{ #category : #generated }
DartSwitchStatementNode >> ofTokens: anOrderedCollection [

	ofTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> onTokens [

	^ onTokens
]

{ #category : #generated }
DartSwitchStatementNode >> onTokens: anOrderedCollection [

	onTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> operatorTokens [

	^ operatorTokens
]

{ #category : #generated }
DartSwitchStatementNode >> operatorTokens: anOrderedCollection [

	operatorTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> orEqualTokens [

	^ orEqualTokens
]

{ #category : #generated }
DartSwitchStatementNode >> orEqualTokens: anOrderedCollection [

	orEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> orOrTokens [

	^ orOrTokens
]

{ #category : #generated }
DartSwitchStatementNode >> orOrTokens: anOrderedCollection [

	orOrTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> orTokens [

	^ orTokens
]

{ #category : #generated }
DartSwitchStatementNode >> orTokens: anOrderedCollection [

	orTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> otherVariables [

	^ #( #expression #statements #statementses )
]

{ #category : #generated }
DartSwitchStatementNode >> partTokens [

	^ partTokens
]

{ #category : #generated }
DartSwitchStatementNode >> partTokens: anOrderedCollection [

	partTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> percentEqualTokens [

	^ percentEqualTokens
]

{ #category : #generated }
DartSwitchStatementNode >> percentEqualTokens: anOrderedCollection [

	percentEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> percentTokens [

	^ percentTokens
]

{ #category : #generated }
DartSwitchStatementNode >> percentTokens: anOrderedCollection [

	percentTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> periodPeriodPeriodQuestionMarkTokens [

	^ periodPeriodPeriodQuestionMarkTokens
]

{ #category : #generated }
DartSwitchStatementNode >> periodPeriodPeriodQuestionMarkTokens: anOrderedCollection [

	periodPeriodPeriodQuestionMarkTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> periodPeriodPeriodTokens [

	^ periodPeriodPeriodTokens
]

{ #category : #generated }
DartSwitchStatementNode >> periodPeriodPeriodTokens: anOrderedCollection [

	periodPeriodPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> periodPeriodTokens [

	^ periodPeriodTokens
]

{ #category : #generated }
DartSwitchStatementNode >> periodPeriodTokens: anOrderedCollection [

	periodPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> periodTokens [

	^ periodTokens
]

{ #category : #generated }
DartSwitchStatementNode >> periodTokens: anOrderedCollection [

	periodTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> plusEqualTokens [

	^ plusEqualTokens
]

{ #category : #generated }
DartSwitchStatementNode >> plusEqualTokens: anOrderedCollection [

	plusEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> plusPlusTokens [

	^ plusPlusTokens
]

{ #category : #generated }
DartSwitchStatementNode >> plusPlusTokens: anOrderedCollection [

	plusPlusTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> plusTokens [

	^ plusTokens
]

{ #category : #generated }
DartSwitchStatementNode >> plusTokens: anOrderedCollection [

	plusTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> questionMarkPeriodPeriodTokens [

	^ questionMarkPeriodPeriodTokens
]

{ #category : #generated }
DartSwitchStatementNode >> questionMarkPeriodPeriodTokens: anOrderedCollection [

	questionMarkPeriodPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> questionMarkPeriodTokens [

	^ questionMarkPeriodTokens
]

{ #category : #generated }
DartSwitchStatementNode >> questionMarkPeriodTokens: anOrderedCollection [

	questionMarkPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> questionMarkQuestionMarkEqualTokens [

	^ questionMarkQuestionMarkEqualTokens
]

{ #category : #generated }
DartSwitchStatementNode >> questionMarkQuestionMarkEqualTokens: anOrderedCollection [

	questionMarkQuestionMarkEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> questionMarkQuestionMarkTokens [

	^ questionMarkQuestionMarkTokens
]

{ #category : #generated }
DartSwitchStatementNode >> questionMarkQuestionMarkTokens: anOrderedCollection [

	questionMarkQuestionMarkTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> questionMarkTokens [

	^ questionMarkTokens
]

{ #category : #generated }
DartSwitchStatementNode >> questionMarkTokens: anOrderedCollection [

	questionMarkTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> rAW_MULTI_LINE_STRINGTokens [

	^ rAW_MULTI_LINE_STRINGTokens
]

{ #category : #generated }
DartSwitchStatementNode >> rAW_MULTI_LINE_STRINGTokens: anOrderedCollection [

	rAW_MULTI_LINE_STRINGTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> rAW_SINGLE_LINE_STRINGTokens [

	^ rAW_SINGLE_LINE_STRINGTokens
]

{ #category : #generated }
DartSwitchStatementNode >> rAW_SINGLE_LINE_STRINGTokens: anOrderedCollection [

	rAW_SINGLE_LINE_STRINGTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> rbraceTokens [

	^ rbraceTokens
]

{ #category : #generated }
DartSwitchStatementNode >> rbraceTokens: anOrderedCollection [

	rbraceTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> requiredTokens [

	^ requiredTokens
]

{ #category : #generated }
DartSwitchStatementNode >> requiredTokens: anOrderedCollection [

	requiredTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> rightBracketTokens [

	^ rightBracketTokens
]

{ #category : #generated }
DartSwitchStatementNode >> rightBracketTokens: anOrderedCollection [

	rightBracketTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> rightParenTokens [

	^ rightParenTokens
]

{ #category : #generated }
DartSwitchStatementNode >> rightParenTokens: anOrderedCollection [

	rightParenTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens [

	^ sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartSwitchStatementNode >> sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens [

	^ sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartSwitchStatementNode >> sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> sINGLE_LINE_STRING_DQ_MID_ENDTokens [

	^ sINGLE_LINE_STRING_DQ_MID_ENDTokens
]

{ #category : #generated }
DartSwitchStatementNode >> sINGLE_LINE_STRING_DQ_MID_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> sINGLE_LINE_STRING_DQ_MID_MIDTokens [

	^ sINGLE_LINE_STRING_DQ_MID_MIDTokens
]

{ #category : #generated }
DartSwitchStatementNode >> sINGLE_LINE_STRING_DQ_MID_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens [

	^ sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartSwitchStatementNode >> sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens [

	^ sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartSwitchStatementNode >> sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> sINGLE_LINE_STRING_SQ_MID_ENDTokens [

	^ sINGLE_LINE_STRING_SQ_MID_ENDTokens
]

{ #category : #generated }
DartSwitchStatementNode >> sINGLE_LINE_STRING_SQ_MID_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> sINGLE_LINE_STRING_SQ_MID_MIDTokens [

	^ sINGLE_LINE_STRING_SQ_MID_MIDTokens
]

{ #category : #generated }
DartSwitchStatementNode >> sINGLE_LINE_STRING_SQ_MID_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> semicolonTokens [

	^ semicolonTokens
]

{ #category : #generated }
DartSwitchStatementNode >> semicolonTokens: anOrderedCollection [

	semicolonTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> setTokens [

	^ setTokens
]

{ #category : #generated }
DartSwitchStatementNode >> setTokens: anOrderedCollection [

	setTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> showTokens [

	^ showTokens
]

{ #category : #generated }
DartSwitchStatementNode >> showTokens: anOrderedCollection [

	showTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> statements [

	^ statements
]

{ #category : #generated }
DartSwitchStatementNode >> statements: anOrderedCollection [

	self setParent: self statements to: nil.
	statements := anOrderedCollection.
	self setParent: self statements to: self
]

{ #category : #generated }
DartSwitchStatementNode >> statementses [

	^ statementses
]

{ #category : #generated }
DartSwitchStatementNode >> statementses: anOrderedCollection [

	self setParent: self statementses to: nil.
	statementses := anOrderedCollection.
	self setParent: self statementses to: self
]

{ #category : #generated }
DartSwitchStatementNode >> staticTokens [

	^ staticTokens
]

{ #category : #generated }
DartSwitchStatementNode >> staticTokens: anOrderedCollection [

	staticTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> superTokens [

	^ superTokens
]

{ #category : #generated }
DartSwitchStatementNode >> superTokens: anOrderedCollection [

	superTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> switchToken [

	^ switchToken
]

{ #category : #generated }
DartSwitchStatementNode >> switchToken: aSmaCCToken [

	switchToken := aSmaCCToken
]

{ #category : #generated }
DartSwitchStatementNode >> syncTokens [

	^ syncTokens
]

{ #category : #generated }
DartSwitchStatementNode >> syncTokens: anOrderedCollection [

	syncTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> thisTokens [

	^ thisTokens
]

{ #category : #generated }
DartSwitchStatementNode >> thisTokens: anOrderedCollection [

	thisTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> throwTokens [

	^ throwTokens
]

{ #category : #generated }
DartSwitchStatementNode >> throwTokens: anOrderedCollection [

	throwTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> tildeDivideEqualTokens [

	^ tildeDivideEqualTokens
]

{ #category : #generated }
DartSwitchStatementNode >> tildeDivideEqualTokens: anOrderedCollection [

	tildeDivideEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> tildeDivideTokens [

	^ tildeDivideTokens
]

{ #category : #generated }
DartSwitchStatementNode >> tildeDivideTokens: anOrderedCollection [

	tildeDivideTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> tildeTokens [

	^ tildeTokens
]

{ #category : #generated }
DartSwitchStatementNode >> tildeTokens: anOrderedCollection [

	tildeTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> timesEqualTokens [

	^ timesEqualTokens
]

{ #category : #generated }
DartSwitchStatementNode >> timesEqualTokens: anOrderedCollection [

	timesEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> timesTokens [

	^ timesTokens
]

{ #category : #generated }
DartSwitchStatementNode >> timesTokens: anOrderedCollection [

	timesTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> tokenVariables [

	^ #( #switchToken #defaultToken )
]

{ #category : #generated }
DartSwitchStatementNode >> trueTokens [

	^ trueTokens
]

{ #category : #generated }
DartSwitchStatementNode >> trueTokens: anOrderedCollection [

	trueTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> typedefTokens [

	^ typedefTokens
]

{ #category : #generated }
DartSwitchStatementNode >> typedefTokens: anOrderedCollection [

	typedefTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> upArrowEqualTokens [

	^ upArrowEqualTokens
]

{ #category : #generated }
DartSwitchStatementNode >> upArrowEqualTokens: anOrderedCollection [

	upArrowEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> upArrowTokens [

	^ upArrowTokens
]

{ #category : #generated }
DartSwitchStatementNode >> upArrowTokens: anOrderedCollection [

	upArrowTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> varTokens [

	^ varTokens
]

{ #category : #generated }
DartSwitchStatementNode >> varTokens: anOrderedCollection [

	varTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> voidTokens [

	^ voidTokens
]

{ #category : #generated }
DartSwitchStatementNode >> voidTokens: anOrderedCollection [

	voidTokens := anOrderedCollection
]

{ #category : #generated }
DartSwitchStatementNode >> yieldTokens [

	^ yieldTokens
]

{ #category : #generated }
DartSwitchStatementNode >> yieldTokens: anOrderedCollection [

	yieldTokens := anOrderedCollection
]
