Class {
	#name : #DartProgramNode,
	#superclass : #SmaCCParseNode,
	#instVars : [
		'lbraceTokens',
		'equalGreaterThanTokens',
		'semicolonTokens',
		'asyncTokens',
		'timesTokens',
		'syncTokens',
		'_Tokens',
		'staticTokens',
		'rbraceTokens',
		'externalTokens',
		'finalTokens',
		'covariantTokens',
		'varTokens',
		'abstractTokens',
		'constTokens',
		'lateTokens',
		'equalTokens',
		'commaTokens',
		'leftParenTokens',
		'rightParenTokens',
		'colonTokens',
		'factoryTokens',
		'periodTokens',
		'newTokens',
		'getTokens',
		'setTokens',
		'operatorTokens',
		'identifierTokens',
		'hideTokens',
		'ofTokens',
		'onTokens',
		'showTokens',
		'awaitTokens',
		'yieldTokens',
		'functionTokens',
		'questionMarkTokens',
		'thisTokens',
		'superTokens',
		'lessThanTokens',
		'extendsTokens',
		'greaterThanTokens',
		'tildeTokens',
		'leftBracketTokens',
		'rightBracketTokens',
		'throwTokens',
		'periodPeriodTokens',
		'questionMarkPeriodPeriodTokens',
		'voidTokens',
		'dynamicTokens',
		'assertTokens',
		'asTokens',
		'deferredTokens',
		'exportTokens',
		'extensionTokens',
		'implementsTokens',
		'importTokens',
		'interfaceTokens',
		'libraryTokens',
		'mixinTokens',
		'partTokens',
		'requiredTokens',
		'typedefTokens',
		'equalEqualTokens',
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
		'nullTokens',
		'trueTokens',
		'falseTokens',
		'numberTokens',
		'hEX_NUMBERTokens',
		'orOrTokens',
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
		'periodPeriodPeriodTokens',
		'periodPeriodPeriodQuestionMarkTokens',
		'ifTokens',
		'elseTokens',
		'forTokens',
		'andAndTokens',
		'whileTokens',
		'doTokens',
		'switchTokens',
		'rethrowTokens',
		'tryTokens',
		'breakTokens',
		'continueTokens',
		'returnTokens',
		'mULTI_LINE_STRING_SQ_MID_MIDTokens',
		'mULTI_LINE_STRING_DQ_MID_MIDTokens',
		'sINGLE_LINE_STRING_SQ_MID_MIDTokens',
		'sINGLE_LINE_STRING_DQ_MID_MIDTokens',
		'inTokens',
		'bangEqualTokens',
		'caseTokens',
		'defaultTokens',
		'finallyTokens',
		'isTokens',
		'catchTokens',
		'plusPlusTokens',
		'minusMinusTokens'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartProgramNode >> _Tokens [

	^ _Tokens
]

{ #category : #generated }
DartProgramNode >> _Tokens: anOrderedCollection [

	_Tokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> abstractTokens [

	^ abstractTokens
]

{ #category : #generated }
DartProgramNode >> abstractTokens: anOrderedCollection [

	abstractTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitProgram: self
]

{ #category : #generated }
DartProgramNode >> andAndTokens [

	^ andAndTokens
]

{ #category : #generated }
DartProgramNode >> andAndTokens: anOrderedCollection [

	andAndTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> andEqualTokens [

	^ andEqualTokens
]

{ #category : #generated }
DartProgramNode >> andEqualTokens: anOrderedCollection [

	andEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> andTokens [

	^ andTokens
]

{ #category : #generated }
DartProgramNode >> andTokens: anOrderedCollection [

	andTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> asTokens [

	^ asTokens
]

{ #category : #generated }
DartProgramNode >> asTokens: anOrderedCollection [

	asTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> assertTokens [

	^ assertTokens
]

{ #category : #generated }
DartProgramNode >> assertTokens: anOrderedCollection [

	assertTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> asyncTokens [

	^ asyncTokens
]

{ #category : #generated }
DartProgramNode >> asyncTokens: anOrderedCollection [

	asyncTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> awaitTokens [

	^ awaitTokens
]

{ #category : #generated }
DartProgramNode >> awaitTokens: anOrderedCollection [

	awaitTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> bangEqualTokens [

	^ bangEqualTokens
]

{ #category : #generated }
DartProgramNode >> bangEqualTokens: anOrderedCollection [

	bangEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> bangTokens [

	^ bangTokens
]

{ #category : #generated }
DartProgramNode >> bangTokens: anOrderedCollection [

	bangTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> breakTokens [

	^ breakTokens
]

{ #category : #generated }
DartProgramNode >> breakTokens: anOrderedCollection [

	breakTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> caseTokens [

	^ caseTokens
]

{ #category : #generated }
DartProgramNode >> caseTokens: anOrderedCollection [

	caseTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> catchTokens [

	^ catchTokens
]

{ #category : #generated }
DartProgramNode >> catchTokens: anOrderedCollection [

	catchTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> colonTokens [

	^ colonTokens
]

{ #category : #generated }
DartProgramNode >> colonTokens: anOrderedCollection [

	colonTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> commaTokens [

	^ commaTokens
]

{ #category : #generated }
DartProgramNode >> commaTokens: anOrderedCollection [

	commaTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> compositeTokenVariables [

	^ #( #lbraceTokens #equalGreaterThanTokens #semicolonTokens
	     #asyncTokens #timesTokens #syncTokens #_Tokens #staticTokens
	     #rbraceTokens #externalTokens #finalTokens #covariantTokens
	     #varTokens #abstractTokens #constTokens #lateTokens #equalTokens
	     #commaTokens #leftParenTokens #rightParenTokens
	     #colonTokens #factoryTokens #periodTokens #newTokens #getTokens
	     #setTokens #operatorTokens #identifierTokens #hideTokens
	     #ofTokens #onTokens #showTokens #awaitTokens #yieldTokens
	     #functionTokens #questionMarkTokens #thisTokens #superTokens
	     #lessThanTokens #extendsTokens #greaterThanTokens
	     #tildeTokens #leftBracketTokens #rightBracketTokens
	     #throwTokens #periodPeriodTokens #questionMarkPeriodPeriodTokens
	     #voidTokens #dynamicTokens #assertTokens #asTokens #deferredTokens
	     #exportTokens #extensionTokens #implementsTokens
	     #importTokens #interfaceTokens #libraryTokens #mixinTokens
	     #partTokens #requiredTokens #typedefTokens #equalEqualTokens
	     #bangTokens #questionMarkPeriodTokens #timesEqualTokens
	     #divideEqualTokens #tildeDivideEqualTokens
	     #percentEqualTokens #plusEqualTokens #minusEqualTokens
	     #lessThanLessThanEqualTokens #andEqualTokens #upArrowEqualTokens
	     #orEqualTokens #questionMarkQuestionMarkEqualTokens
	     #questionMarkQuestionMarkTokens #divideTokens #percentTokens
	     #tildeDivideTokens #plusTokens #minusTokens #lessThanLessThanTokens
	     #lessThanEqualTokens #andTokens #upArrowTokens #orTokens
	     #nullTokens #trueTokens #falseTokens #numberTokens #hEX_NUMBERTokens
	     #orOrTokens #rAW_MULTI_LINE_STRINGTokens
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
	     #periodPeriodPeriodTokens #periodPeriodPeriodQuestionMarkTokens
	     #ifTokens #elseTokens #forTokens #andAndTokens #whileTokens
	     #doTokens #switchTokens #rethrowTokens #tryTokens #breakTokens
	     #continueTokens #returnTokens #mULTI_LINE_STRING_SQ_MID_MIDTokens
	     #mULTI_LINE_STRING_DQ_MID_MIDTokens
	     #sINGLE_LINE_STRING_SQ_MID_MIDTokens
	     #sINGLE_LINE_STRING_DQ_MID_MIDTokens
	     #inTokens #bangEqualTokens #caseTokens #defaultTokens
	     #finallyTokens #isTokens #catchTokens #plusPlusTokens
	     #minusMinusTokens )
]

{ #category : #generated }
DartProgramNode >> constTokens [

	^ constTokens
]

{ #category : #generated }
DartProgramNode >> constTokens: anOrderedCollection [

	constTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> continueTokens [

	^ continueTokens
]

{ #category : #generated }
DartProgramNode >> continueTokens: anOrderedCollection [

	continueTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> covariantTokens [

	^ covariantTokens
]

{ #category : #generated }
DartProgramNode >> covariantTokens: anOrderedCollection [

	covariantTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> defaultTokens [

	^ defaultTokens
]

{ #category : #generated }
DartProgramNode >> defaultTokens: anOrderedCollection [

	defaultTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> deferredTokens [

	^ deferredTokens
]

{ #category : #generated }
DartProgramNode >> deferredTokens: anOrderedCollection [

	deferredTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> divideEqualTokens [

	^ divideEqualTokens
]

{ #category : #generated }
DartProgramNode >> divideEqualTokens: anOrderedCollection [

	divideEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> divideTokens [

	^ divideTokens
]

{ #category : #generated }
DartProgramNode >> divideTokens: anOrderedCollection [

	divideTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> doTokens [

	^ doTokens
]

{ #category : #generated }
DartProgramNode >> doTokens: anOrderedCollection [

	doTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> dynamicTokens [

	^ dynamicTokens
]

{ #category : #generated }
DartProgramNode >> dynamicTokens: anOrderedCollection [

	dynamicTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> elseTokens [

	^ elseTokens
]

{ #category : #generated }
DartProgramNode >> elseTokens: anOrderedCollection [

	elseTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> equalEqualTokens [

	^ equalEqualTokens
]

{ #category : #generated }
DartProgramNode >> equalEqualTokens: anOrderedCollection [

	equalEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> equalGreaterThanTokens [

	^ equalGreaterThanTokens
]

{ #category : #generated }
DartProgramNode >> equalGreaterThanTokens: anOrderedCollection [

	equalGreaterThanTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> equalTokens [

	^ equalTokens
]

{ #category : #generated }
DartProgramNode >> equalTokens: anOrderedCollection [

	equalTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> exportTokens [

	^ exportTokens
]

{ #category : #generated }
DartProgramNode >> exportTokens: anOrderedCollection [

	exportTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> extendsTokens [

	^ extendsTokens
]

{ #category : #generated }
DartProgramNode >> extendsTokens: anOrderedCollection [

	extendsTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> extensionTokens [

	^ extensionTokens
]

{ #category : #generated }
DartProgramNode >> extensionTokens: anOrderedCollection [

	extensionTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> externalTokens [

	^ externalTokens
]

{ #category : #generated }
DartProgramNode >> externalTokens: anOrderedCollection [

	externalTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> factoryTokens [

	^ factoryTokens
]

{ #category : #generated }
DartProgramNode >> factoryTokens: anOrderedCollection [

	factoryTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> falseTokens [

	^ falseTokens
]

{ #category : #generated }
DartProgramNode >> falseTokens: anOrderedCollection [

	falseTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> finalTokens [

	^ finalTokens
]

{ #category : #generated }
DartProgramNode >> finalTokens: anOrderedCollection [

	finalTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> finallyTokens [

	^ finallyTokens
]

{ #category : #generated }
DartProgramNode >> finallyTokens: anOrderedCollection [

	finallyTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> forTokens [

	^ forTokens
]

{ #category : #generated }
DartProgramNode >> forTokens: anOrderedCollection [

	forTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> functionTokens [

	^ functionTokens
]

{ #category : #generated }
DartProgramNode >> functionTokens: anOrderedCollection [

	functionTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> getTokens [

	^ getTokens
]

{ #category : #generated }
DartProgramNode >> getTokens: anOrderedCollection [

	getTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> greaterThanTokens [

	^ greaterThanTokens
]

{ #category : #generated }
DartProgramNode >> greaterThanTokens: anOrderedCollection [

	greaterThanTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> hEX_NUMBERTokens [

	^ hEX_NUMBERTokens
]

{ #category : #generated }
DartProgramNode >> hEX_NUMBERTokens: anOrderedCollection [

	hEX_NUMBERTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> hideTokens [

	^ hideTokens
]

{ #category : #generated }
DartProgramNode >> hideTokens: anOrderedCollection [

	hideTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> identifierTokens [

	^ identifierTokens
]

{ #category : #generated }
DartProgramNode >> identifierTokens: anOrderedCollection [

	identifierTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> ifTokens [

	^ ifTokens
]

{ #category : #generated }
DartProgramNode >> ifTokens: anOrderedCollection [

	ifTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> implementsTokens [

	^ implementsTokens
]

{ #category : #generated }
DartProgramNode >> implementsTokens: anOrderedCollection [

	implementsTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> importTokens [

	^ importTokens
]

{ #category : #generated }
DartProgramNode >> importTokens: anOrderedCollection [

	importTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> inTokens [

	^ inTokens
]

{ #category : #generated }
DartProgramNode >> inTokens: anOrderedCollection [

	inTokens := anOrderedCollection
]

{ #category : #'generated-initialize-release' }
DartProgramNode >> initialize [
	super initialize.
	lbraceTokens := OrderedCollection new: 2.
	equalGreaterThanTokens := OrderedCollection new: 2.
	semicolonTokens := OrderedCollection new: 2.
	asyncTokens := OrderedCollection new: 2.
	timesTokens := OrderedCollection new: 2.
	syncTokens := OrderedCollection new: 2.
	_Tokens := OrderedCollection new: 2.
	staticTokens := OrderedCollection new: 2.
	rbraceTokens := OrderedCollection new: 2.
	externalTokens := OrderedCollection new: 2.
	finalTokens := OrderedCollection new: 2.
	covariantTokens := OrderedCollection new: 2.
	varTokens := OrderedCollection new: 2.
	abstractTokens := OrderedCollection new: 2.
	constTokens := OrderedCollection new: 2.
	lateTokens := OrderedCollection new: 2.
	equalTokens := OrderedCollection new: 2.
	commaTokens := OrderedCollection new: 2.
	leftParenTokens := OrderedCollection new: 2.
	rightParenTokens := OrderedCollection new: 2.
	colonTokens := OrderedCollection new: 2.
	factoryTokens := OrderedCollection new: 2.
	periodTokens := OrderedCollection new: 2.
	newTokens := OrderedCollection new: 2.
	getTokens := OrderedCollection new: 2.
	setTokens := OrderedCollection new: 2.
	operatorTokens := OrderedCollection new: 2.
	identifierTokens := OrderedCollection new: 2.
	hideTokens := OrderedCollection new: 2.
	ofTokens := OrderedCollection new: 2.
	onTokens := OrderedCollection new: 2.
	showTokens := OrderedCollection new: 2.
	awaitTokens := OrderedCollection new: 2.
	yieldTokens := OrderedCollection new: 2.
	functionTokens := OrderedCollection new: 2.
	questionMarkTokens := OrderedCollection new: 2.
	thisTokens := OrderedCollection new: 2.
	superTokens := OrderedCollection new: 2.
	lessThanTokens := OrderedCollection new: 2.
	extendsTokens := OrderedCollection new: 2.
	greaterThanTokens := OrderedCollection new: 2.
	tildeTokens := OrderedCollection new: 2.
	leftBracketTokens := OrderedCollection new: 2.
	rightBracketTokens := OrderedCollection new: 2.
	throwTokens := OrderedCollection new: 2.
	periodPeriodTokens := OrderedCollection new: 2.
	questionMarkPeriodPeriodTokens := OrderedCollection new: 2.
	voidTokens := OrderedCollection new: 2.
	dynamicTokens := OrderedCollection new: 2.
	assertTokens := OrderedCollection new: 2.
	asTokens := OrderedCollection new: 2.
	deferredTokens := OrderedCollection new: 2.
	exportTokens := OrderedCollection new: 2.
	extensionTokens := OrderedCollection new: 2.
	implementsTokens := OrderedCollection new: 2.
	importTokens := OrderedCollection new: 2.
	interfaceTokens := OrderedCollection new: 2.
	libraryTokens := OrderedCollection new: 2.
	mixinTokens := OrderedCollection new: 2.
	partTokens := OrderedCollection new: 2.
	requiredTokens := OrderedCollection new: 2.
	typedefTokens := OrderedCollection new: 2.
	equalEqualTokens := OrderedCollection new: 2.
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
	nullTokens := OrderedCollection new: 2.
	trueTokens := OrderedCollection new: 2.
	falseTokens := OrderedCollection new: 2.
	numberTokens := OrderedCollection new: 2.
	hEX_NUMBERTokens := OrderedCollection new: 2.
	orOrTokens := OrderedCollection new: 2.
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
	periodPeriodPeriodTokens := OrderedCollection new: 2.
	periodPeriodPeriodQuestionMarkTokens := OrderedCollection new: 2.
	ifTokens := OrderedCollection new: 2.
	elseTokens := OrderedCollection new: 2.
	forTokens := OrderedCollection new: 2.
	andAndTokens := OrderedCollection new: 2.
	whileTokens := OrderedCollection new: 2.
	doTokens := OrderedCollection new: 2.
	switchTokens := OrderedCollection new: 2.
	rethrowTokens := OrderedCollection new: 2.
	tryTokens := OrderedCollection new: 2.
	breakTokens := OrderedCollection new: 2.
	continueTokens := OrderedCollection new: 2.
	returnTokens := OrderedCollection new: 2.
	mULTI_LINE_STRING_SQ_MID_MIDTokens := OrderedCollection new: 2.
	mULTI_LINE_STRING_DQ_MID_MIDTokens := OrderedCollection new: 2.
	sINGLE_LINE_STRING_SQ_MID_MIDTokens := OrderedCollection new: 2.
	sINGLE_LINE_STRING_DQ_MID_MIDTokens := OrderedCollection new: 2.
	inTokens := OrderedCollection new: 2.
	bangEqualTokens := OrderedCollection new: 2.
	caseTokens := OrderedCollection new: 2.
	defaultTokens := OrderedCollection new: 2.
	finallyTokens := OrderedCollection new: 2.
	isTokens := OrderedCollection new: 2.
	catchTokens := OrderedCollection new: 2.
	plusPlusTokens := OrderedCollection new: 2.
	minusMinusTokens := OrderedCollection new: 2.
]

{ #category : #generated }
DartProgramNode >> interfaceTokens [

	^ interfaceTokens
]

{ #category : #generated }
DartProgramNode >> interfaceTokens: anOrderedCollection [

	interfaceTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> isTokens [

	^ isTokens
]

{ #category : #generated }
DartProgramNode >> isTokens: anOrderedCollection [

	isTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> lateTokens [

	^ lateTokens
]

{ #category : #generated }
DartProgramNode >> lateTokens: anOrderedCollection [

	lateTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> lbraceTokens [

	^ lbraceTokens
]

{ #category : #generated }
DartProgramNode >> lbraceTokens: anOrderedCollection [

	lbraceTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> leftBracketTokens [

	^ leftBracketTokens
]

{ #category : #generated }
DartProgramNode >> leftBracketTokens: anOrderedCollection [

	leftBracketTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> leftParenTokens [

	^ leftParenTokens
]

{ #category : #generated }
DartProgramNode >> leftParenTokens: anOrderedCollection [

	leftParenTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> lessThanEqualTokens [

	^ lessThanEqualTokens
]

{ #category : #generated }
DartProgramNode >> lessThanEqualTokens: anOrderedCollection [

	lessThanEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> lessThanLessThanEqualTokens [

	^ lessThanLessThanEqualTokens
]

{ #category : #generated }
DartProgramNode >> lessThanLessThanEqualTokens: anOrderedCollection [

	lessThanLessThanEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> lessThanLessThanTokens [

	^ lessThanLessThanTokens
]

{ #category : #generated }
DartProgramNode >> lessThanLessThanTokens: anOrderedCollection [

	lessThanLessThanTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> lessThanTokens [

	^ lessThanTokens
]

{ #category : #generated }
DartProgramNode >> lessThanTokens: anOrderedCollection [

	lessThanTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> libraryTokens [

	^ libraryTokens
]

{ #category : #generated }
DartProgramNode >> libraryTokens: anOrderedCollection [

	libraryTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> mULTI_LINE_STRING_DQ_BEGIN_ENDTokens [

	^ mULTI_LINE_STRING_DQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartProgramNode >> mULTI_LINE_STRING_DQ_BEGIN_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> mULTI_LINE_STRING_DQ_BEGIN_MIDTokens [

	^ mULTI_LINE_STRING_DQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartProgramNode >> mULTI_LINE_STRING_DQ_BEGIN_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> mULTI_LINE_STRING_DQ_MID_ENDTokens [

	^ mULTI_LINE_STRING_DQ_MID_ENDTokens
]

{ #category : #generated }
DartProgramNode >> mULTI_LINE_STRING_DQ_MID_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> mULTI_LINE_STRING_DQ_MID_MIDTokens [

	^ mULTI_LINE_STRING_DQ_MID_MIDTokens
]

{ #category : #generated }
DartProgramNode >> mULTI_LINE_STRING_DQ_MID_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> mULTI_LINE_STRING_SQ_BEGIN_ENDTokens [

	^ mULTI_LINE_STRING_SQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartProgramNode >> mULTI_LINE_STRING_SQ_BEGIN_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> mULTI_LINE_STRING_SQ_BEGIN_MIDTokens [

	^ mULTI_LINE_STRING_SQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartProgramNode >> mULTI_LINE_STRING_SQ_BEGIN_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> mULTI_LINE_STRING_SQ_MID_ENDTokens [

	^ mULTI_LINE_STRING_SQ_MID_ENDTokens
]

{ #category : #generated }
DartProgramNode >> mULTI_LINE_STRING_SQ_MID_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> mULTI_LINE_STRING_SQ_MID_MIDTokens [

	^ mULTI_LINE_STRING_SQ_MID_MIDTokens
]

{ #category : #generated }
DartProgramNode >> mULTI_LINE_STRING_SQ_MID_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> minusEqualTokens [

	^ minusEqualTokens
]

{ #category : #generated }
DartProgramNode >> minusEqualTokens: anOrderedCollection [

	minusEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> minusMinusTokens [

	^ minusMinusTokens
]

{ #category : #generated }
DartProgramNode >> minusMinusTokens: anOrderedCollection [

	minusMinusTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> minusTokens [

	^ minusTokens
]

{ #category : #generated }
DartProgramNode >> minusTokens: anOrderedCollection [

	minusTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> mixinTokens [

	^ mixinTokens
]

{ #category : #generated }
DartProgramNode >> mixinTokens: anOrderedCollection [

	mixinTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> newTokens [

	^ newTokens
]

{ #category : #generated }
DartProgramNode >> newTokens: anOrderedCollection [

	newTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> nullTokens [

	^ nullTokens
]

{ #category : #generated }
DartProgramNode >> nullTokens: anOrderedCollection [

	nullTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> numberTokens [

	^ numberTokens
]

{ #category : #generated }
DartProgramNode >> numberTokens: anOrderedCollection [

	numberTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> ofTokens [

	^ ofTokens
]

{ #category : #generated }
DartProgramNode >> ofTokens: anOrderedCollection [

	ofTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> onTokens [

	^ onTokens
]

{ #category : #generated }
DartProgramNode >> onTokens: anOrderedCollection [

	onTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> operatorTokens [

	^ operatorTokens
]

{ #category : #generated }
DartProgramNode >> operatorTokens: anOrderedCollection [

	operatorTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> orEqualTokens [

	^ orEqualTokens
]

{ #category : #generated }
DartProgramNode >> orEqualTokens: anOrderedCollection [

	orEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> orOrTokens [

	^ orOrTokens
]

{ #category : #generated }
DartProgramNode >> orOrTokens: anOrderedCollection [

	orOrTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> orTokens [

	^ orTokens
]

{ #category : #generated }
DartProgramNode >> orTokens: anOrderedCollection [

	orTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> partTokens [

	^ partTokens
]

{ #category : #generated }
DartProgramNode >> partTokens: anOrderedCollection [

	partTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> percentEqualTokens [

	^ percentEqualTokens
]

{ #category : #generated }
DartProgramNode >> percentEqualTokens: anOrderedCollection [

	percentEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> percentTokens [

	^ percentTokens
]

{ #category : #generated }
DartProgramNode >> percentTokens: anOrderedCollection [

	percentTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> periodPeriodPeriodQuestionMarkTokens [

	^ periodPeriodPeriodQuestionMarkTokens
]

{ #category : #generated }
DartProgramNode >> periodPeriodPeriodQuestionMarkTokens: anOrderedCollection [

	periodPeriodPeriodQuestionMarkTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> periodPeriodPeriodTokens [

	^ periodPeriodPeriodTokens
]

{ #category : #generated }
DartProgramNode >> periodPeriodPeriodTokens: anOrderedCollection [

	periodPeriodPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> periodPeriodTokens [

	^ periodPeriodTokens
]

{ #category : #generated }
DartProgramNode >> periodPeriodTokens: anOrderedCollection [

	periodPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> periodTokens [

	^ periodTokens
]

{ #category : #generated }
DartProgramNode >> periodTokens: anOrderedCollection [

	periodTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> plusEqualTokens [

	^ plusEqualTokens
]

{ #category : #generated }
DartProgramNode >> plusEqualTokens: anOrderedCollection [

	plusEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> plusPlusTokens [

	^ plusPlusTokens
]

{ #category : #generated }
DartProgramNode >> plusPlusTokens: anOrderedCollection [

	plusPlusTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> plusTokens [

	^ plusTokens
]

{ #category : #generated }
DartProgramNode >> plusTokens: anOrderedCollection [

	plusTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> questionMarkPeriodPeriodTokens [

	^ questionMarkPeriodPeriodTokens
]

{ #category : #generated }
DartProgramNode >> questionMarkPeriodPeriodTokens: anOrderedCollection [

	questionMarkPeriodPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> questionMarkPeriodTokens [

	^ questionMarkPeriodTokens
]

{ #category : #generated }
DartProgramNode >> questionMarkPeriodTokens: anOrderedCollection [

	questionMarkPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> questionMarkQuestionMarkEqualTokens [

	^ questionMarkQuestionMarkEqualTokens
]

{ #category : #generated }
DartProgramNode >> questionMarkQuestionMarkEqualTokens: anOrderedCollection [

	questionMarkQuestionMarkEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> questionMarkQuestionMarkTokens [

	^ questionMarkQuestionMarkTokens
]

{ #category : #generated }
DartProgramNode >> questionMarkQuestionMarkTokens: anOrderedCollection [

	questionMarkQuestionMarkTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> questionMarkTokens [

	^ questionMarkTokens
]

{ #category : #generated }
DartProgramNode >> questionMarkTokens: anOrderedCollection [

	questionMarkTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> rAW_MULTI_LINE_STRINGTokens [

	^ rAW_MULTI_LINE_STRINGTokens
]

{ #category : #generated }
DartProgramNode >> rAW_MULTI_LINE_STRINGTokens: anOrderedCollection [

	rAW_MULTI_LINE_STRINGTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> rAW_SINGLE_LINE_STRINGTokens [

	^ rAW_SINGLE_LINE_STRINGTokens
]

{ #category : #generated }
DartProgramNode >> rAW_SINGLE_LINE_STRINGTokens: anOrderedCollection [

	rAW_SINGLE_LINE_STRINGTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> rbraceTokens [

	^ rbraceTokens
]

{ #category : #generated }
DartProgramNode >> rbraceTokens: anOrderedCollection [

	rbraceTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> requiredTokens [

	^ requiredTokens
]

{ #category : #generated }
DartProgramNode >> requiredTokens: anOrderedCollection [

	requiredTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> rethrowTokens [

	^ rethrowTokens
]

{ #category : #generated }
DartProgramNode >> rethrowTokens: anOrderedCollection [

	rethrowTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> returnTokens [

	^ returnTokens
]

{ #category : #generated }
DartProgramNode >> returnTokens: anOrderedCollection [

	returnTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> rightBracketTokens [

	^ rightBracketTokens
]

{ #category : #generated }
DartProgramNode >> rightBracketTokens: anOrderedCollection [

	rightBracketTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> rightParenTokens [

	^ rightParenTokens
]

{ #category : #generated }
DartProgramNode >> rightParenTokens: anOrderedCollection [

	rightParenTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens [

	^ sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartProgramNode >> sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens [

	^ sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartProgramNode >> sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> sINGLE_LINE_STRING_DQ_MID_ENDTokens [

	^ sINGLE_LINE_STRING_DQ_MID_ENDTokens
]

{ #category : #generated }
DartProgramNode >> sINGLE_LINE_STRING_DQ_MID_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> sINGLE_LINE_STRING_DQ_MID_MIDTokens [

	^ sINGLE_LINE_STRING_DQ_MID_MIDTokens
]

{ #category : #generated }
DartProgramNode >> sINGLE_LINE_STRING_DQ_MID_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens [

	^ sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartProgramNode >> sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens [

	^ sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartProgramNode >> sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> sINGLE_LINE_STRING_SQ_MID_ENDTokens [

	^ sINGLE_LINE_STRING_SQ_MID_ENDTokens
]

{ #category : #generated }
DartProgramNode >> sINGLE_LINE_STRING_SQ_MID_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> sINGLE_LINE_STRING_SQ_MID_MIDTokens [

	^ sINGLE_LINE_STRING_SQ_MID_MIDTokens
]

{ #category : #generated }
DartProgramNode >> sINGLE_LINE_STRING_SQ_MID_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> semicolonTokens [

	^ semicolonTokens
]

{ #category : #generated }
DartProgramNode >> semicolonTokens: anOrderedCollection [

	semicolonTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> setTokens [

	^ setTokens
]

{ #category : #generated }
DartProgramNode >> setTokens: anOrderedCollection [

	setTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> showTokens [

	^ showTokens
]

{ #category : #generated }
DartProgramNode >> showTokens: anOrderedCollection [

	showTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> staticTokens [

	^ staticTokens
]

{ #category : #generated }
DartProgramNode >> staticTokens: anOrderedCollection [

	staticTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> superTokens [

	^ superTokens
]

{ #category : #generated }
DartProgramNode >> superTokens: anOrderedCollection [

	superTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> switchTokens [

	^ switchTokens
]

{ #category : #generated }
DartProgramNode >> switchTokens: anOrderedCollection [

	switchTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> syncTokens [

	^ syncTokens
]

{ #category : #generated }
DartProgramNode >> syncTokens: anOrderedCollection [

	syncTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> thisTokens [

	^ thisTokens
]

{ #category : #generated }
DartProgramNode >> thisTokens: anOrderedCollection [

	thisTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> throwTokens [

	^ throwTokens
]

{ #category : #generated }
DartProgramNode >> throwTokens: anOrderedCollection [

	throwTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> tildeDivideEqualTokens [

	^ tildeDivideEqualTokens
]

{ #category : #generated }
DartProgramNode >> tildeDivideEqualTokens: anOrderedCollection [

	tildeDivideEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> tildeDivideTokens [

	^ tildeDivideTokens
]

{ #category : #generated }
DartProgramNode >> tildeDivideTokens: anOrderedCollection [

	tildeDivideTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> tildeTokens [

	^ tildeTokens
]

{ #category : #generated }
DartProgramNode >> tildeTokens: anOrderedCollection [

	tildeTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> timesEqualTokens [

	^ timesEqualTokens
]

{ #category : #generated }
DartProgramNode >> timesEqualTokens: anOrderedCollection [

	timesEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> timesTokens [

	^ timesTokens
]

{ #category : #generated }
DartProgramNode >> timesTokens: anOrderedCollection [

	timesTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> trueTokens [

	^ trueTokens
]

{ #category : #generated }
DartProgramNode >> trueTokens: anOrderedCollection [

	trueTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> tryTokens [

	^ tryTokens
]

{ #category : #generated }
DartProgramNode >> tryTokens: anOrderedCollection [

	tryTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> typedefTokens [

	^ typedefTokens
]

{ #category : #generated }
DartProgramNode >> typedefTokens: anOrderedCollection [

	typedefTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> upArrowEqualTokens [

	^ upArrowEqualTokens
]

{ #category : #generated }
DartProgramNode >> upArrowEqualTokens: anOrderedCollection [

	upArrowEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> upArrowTokens [

	^ upArrowTokens
]

{ #category : #generated }
DartProgramNode >> upArrowTokens: anOrderedCollection [

	upArrowTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> varTokens [

	^ varTokens
]

{ #category : #generated }
DartProgramNode >> varTokens: anOrderedCollection [

	varTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> voidTokens [

	^ voidTokens
]

{ #category : #generated }
DartProgramNode >> voidTokens: anOrderedCollection [

	voidTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> whileTokens [

	^ whileTokens
]

{ #category : #generated }
DartProgramNode >> whileTokens: anOrderedCollection [

	whileTokens := anOrderedCollection
]

{ #category : #generated }
DartProgramNode >> yieldTokens [

	^ yieldTokens
]

{ #category : #generated }
DartProgramNode >> yieldTokens: anOrderedCollection [

	yieldTokens := anOrderedCollection
]
