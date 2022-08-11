Class {
	#name : #DartTypeNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'typeIdentifier',
		'lessThanTokens',
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
		'whileTokens',
		'doTokens',
		'switchTokens',
		'rethrowTokens',
		'tryTokens',
		'breakTokens',
		'continueTokens',
		'returnTokens',
		'expressions',
		'blocks',
		'isTokens',
		'caseTokens',
		'defaultTokens',
		'finallyTokens',
		'assertTokens',
		'catchTokens',
		'plusPlusTokens',
		'minusMinusTokens'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartTypeNode >> _Tokens [

	^ _Tokens
]

{ #category : #generated }
DartTypeNode >> _Tokens: anOrderedCollection [

	_Tokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> abstractTokens [

	^ abstractTokens
]

{ #category : #generated }
DartTypeNode >> abstractTokens: anOrderedCollection [

	abstractTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitType: self
]

{ #category : #generated }
DartTypeNode >> andAndTokens [

	^ andAndTokens
]

{ #category : #generated }
DartTypeNode >> andAndTokens: anOrderedCollection [

	andAndTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> andEqualTokens [

	^ andEqualTokens
]

{ #category : #generated }
DartTypeNode >> andEqualTokens: anOrderedCollection [

	andEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> andTokens [

	^ andTokens
]

{ #category : #generated }
DartTypeNode >> andTokens: anOrderedCollection [

	andTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> asTokens [

	^ asTokens
]

{ #category : #generated }
DartTypeNode >> asTokens: anOrderedCollection [

	asTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> assertTokens [

	^ assertTokens
]

{ #category : #generated }
DartTypeNode >> assertTokens: anOrderedCollection [

	assertTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> asyncTokens [

	^ asyncTokens
]

{ #category : #generated }
DartTypeNode >> asyncTokens: anOrderedCollection [

	asyncTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> awaitTokens [

	^ awaitTokens
]

{ #category : #generated }
DartTypeNode >> awaitTokens: anOrderedCollection [

	awaitTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> bangEqualTokens [

	^ bangEqualTokens
]

{ #category : #generated }
DartTypeNode >> bangEqualTokens: anOrderedCollection [

	bangEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> bangTokens [

	^ bangTokens
]

{ #category : #generated }
DartTypeNode >> bangTokens: anOrderedCollection [

	bangTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> blocks [

	^ blocks
]

{ #category : #generated }
DartTypeNode >> blocks: anOrderedCollection [

	self setParent: self blocks to: nil.
	blocks := anOrderedCollection.
	self setParent: self blocks to: self
]

{ #category : #generated }
DartTypeNode >> breakTokens [

	^ breakTokens
]

{ #category : #generated }
DartTypeNode >> breakTokens: anOrderedCollection [

	breakTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> caseTokens [

	^ caseTokens
]

{ #category : #generated }
DartTypeNode >> caseTokens: anOrderedCollection [

	caseTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> catchTokens [

	^ catchTokens
]

{ #category : #generated }
DartTypeNode >> catchTokens: anOrderedCollection [

	catchTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> colonTokens [

	^ colonTokens
]

{ #category : #generated }
DartTypeNode >> colonTokens: anOrderedCollection [

	colonTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> commaTokens [

	^ commaTokens
]

{ #category : #generated }
DartTypeNode >> commaTokens: anOrderedCollection [

	commaTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> compositeTokenVariables [

	^ #( #lessThanTokens #_Tokens #identifierTokens #dynamicTokens
	     #asyncTokens #hideTokens #ofTokens #onTokens #showTokens
	     #syncTokens #awaitTokens #yieldTokens #periodTokens
	     #functionTokens #newTokens #greaterThanTokens #leftParenTokens
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
	     #inTokens #semicolonTokens #whileTokens #doTokens #switchTokens
	     #rethrowTokens #tryTokens #breakTokens #continueTokens
	     #returnTokens #isTokens #caseTokens #defaultTokens
	     #finallyTokens #assertTokens #catchTokens #plusPlusTokens
	     #minusMinusTokens )
]

{ #category : #generated }
DartTypeNode >> constTokens [

	^ constTokens
]

{ #category : #generated }
DartTypeNode >> constTokens: anOrderedCollection [

	constTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> continueTokens [

	^ continueTokens
]

{ #category : #generated }
DartTypeNode >> continueTokens: anOrderedCollection [

	continueTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> covariantTokens [

	^ covariantTokens
]

{ #category : #generated }
DartTypeNode >> covariantTokens: anOrderedCollection [

	covariantTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> defaultTokens [

	^ defaultTokens
]

{ #category : #generated }
DartTypeNode >> defaultTokens: anOrderedCollection [

	defaultTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> deferredTokens [

	^ deferredTokens
]

{ #category : #generated }
DartTypeNode >> deferredTokens: anOrderedCollection [

	deferredTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> divideEqualTokens [

	^ divideEqualTokens
]

{ #category : #generated }
DartTypeNode >> divideEqualTokens: anOrderedCollection [

	divideEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> divideTokens [

	^ divideTokens
]

{ #category : #generated }
DartTypeNode >> divideTokens: anOrderedCollection [

	divideTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> doTokens [

	^ doTokens
]

{ #category : #generated }
DartTypeNode >> doTokens: anOrderedCollection [

	doTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> dynamicTokens [

	^ dynamicTokens
]

{ #category : #generated }
DartTypeNode >> dynamicTokens: anOrderedCollection [

	dynamicTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> elseTokens [

	^ elseTokens
]

{ #category : #generated }
DartTypeNode >> elseTokens: anOrderedCollection [

	elseTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> equalEqualTokens [

	^ equalEqualTokens
]

{ #category : #generated }
DartTypeNode >> equalEqualTokens: anOrderedCollection [

	equalEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> equalGreaterThanTokens [

	^ equalGreaterThanTokens
]

{ #category : #generated }
DartTypeNode >> equalGreaterThanTokens: anOrderedCollection [

	equalGreaterThanTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> equalTokens [

	^ equalTokens
]

{ #category : #generated }
DartTypeNode >> equalTokens: anOrderedCollection [

	equalTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> exportTokens [

	^ exportTokens
]

{ #category : #generated }
DartTypeNode >> exportTokens: anOrderedCollection [

	exportTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> expressions [

	^ expressions
]

{ #category : #generated }
DartTypeNode >> expressions: anOrderedCollection [

	self setParent: self expressions to: nil.
	expressions := anOrderedCollection.
	self setParent: self expressions to: self
]

{ #category : #generated }
DartTypeNode >> extendsTokens [

	^ extendsTokens
]

{ #category : #generated }
DartTypeNode >> extendsTokens: anOrderedCollection [

	extendsTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> extensionTokens [

	^ extensionTokens
]

{ #category : #generated }
DartTypeNode >> extensionTokens: anOrderedCollection [

	extensionTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> externalTokens [

	^ externalTokens
]

{ #category : #generated }
DartTypeNode >> externalTokens: anOrderedCollection [

	externalTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> factoryTokens [

	^ factoryTokens
]

{ #category : #generated }
DartTypeNode >> factoryTokens: anOrderedCollection [

	factoryTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> falseTokens [

	^ falseTokens
]

{ #category : #generated }
DartTypeNode >> falseTokens: anOrderedCollection [

	falseTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> finalTokens [

	^ finalTokens
]

{ #category : #generated }
DartTypeNode >> finalTokens: anOrderedCollection [

	finalTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> finallyTokens [

	^ finallyTokens
]

{ #category : #generated }
DartTypeNode >> finallyTokens: anOrderedCollection [

	finallyTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> forTokens [

	^ forTokens
]

{ #category : #generated }
DartTypeNode >> forTokens: anOrderedCollection [

	forTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> functionTokens [

	^ functionTokens
]

{ #category : #generated }
DartTypeNode >> functionTokens: anOrderedCollection [

	functionTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> getTokens [

	^ getTokens
]

{ #category : #generated }
DartTypeNode >> getTokens: anOrderedCollection [

	getTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> greaterThanTokens [

	^ greaterThanTokens
]

{ #category : #generated }
DartTypeNode >> greaterThanTokens: anOrderedCollection [

	greaterThanTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> hEX_NUMBERTokens [

	^ hEX_NUMBERTokens
]

{ #category : #generated }
DartTypeNode >> hEX_NUMBERTokens: anOrderedCollection [

	hEX_NUMBERTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> hideTokens [

	^ hideTokens
]

{ #category : #generated }
DartTypeNode >> hideTokens: anOrderedCollection [

	hideTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> identifierTokens [

	^ identifierTokens
]

{ #category : #generated }
DartTypeNode >> identifierTokens: anOrderedCollection [

	identifierTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> ifTokens [

	^ ifTokens
]

{ #category : #generated }
DartTypeNode >> ifTokens: anOrderedCollection [

	ifTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> implementsTokens [

	^ implementsTokens
]

{ #category : #generated }
DartTypeNode >> implementsTokens: anOrderedCollection [

	implementsTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> importTokens [

	^ importTokens
]

{ #category : #generated }
DartTypeNode >> importTokens: anOrderedCollection [

	importTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> inTokens [

	^ inTokens
]

{ #category : #generated }
DartTypeNode >> inTokens: anOrderedCollection [

	inTokens := anOrderedCollection
]

{ #category : #'generated-initialize-release' }
DartTypeNode >> initialize [
	super initialize.
	lessThanTokens := OrderedCollection new: 2.
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
	whileTokens := OrderedCollection new: 2.
	doTokens := OrderedCollection new: 2.
	switchTokens := OrderedCollection new: 2.
	rethrowTokens := OrderedCollection new: 2.
	tryTokens := OrderedCollection new: 2.
	breakTokens := OrderedCollection new: 2.
	continueTokens := OrderedCollection new: 2.
	returnTokens := OrderedCollection new: 2.
	expressions := OrderedCollection new: 2.
	blocks := OrderedCollection new: 2.
	isTokens := OrderedCollection new: 2.
	caseTokens := OrderedCollection new: 2.
	defaultTokens := OrderedCollection new: 2.
	finallyTokens := OrderedCollection new: 2.
	assertTokens := OrderedCollection new: 2.
	catchTokens := OrderedCollection new: 2.
	plusPlusTokens := OrderedCollection new: 2.
	minusMinusTokens := OrderedCollection new: 2.
]

{ #category : #generated }
DartTypeNode >> interfaceTokens [

	^ interfaceTokens
]

{ #category : #generated }
DartTypeNode >> interfaceTokens: anOrderedCollection [

	interfaceTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> isTokens [

	^ isTokens
]

{ #category : #generated }
DartTypeNode >> isTokens: anOrderedCollection [

	isTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> lateTokens [

	^ lateTokens
]

{ #category : #generated }
DartTypeNode >> lateTokens: anOrderedCollection [

	lateTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> lbraceTokens [

	^ lbraceTokens
]

{ #category : #generated }
DartTypeNode >> lbraceTokens: anOrderedCollection [

	lbraceTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> leftBracketTokens [

	^ leftBracketTokens
]

{ #category : #generated }
DartTypeNode >> leftBracketTokens: anOrderedCollection [

	leftBracketTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> leftParenTokens [

	^ leftParenTokens
]

{ #category : #generated }
DartTypeNode >> leftParenTokens: anOrderedCollection [

	leftParenTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> lessThanEqualTokens [

	^ lessThanEqualTokens
]

{ #category : #generated }
DartTypeNode >> lessThanEqualTokens: anOrderedCollection [

	lessThanEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> lessThanLessThanEqualTokens [

	^ lessThanLessThanEqualTokens
]

{ #category : #generated }
DartTypeNode >> lessThanLessThanEqualTokens: anOrderedCollection [

	lessThanLessThanEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> lessThanLessThanTokens [

	^ lessThanLessThanTokens
]

{ #category : #generated }
DartTypeNode >> lessThanLessThanTokens: anOrderedCollection [

	lessThanLessThanTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> lessThanTokens [

	^ lessThanTokens
]

{ #category : #generated }
DartTypeNode >> lessThanTokens: anOrderedCollection [

	lessThanTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> libraryTokens [

	^ libraryTokens
]

{ #category : #generated }
DartTypeNode >> libraryTokens: anOrderedCollection [

	libraryTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> mULTI_LINE_STRING_DQ_BEGIN_ENDTokens [

	^ mULTI_LINE_STRING_DQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartTypeNode >> mULTI_LINE_STRING_DQ_BEGIN_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> mULTI_LINE_STRING_DQ_BEGIN_MIDTokens [

	^ mULTI_LINE_STRING_DQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartTypeNode >> mULTI_LINE_STRING_DQ_BEGIN_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> mULTI_LINE_STRING_DQ_MID_ENDTokens [

	^ mULTI_LINE_STRING_DQ_MID_ENDTokens
]

{ #category : #generated }
DartTypeNode >> mULTI_LINE_STRING_DQ_MID_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> mULTI_LINE_STRING_DQ_MID_MIDTokens [

	^ mULTI_LINE_STRING_DQ_MID_MIDTokens
]

{ #category : #generated }
DartTypeNode >> mULTI_LINE_STRING_DQ_MID_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> mULTI_LINE_STRING_SQ_BEGIN_ENDTokens [

	^ mULTI_LINE_STRING_SQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartTypeNode >> mULTI_LINE_STRING_SQ_BEGIN_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> mULTI_LINE_STRING_SQ_BEGIN_MIDTokens [

	^ mULTI_LINE_STRING_SQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartTypeNode >> mULTI_LINE_STRING_SQ_BEGIN_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> mULTI_LINE_STRING_SQ_MID_ENDTokens [

	^ mULTI_LINE_STRING_SQ_MID_ENDTokens
]

{ #category : #generated }
DartTypeNode >> mULTI_LINE_STRING_SQ_MID_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> mULTI_LINE_STRING_SQ_MID_MIDTokens [

	^ mULTI_LINE_STRING_SQ_MID_MIDTokens
]

{ #category : #generated }
DartTypeNode >> mULTI_LINE_STRING_SQ_MID_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> minusEqualTokens [

	^ minusEqualTokens
]

{ #category : #generated }
DartTypeNode >> minusEqualTokens: anOrderedCollection [

	minusEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> minusMinusTokens [

	^ minusMinusTokens
]

{ #category : #generated }
DartTypeNode >> minusMinusTokens: anOrderedCollection [

	minusMinusTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> minusTokens [

	^ minusTokens
]

{ #category : #generated }
DartTypeNode >> minusTokens: anOrderedCollection [

	minusTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> mixinTokens [

	^ mixinTokens
]

{ #category : #generated }
DartTypeNode >> mixinTokens: anOrderedCollection [

	mixinTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> newTokens [

	^ newTokens
]

{ #category : #generated }
DartTypeNode >> newTokens: anOrderedCollection [

	newTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> nullTokens [

	^ nullTokens
]

{ #category : #generated }
DartTypeNode >> nullTokens: anOrderedCollection [

	nullTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> numberTokens [

	^ numberTokens
]

{ #category : #generated }
DartTypeNode >> numberTokens: anOrderedCollection [

	numberTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> ofTokens [

	^ ofTokens
]

{ #category : #generated }
DartTypeNode >> ofTokens: anOrderedCollection [

	ofTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> onTokens [

	^ onTokens
]

{ #category : #generated }
DartTypeNode >> onTokens: anOrderedCollection [

	onTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> operatorTokens [

	^ operatorTokens
]

{ #category : #generated }
DartTypeNode >> operatorTokens: anOrderedCollection [

	operatorTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> orEqualTokens [

	^ orEqualTokens
]

{ #category : #generated }
DartTypeNode >> orEqualTokens: anOrderedCollection [

	orEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> orOrTokens [

	^ orOrTokens
]

{ #category : #generated }
DartTypeNode >> orOrTokens: anOrderedCollection [

	orOrTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> orTokens [

	^ orTokens
]

{ #category : #generated }
DartTypeNode >> orTokens: anOrderedCollection [

	orTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> otherVariables [

	^ #( #typeIdentifier #expressions #blocks )
]

{ #category : #generated }
DartTypeNode >> partTokens [

	^ partTokens
]

{ #category : #generated }
DartTypeNode >> partTokens: anOrderedCollection [

	partTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> percentEqualTokens [

	^ percentEqualTokens
]

{ #category : #generated }
DartTypeNode >> percentEqualTokens: anOrderedCollection [

	percentEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> percentTokens [

	^ percentTokens
]

{ #category : #generated }
DartTypeNode >> percentTokens: anOrderedCollection [

	percentTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> periodPeriodPeriodQuestionMarkTokens [

	^ periodPeriodPeriodQuestionMarkTokens
]

{ #category : #generated }
DartTypeNode >> periodPeriodPeriodQuestionMarkTokens: anOrderedCollection [

	periodPeriodPeriodQuestionMarkTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> periodPeriodPeriodTokens [

	^ periodPeriodPeriodTokens
]

{ #category : #generated }
DartTypeNode >> periodPeriodPeriodTokens: anOrderedCollection [

	periodPeriodPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> periodPeriodTokens [

	^ periodPeriodTokens
]

{ #category : #generated }
DartTypeNode >> periodPeriodTokens: anOrderedCollection [

	periodPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> periodTokens [

	^ periodTokens
]

{ #category : #generated }
DartTypeNode >> periodTokens: anOrderedCollection [

	periodTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> plusEqualTokens [

	^ plusEqualTokens
]

{ #category : #generated }
DartTypeNode >> plusEqualTokens: anOrderedCollection [

	plusEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> plusPlusTokens [

	^ plusPlusTokens
]

{ #category : #generated }
DartTypeNode >> plusPlusTokens: anOrderedCollection [

	plusPlusTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> plusTokens [

	^ plusTokens
]

{ #category : #generated }
DartTypeNode >> plusTokens: anOrderedCollection [

	plusTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> questionMarkPeriodPeriodTokens [

	^ questionMarkPeriodPeriodTokens
]

{ #category : #generated }
DartTypeNode >> questionMarkPeriodPeriodTokens: anOrderedCollection [

	questionMarkPeriodPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> questionMarkPeriodTokens [

	^ questionMarkPeriodTokens
]

{ #category : #generated }
DartTypeNode >> questionMarkPeriodTokens: anOrderedCollection [

	questionMarkPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> questionMarkQuestionMarkEqualTokens [

	^ questionMarkQuestionMarkEqualTokens
]

{ #category : #generated }
DartTypeNode >> questionMarkQuestionMarkEqualTokens: anOrderedCollection [

	questionMarkQuestionMarkEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> questionMarkQuestionMarkTokens [

	^ questionMarkQuestionMarkTokens
]

{ #category : #generated }
DartTypeNode >> questionMarkQuestionMarkTokens: anOrderedCollection [

	questionMarkQuestionMarkTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> questionMarkTokens [

	^ questionMarkTokens
]

{ #category : #generated }
DartTypeNode >> questionMarkTokens: anOrderedCollection [

	questionMarkTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> rAW_MULTI_LINE_STRINGTokens [

	^ rAW_MULTI_LINE_STRINGTokens
]

{ #category : #generated }
DartTypeNode >> rAW_MULTI_LINE_STRINGTokens: anOrderedCollection [

	rAW_MULTI_LINE_STRINGTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> rAW_SINGLE_LINE_STRINGTokens [

	^ rAW_SINGLE_LINE_STRINGTokens
]

{ #category : #generated }
DartTypeNode >> rAW_SINGLE_LINE_STRINGTokens: anOrderedCollection [

	rAW_SINGLE_LINE_STRINGTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> rbraceTokens [

	^ rbraceTokens
]

{ #category : #generated }
DartTypeNode >> rbraceTokens: anOrderedCollection [

	rbraceTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> requiredTokens [

	^ requiredTokens
]

{ #category : #generated }
DartTypeNode >> requiredTokens: anOrderedCollection [

	requiredTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> rethrowTokens [

	^ rethrowTokens
]

{ #category : #generated }
DartTypeNode >> rethrowTokens: anOrderedCollection [

	rethrowTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> returnTokens [

	^ returnTokens
]

{ #category : #generated }
DartTypeNode >> returnTokens: anOrderedCollection [

	returnTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> rightBracketTokens [

	^ rightBracketTokens
]

{ #category : #generated }
DartTypeNode >> rightBracketTokens: anOrderedCollection [

	rightBracketTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> rightParenTokens [

	^ rightParenTokens
]

{ #category : #generated }
DartTypeNode >> rightParenTokens: anOrderedCollection [

	rightParenTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens [

	^ sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartTypeNode >> sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens [

	^ sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartTypeNode >> sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> sINGLE_LINE_STRING_DQ_MID_ENDTokens [

	^ sINGLE_LINE_STRING_DQ_MID_ENDTokens
]

{ #category : #generated }
DartTypeNode >> sINGLE_LINE_STRING_DQ_MID_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> sINGLE_LINE_STRING_DQ_MID_MIDTokens [

	^ sINGLE_LINE_STRING_DQ_MID_MIDTokens
]

{ #category : #generated }
DartTypeNode >> sINGLE_LINE_STRING_DQ_MID_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens [

	^ sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartTypeNode >> sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens [

	^ sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartTypeNode >> sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> sINGLE_LINE_STRING_SQ_MID_ENDTokens [

	^ sINGLE_LINE_STRING_SQ_MID_ENDTokens
]

{ #category : #generated }
DartTypeNode >> sINGLE_LINE_STRING_SQ_MID_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> sINGLE_LINE_STRING_SQ_MID_MIDTokens [

	^ sINGLE_LINE_STRING_SQ_MID_MIDTokens
]

{ #category : #generated }
DartTypeNode >> sINGLE_LINE_STRING_SQ_MID_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> semicolonTokens [

	^ semicolonTokens
]

{ #category : #generated }
DartTypeNode >> semicolonTokens: anOrderedCollection [

	semicolonTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> setTokens [

	^ setTokens
]

{ #category : #generated }
DartTypeNode >> setTokens: anOrderedCollection [

	setTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> showTokens [

	^ showTokens
]

{ #category : #generated }
DartTypeNode >> showTokens: anOrderedCollection [

	showTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> staticTokens [

	^ staticTokens
]

{ #category : #generated }
DartTypeNode >> staticTokens: anOrderedCollection [

	staticTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> superTokens [

	^ superTokens
]

{ #category : #generated }
DartTypeNode >> superTokens: anOrderedCollection [

	superTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> switchTokens [

	^ switchTokens
]

{ #category : #generated }
DartTypeNode >> switchTokens: anOrderedCollection [

	switchTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> syncTokens [

	^ syncTokens
]

{ #category : #generated }
DartTypeNode >> syncTokens: anOrderedCollection [

	syncTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> thisTokens [

	^ thisTokens
]

{ #category : #generated }
DartTypeNode >> thisTokens: anOrderedCollection [

	thisTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> throwTokens [

	^ throwTokens
]

{ #category : #generated }
DartTypeNode >> throwTokens: anOrderedCollection [

	throwTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> tildeDivideEqualTokens [

	^ tildeDivideEqualTokens
]

{ #category : #generated }
DartTypeNode >> tildeDivideEqualTokens: anOrderedCollection [

	tildeDivideEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> tildeDivideTokens [

	^ tildeDivideTokens
]

{ #category : #generated }
DartTypeNode >> tildeDivideTokens: anOrderedCollection [

	tildeDivideTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> tildeTokens [

	^ tildeTokens
]

{ #category : #generated }
DartTypeNode >> tildeTokens: anOrderedCollection [

	tildeTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> timesEqualTokens [

	^ timesEqualTokens
]

{ #category : #generated }
DartTypeNode >> timesEqualTokens: anOrderedCollection [

	timesEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> timesTokens [

	^ timesTokens
]

{ #category : #generated }
DartTypeNode >> timesTokens: anOrderedCollection [

	timesTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> trueTokens [

	^ trueTokens
]

{ #category : #generated }
DartTypeNode >> trueTokens: anOrderedCollection [

	trueTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> tryTokens [

	^ tryTokens
]

{ #category : #generated }
DartTypeNode >> tryTokens: anOrderedCollection [

	tryTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> typeIdentifier [

	^ typeIdentifier
]

{ #category : #generated }
DartTypeNode >> typeIdentifier: anOrderedCollection [

	self setParent: self typeIdentifier to: nil.
	typeIdentifier := anOrderedCollection.
	self setParent: self typeIdentifier to: self
]

{ #category : #generated }
DartTypeNode >> typedefTokens [

	^ typedefTokens
]

{ #category : #generated }
DartTypeNode >> typedefTokens: anOrderedCollection [

	typedefTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> upArrowEqualTokens [

	^ upArrowEqualTokens
]

{ #category : #generated }
DartTypeNode >> upArrowEqualTokens: anOrderedCollection [

	upArrowEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> upArrowTokens [

	^ upArrowTokens
]

{ #category : #generated }
DartTypeNode >> upArrowTokens: anOrderedCollection [

	upArrowTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> varTokens [

	^ varTokens
]

{ #category : #generated }
DartTypeNode >> varTokens: anOrderedCollection [

	varTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> voidTokens [

	^ voidTokens
]

{ #category : #generated }
DartTypeNode >> voidTokens: anOrderedCollection [

	voidTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> whileTokens [

	^ whileTokens
]

{ #category : #generated }
DartTypeNode >> whileTokens: anOrderedCollection [

	whileTokens := anOrderedCollection
]

{ #category : #generated }
DartTypeNode >> yieldTokens [

	^ yieldTokens
]

{ #category : #generated }
DartTypeNode >> yieldTokens: anOrderedCollection [

	yieldTokens := anOrderedCollection
]
