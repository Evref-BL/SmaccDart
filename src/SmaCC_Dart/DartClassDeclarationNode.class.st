Class {
	#name : #DartClassDeclarationNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'clazzToken',
		'type',
		'lbraceTokens',
		'classMemberDefinitions',
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
		'minusMinusTokens',
		'withToken'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartClassDeclarationNode >> _Tokens [

	^ _Tokens
]

{ #category : #generated }
DartClassDeclarationNode >> _Tokens: anOrderedCollection [

	_Tokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> abstractTokens [

	^ abstractTokens
]

{ #category : #generated }
DartClassDeclarationNode >> abstractTokens: anOrderedCollection [

	abstractTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitClassDeclaration: self
]

{ #category : #generated }
DartClassDeclarationNode >> andAndTokens [

	^ andAndTokens
]

{ #category : #generated }
DartClassDeclarationNode >> andAndTokens: anOrderedCollection [

	andAndTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> andEqualTokens [

	^ andEqualTokens
]

{ #category : #generated }
DartClassDeclarationNode >> andEqualTokens: anOrderedCollection [

	andEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> andTokens [

	^ andTokens
]

{ #category : #generated }
DartClassDeclarationNode >> andTokens: anOrderedCollection [

	andTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> asTokens [

	^ asTokens
]

{ #category : #generated }
DartClassDeclarationNode >> asTokens: anOrderedCollection [

	asTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> assertTokens [

	^ assertTokens
]

{ #category : #generated }
DartClassDeclarationNode >> assertTokens: anOrderedCollection [

	assertTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> asyncTokens [

	^ asyncTokens
]

{ #category : #generated }
DartClassDeclarationNode >> asyncTokens: anOrderedCollection [

	asyncTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> awaitTokens [

	^ awaitTokens
]

{ #category : #generated }
DartClassDeclarationNode >> awaitTokens: anOrderedCollection [

	awaitTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> bangEqualTokens [

	^ bangEqualTokens
]

{ #category : #generated }
DartClassDeclarationNode >> bangEqualTokens: anOrderedCollection [

	bangEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> bangTokens [

	^ bangTokens
]

{ #category : #generated }
DartClassDeclarationNode >> bangTokens: anOrderedCollection [

	bangTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> blocks [

	^ blocks
]

{ #category : #generated }
DartClassDeclarationNode >> blocks: anOrderedCollection [

	self setParent: self blocks to: nil.
	blocks := anOrderedCollection.
	self setParent: self blocks to: self
]

{ #category : #generated }
DartClassDeclarationNode >> breakTokens [

	^ breakTokens
]

{ #category : #generated }
DartClassDeclarationNode >> breakTokens: anOrderedCollection [

	breakTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> caseTokens [

	^ caseTokens
]

{ #category : #generated }
DartClassDeclarationNode >> caseTokens: anOrderedCollection [

	caseTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> catchTokens [

	^ catchTokens
]

{ #category : #generated }
DartClassDeclarationNode >> catchTokens: anOrderedCollection [

	catchTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> classMemberDefinitions [

	^ classMemberDefinitions
]

{ #category : #generated }
DartClassDeclarationNode >> classMemberDefinitions: anOrderedCollection [

	self setParents: self classMemberDefinitions to: nil.
	classMemberDefinitions := anOrderedCollection.
	self setParents: self classMemberDefinitions to: self
]

{ #category : #generated }
DartClassDeclarationNode >> clazzToken [

	^ clazzToken
]

{ #category : #generated }
DartClassDeclarationNode >> clazzToken: aSmaCCToken [

	clazzToken := aSmaCCToken
]

{ #category : #generated }
DartClassDeclarationNode >> colonTokens [

	^ colonTokens
]

{ #category : #generated }
DartClassDeclarationNode >> colonTokens: anOrderedCollection [

	colonTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> commaTokens [

	^ commaTokens
]

{ #category : #generated }
DartClassDeclarationNode >> commaTokens: anOrderedCollection [

	commaTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> compositeNodeVariables [

	^ #( #classMemberDefinitions )
]

{ #category : #generated }
DartClassDeclarationNode >> compositeTokenVariables [

	^ #( #lbraceTokens #_Tokens #identifierTokens #dynamicTokens
	     #asyncTokens #hideTokens #ofTokens #onTokens #showTokens
	     #syncTokens #awaitTokens #yieldTokens #periodTokens
	     #functionTokens #newTokens #lessThanTokens #greaterThanTokens
	     #leftParenTokens #rightParenTokens #commaTokens #questionMarkTokens
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
	     #questionMarkQuestionMarkTokens #rbraceTokens #timesTokens
	     #nullTokens #trueTokens #falseTokens #numberTokens #hEX_NUMBERTokens
	     #tildeTokens #orOrTokens #equalEqualTokens #finalTokens
	     #andAndTokens #varTokens #rAW_MULTI_LINE_STRINGTokens
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
DartClassDeclarationNode >> constTokens [

	^ constTokens
]

{ #category : #generated }
DartClassDeclarationNode >> constTokens: anOrderedCollection [

	constTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> continueTokens [

	^ continueTokens
]

{ #category : #generated }
DartClassDeclarationNode >> continueTokens: anOrderedCollection [

	continueTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> covariantTokens [

	^ covariantTokens
]

{ #category : #generated }
DartClassDeclarationNode >> covariantTokens: anOrderedCollection [

	covariantTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> defaultTokens [

	^ defaultTokens
]

{ #category : #generated }
DartClassDeclarationNode >> defaultTokens: anOrderedCollection [

	defaultTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> deferredTokens [

	^ deferredTokens
]

{ #category : #generated }
DartClassDeclarationNode >> deferredTokens: anOrderedCollection [

	deferredTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> divideEqualTokens [

	^ divideEqualTokens
]

{ #category : #generated }
DartClassDeclarationNode >> divideEqualTokens: anOrderedCollection [

	divideEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> divideTokens [

	^ divideTokens
]

{ #category : #generated }
DartClassDeclarationNode >> divideTokens: anOrderedCollection [

	divideTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> doTokens [

	^ doTokens
]

{ #category : #generated }
DartClassDeclarationNode >> doTokens: anOrderedCollection [

	doTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> dynamicTokens [

	^ dynamicTokens
]

{ #category : #generated }
DartClassDeclarationNode >> dynamicTokens: anOrderedCollection [

	dynamicTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> elseTokens [

	^ elseTokens
]

{ #category : #generated }
DartClassDeclarationNode >> elseTokens: anOrderedCollection [

	elseTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> equalEqualTokens [

	^ equalEqualTokens
]

{ #category : #generated }
DartClassDeclarationNode >> equalEqualTokens: anOrderedCollection [

	equalEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> equalGreaterThanTokens [

	^ equalGreaterThanTokens
]

{ #category : #generated }
DartClassDeclarationNode >> equalGreaterThanTokens: anOrderedCollection [

	equalGreaterThanTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> equalTokens [

	^ equalTokens
]

{ #category : #generated }
DartClassDeclarationNode >> equalTokens: anOrderedCollection [

	equalTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> exportTokens [

	^ exportTokens
]

{ #category : #generated }
DartClassDeclarationNode >> exportTokens: anOrderedCollection [

	exportTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> expressions [

	^ expressions
]

{ #category : #generated }
DartClassDeclarationNode >> expressions: anOrderedCollection [

	self setParent: self expressions to: nil.
	expressions := anOrderedCollection.
	self setParent: self expressions to: self
]

{ #category : #generated }
DartClassDeclarationNode >> extendsTokens [

	^ extendsTokens
]

{ #category : #generated }
DartClassDeclarationNode >> extendsTokens: anOrderedCollection [

	extendsTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> extensionTokens [

	^ extensionTokens
]

{ #category : #generated }
DartClassDeclarationNode >> extensionTokens: anOrderedCollection [

	extensionTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> externalTokens [

	^ externalTokens
]

{ #category : #generated }
DartClassDeclarationNode >> externalTokens: anOrderedCollection [

	externalTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> factoryTokens [

	^ factoryTokens
]

{ #category : #generated }
DartClassDeclarationNode >> factoryTokens: anOrderedCollection [

	factoryTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> falseTokens [

	^ falseTokens
]

{ #category : #generated }
DartClassDeclarationNode >> falseTokens: anOrderedCollection [

	falseTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> finalTokens [

	^ finalTokens
]

{ #category : #generated }
DartClassDeclarationNode >> finalTokens: anOrderedCollection [

	finalTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> finallyTokens [

	^ finallyTokens
]

{ #category : #generated }
DartClassDeclarationNode >> finallyTokens: anOrderedCollection [

	finallyTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> forTokens [

	^ forTokens
]

{ #category : #generated }
DartClassDeclarationNode >> forTokens: anOrderedCollection [

	forTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> functionTokens [

	^ functionTokens
]

{ #category : #generated }
DartClassDeclarationNode >> functionTokens: anOrderedCollection [

	functionTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> getTokens [

	^ getTokens
]

{ #category : #generated }
DartClassDeclarationNode >> getTokens: anOrderedCollection [

	getTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> greaterThanTokens [

	^ greaterThanTokens
]

{ #category : #generated }
DartClassDeclarationNode >> greaterThanTokens: anOrderedCollection [

	greaterThanTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> hEX_NUMBERTokens [

	^ hEX_NUMBERTokens
]

{ #category : #generated }
DartClassDeclarationNode >> hEX_NUMBERTokens: anOrderedCollection [

	hEX_NUMBERTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> hideTokens [

	^ hideTokens
]

{ #category : #generated }
DartClassDeclarationNode >> hideTokens: anOrderedCollection [

	hideTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> identifierTokens [

	^ identifierTokens
]

{ #category : #generated }
DartClassDeclarationNode >> identifierTokens: anOrderedCollection [

	identifierTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> ifTokens [

	^ ifTokens
]

{ #category : #generated }
DartClassDeclarationNode >> ifTokens: anOrderedCollection [

	ifTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> implementsTokens [

	^ implementsTokens
]

{ #category : #generated }
DartClassDeclarationNode >> implementsTokens: anOrderedCollection [

	implementsTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> importTokens [

	^ importTokens
]

{ #category : #generated }
DartClassDeclarationNode >> importTokens: anOrderedCollection [

	importTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> inTokens [

	^ inTokens
]

{ #category : #generated }
DartClassDeclarationNode >> inTokens: anOrderedCollection [

	inTokens := anOrderedCollection
]

{ #category : #'generated-initialize-release' }
DartClassDeclarationNode >> initialize [
	super initialize.
	lbraceTokens := OrderedCollection new: 2.
	classMemberDefinitions := OrderedCollection new: 2.
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
DartClassDeclarationNode >> interfaceTokens [

	^ interfaceTokens
]

{ #category : #generated }
DartClassDeclarationNode >> interfaceTokens: anOrderedCollection [

	interfaceTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> isTokens [

	^ isTokens
]

{ #category : #generated }
DartClassDeclarationNode >> isTokens: anOrderedCollection [

	isTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> lateTokens [

	^ lateTokens
]

{ #category : #generated }
DartClassDeclarationNode >> lateTokens: anOrderedCollection [

	lateTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> lbraceTokens [

	^ lbraceTokens
]

{ #category : #generated }
DartClassDeclarationNode >> lbraceTokens: anOrderedCollection [

	lbraceTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> leftBracketTokens [

	^ leftBracketTokens
]

{ #category : #generated }
DartClassDeclarationNode >> leftBracketTokens: anOrderedCollection [

	leftBracketTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> leftParenTokens [

	^ leftParenTokens
]

{ #category : #generated }
DartClassDeclarationNode >> leftParenTokens: anOrderedCollection [

	leftParenTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> lessThanEqualTokens [

	^ lessThanEqualTokens
]

{ #category : #generated }
DartClassDeclarationNode >> lessThanEqualTokens: anOrderedCollection [

	lessThanEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> lessThanLessThanEqualTokens [

	^ lessThanLessThanEqualTokens
]

{ #category : #generated }
DartClassDeclarationNode >> lessThanLessThanEqualTokens: anOrderedCollection [

	lessThanLessThanEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> lessThanLessThanTokens [

	^ lessThanLessThanTokens
]

{ #category : #generated }
DartClassDeclarationNode >> lessThanLessThanTokens: anOrderedCollection [

	lessThanLessThanTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> lessThanTokens [

	^ lessThanTokens
]

{ #category : #generated }
DartClassDeclarationNode >> lessThanTokens: anOrderedCollection [

	lessThanTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> libraryTokens [

	^ libraryTokens
]

{ #category : #generated }
DartClassDeclarationNode >> libraryTokens: anOrderedCollection [

	libraryTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> mULTI_LINE_STRING_DQ_BEGIN_ENDTokens [

	^ mULTI_LINE_STRING_DQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartClassDeclarationNode >> mULTI_LINE_STRING_DQ_BEGIN_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> mULTI_LINE_STRING_DQ_BEGIN_MIDTokens [

	^ mULTI_LINE_STRING_DQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartClassDeclarationNode >> mULTI_LINE_STRING_DQ_BEGIN_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> mULTI_LINE_STRING_DQ_MID_ENDTokens [

	^ mULTI_LINE_STRING_DQ_MID_ENDTokens
]

{ #category : #generated }
DartClassDeclarationNode >> mULTI_LINE_STRING_DQ_MID_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> mULTI_LINE_STRING_DQ_MID_MIDTokens [

	^ mULTI_LINE_STRING_DQ_MID_MIDTokens
]

{ #category : #generated }
DartClassDeclarationNode >> mULTI_LINE_STRING_DQ_MID_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> mULTI_LINE_STRING_SQ_BEGIN_ENDTokens [

	^ mULTI_LINE_STRING_SQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartClassDeclarationNode >> mULTI_LINE_STRING_SQ_BEGIN_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> mULTI_LINE_STRING_SQ_BEGIN_MIDTokens [

	^ mULTI_LINE_STRING_SQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartClassDeclarationNode >> mULTI_LINE_STRING_SQ_BEGIN_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> mULTI_LINE_STRING_SQ_MID_ENDTokens [

	^ mULTI_LINE_STRING_SQ_MID_ENDTokens
]

{ #category : #generated }
DartClassDeclarationNode >> mULTI_LINE_STRING_SQ_MID_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> mULTI_LINE_STRING_SQ_MID_MIDTokens [

	^ mULTI_LINE_STRING_SQ_MID_MIDTokens
]

{ #category : #generated }
DartClassDeclarationNode >> mULTI_LINE_STRING_SQ_MID_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> minusEqualTokens [

	^ minusEqualTokens
]

{ #category : #generated }
DartClassDeclarationNode >> minusEqualTokens: anOrderedCollection [

	minusEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> minusMinusTokens [

	^ minusMinusTokens
]

{ #category : #generated }
DartClassDeclarationNode >> minusMinusTokens: anOrderedCollection [

	minusMinusTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> minusTokens [

	^ minusTokens
]

{ #category : #generated }
DartClassDeclarationNode >> minusTokens: anOrderedCollection [

	minusTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> mixinTokens [

	^ mixinTokens
]

{ #category : #generated }
DartClassDeclarationNode >> mixinTokens: anOrderedCollection [

	mixinTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> newTokens [

	^ newTokens
]

{ #category : #generated }
DartClassDeclarationNode >> newTokens: anOrderedCollection [

	newTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> nodeVariables [

	^ #( #type )
]

{ #category : #generated }
DartClassDeclarationNode >> nullTokens [

	^ nullTokens
]

{ #category : #generated }
DartClassDeclarationNode >> nullTokens: anOrderedCollection [

	nullTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> numberTokens [

	^ numberTokens
]

{ #category : #generated }
DartClassDeclarationNode >> numberTokens: anOrderedCollection [

	numberTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> ofTokens [

	^ ofTokens
]

{ #category : #generated }
DartClassDeclarationNode >> ofTokens: anOrderedCollection [

	ofTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> onTokens [

	^ onTokens
]

{ #category : #generated }
DartClassDeclarationNode >> onTokens: anOrderedCollection [

	onTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> operatorTokens [

	^ operatorTokens
]

{ #category : #generated }
DartClassDeclarationNode >> operatorTokens: anOrderedCollection [

	operatorTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> orEqualTokens [

	^ orEqualTokens
]

{ #category : #generated }
DartClassDeclarationNode >> orEqualTokens: anOrderedCollection [

	orEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> orOrTokens [

	^ orOrTokens
]

{ #category : #generated }
DartClassDeclarationNode >> orOrTokens: anOrderedCollection [

	orOrTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> orTokens [

	^ orTokens
]

{ #category : #generated }
DartClassDeclarationNode >> orTokens: anOrderedCollection [

	orTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> otherVariables [

	^ #( #expressions #blocks )
]

{ #category : #generated }
DartClassDeclarationNode >> partTokens [

	^ partTokens
]

{ #category : #generated }
DartClassDeclarationNode >> partTokens: anOrderedCollection [

	partTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> percentEqualTokens [

	^ percentEqualTokens
]

{ #category : #generated }
DartClassDeclarationNode >> percentEqualTokens: anOrderedCollection [

	percentEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> percentTokens [

	^ percentTokens
]

{ #category : #generated }
DartClassDeclarationNode >> percentTokens: anOrderedCollection [

	percentTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> periodPeriodPeriodQuestionMarkTokens [

	^ periodPeriodPeriodQuestionMarkTokens
]

{ #category : #generated }
DartClassDeclarationNode >> periodPeriodPeriodQuestionMarkTokens: anOrderedCollection [

	periodPeriodPeriodQuestionMarkTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> periodPeriodPeriodTokens [

	^ periodPeriodPeriodTokens
]

{ #category : #generated }
DartClassDeclarationNode >> periodPeriodPeriodTokens: anOrderedCollection [

	periodPeriodPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> periodPeriodTokens [

	^ periodPeriodTokens
]

{ #category : #generated }
DartClassDeclarationNode >> periodPeriodTokens: anOrderedCollection [

	periodPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> periodTokens [

	^ periodTokens
]

{ #category : #generated }
DartClassDeclarationNode >> periodTokens: anOrderedCollection [

	periodTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> plusEqualTokens [

	^ plusEqualTokens
]

{ #category : #generated }
DartClassDeclarationNode >> plusEqualTokens: anOrderedCollection [

	plusEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> plusPlusTokens [

	^ plusPlusTokens
]

{ #category : #generated }
DartClassDeclarationNode >> plusPlusTokens: anOrderedCollection [

	plusPlusTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> plusTokens [

	^ plusTokens
]

{ #category : #generated }
DartClassDeclarationNode >> plusTokens: anOrderedCollection [

	plusTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> questionMarkPeriodPeriodTokens [

	^ questionMarkPeriodPeriodTokens
]

{ #category : #generated }
DartClassDeclarationNode >> questionMarkPeriodPeriodTokens: anOrderedCollection [

	questionMarkPeriodPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> questionMarkPeriodTokens [

	^ questionMarkPeriodTokens
]

{ #category : #generated }
DartClassDeclarationNode >> questionMarkPeriodTokens: anOrderedCollection [

	questionMarkPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> questionMarkQuestionMarkEqualTokens [

	^ questionMarkQuestionMarkEqualTokens
]

{ #category : #generated }
DartClassDeclarationNode >> questionMarkQuestionMarkEqualTokens: anOrderedCollection [

	questionMarkQuestionMarkEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> questionMarkQuestionMarkTokens [

	^ questionMarkQuestionMarkTokens
]

{ #category : #generated }
DartClassDeclarationNode >> questionMarkQuestionMarkTokens: anOrderedCollection [

	questionMarkQuestionMarkTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> questionMarkTokens [

	^ questionMarkTokens
]

{ #category : #generated }
DartClassDeclarationNode >> questionMarkTokens: anOrderedCollection [

	questionMarkTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> rAW_MULTI_LINE_STRINGTokens [

	^ rAW_MULTI_LINE_STRINGTokens
]

{ #category : #generated }
DartClassDeclarationNode >> rAW_MULTI_LINE_STRINGTokens: anOrderedCollection [

	rAW_MULTI_LINE_STRINGTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> rAW_SINGLE_LINE_STRINGTokens [

	^ rAW_SINGLE_LINE_STRINGTokens
]

{ #category : #generated }
DartClassDeclarationNode >> rAW_SINGLE_LINE_STRINGTokens: anOrderedCollection [

	rAW_SINGLE_LINE_STRINGTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> rbraceTokens [

	^ rbraceTokens
]

{ #category : #generated }
DartClassDeclarationNode >> rbraceTokens: anOrderedCollection [

	rbraceTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> requiredTokens [

	^ requiredTokens
]

{ #category : #generated }
DartClassDeclarationNode >> requiredTokens: anOrderedCollection [

	requiredTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> rethrowTokens [

	^ rethrowTokens
]

{ #category : #generated }
DartClassDeclarationNode >> rethrowTokens: anOrderedCollection [

	rethrowTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> returnTokens [

	^ returnTokens
]

{ #category : #generated }
DartClassDeclarationNode >> returnTokens: anOrderedCollection [

	returnTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> rightBracketTokens [

	^ rightBracketTokens
]

{ #category : #generated }
DartClassDeclarationNode >> rightBracketTokens: anOrderedCollection [

	rightBracketTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> rightParenTokens [

	^ rightParenTokens
]

{ #category : #generated }
DartClassDeclarationNode >> rightParenTokens: anOrderedCollection [

	rightParenTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens [

	^ sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartClassDeclarationNode >> sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens [

	^ sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartClassDeclarationNode >> sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> sINGLE_LINE_STRING_DQ_MID_ENDTokens [

	^ sINGLE_LINE_STRING_DQ_MID_ENDTokens
]

{ #category : #generated }
DartClassDeclarationNode >> sINGLE_LINE_STRING_DQ_MID_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> sINGLE_LINE_STRING_DQ_MID_MIDTokens [

	^ sINGLE_LINE_STRING_DQ_MID_MIDTokens
]

{ #category : #generated }
DartClassDeclarationNode >> sINGLE_LINE_STRING_DQ_MID_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens [

	^ sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartClassDeclarationNode >> sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens [

	^ sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartClassDeclarationNode >> sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> sINGLE_LINE_STRING_SQ_MID_ENDTokens [

	^ sINGLE_LINE_STRING_SQ_MID_ENDTokens
]

{ #category : #generated }
DartClassDeclarationNode >> sINGLE_LINE_STRING_SQ_MID_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> sINGLE_LINE_STRING_SQ_MID_MIDTokens [

	^ sINGLE_LINE_STRING_SQ_MID_MIDTokens
]

{ #category : #generated }
DartClassDeclarationNode >> sINGLE_LINE_STRING_SQ_MID_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> semicolonTokens [

	^ semicolonTokens
]

{ #category : #generated }
DartClassDeclarationNode >> semicolonTokens: anOrderedCollection [

	semicolonTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> setTokens [

	^ setTokens
]

{ #category : #generated }
DartClassDeclarationNode >> setTokens: anOrderedCollection [

	setTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> showTokens [

	^ showTokens
]

{ #category : #generated }
DartClassDeclarationNode >> showTokens: anOrderedCollection [

	showTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> staticTokens [

	^ staticTokens
]

{ #category : #generated }
DartClassDeclarationNode >> staticTokens: anOrderedCollection [

	staticTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> superTokens [

	^ superTokens
]

{ #category : #generated }
DartClassDeclarationNode >> superTokens: anOrderedCollection [

	superTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> switchTokens [

	^ switchTokens
]

{ #category : #generated }
DartClassDeclarationNode >> switchTokens: anOrderedCollection [

	switchTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> syncTokens [

	^ syncTokens
]

{ #category : #generated }
DartClassDeclarationNode >> syncTokens: anOrderedCollection [

	syncTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> thisTokens [

	^ thisTokens
]

{ #category : #generated }
DartClassDeclarationNode >> thisTokens: anOrderedCollection [

	thisTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> throwTokens [

	^ throwTokens
]

{ #category : #generated }
DartClassDeclarationNode >> throwTokens: anOrderedCollection [

	throwTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> tildeDivideEqualTokens [

	^ tildeDivideEqualTokens
]

{ #category : #generated }
DartClassDeclarationNode >> tildeDivideEqualTokens: anOrderedCollection [

	tildeDivideEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> tildeDivideTokens [

	^ tildeDivideTokens
]

{ #category : #generated }
DartClassDeclarationNode >> tildeDivideTokens: anOrderedCollection [

	tildeDivideTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> tildeTokens [

	^ tildeTokens
]

{ #category : #generated }
DartClassDeclarationNode >> tildeTokens: anOrderedCollection [

	tildeTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> timesEqualTokens [

	^ timesEqualTokens
]

{ #category : #generated }
DartClassDeclarationNode >> timesEqualTokens: anOrderedCollection [

	timesEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> timesTokens [

	^ timesTokens
]

{ #category : #generated }
DartClassDeclarationNode >> timesTokens: anOrderedCollection [

	timesTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> tokenVariables [

	^ #( #clazzToken #withToken )
]

{ #category : #generated }
DartClassDeclarationNode >> trueTokens [

	^ trueTokens
]

{ #category : #generated }
DartClassDeclarationNode >> trueTokens: anOrderedCollection [

	trueTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> tryTokens [

	^ tryTokens
]

{ #category : #generated }
DartClassDeclarationNode >> tryTokens: anOrderedCollection [

	tryTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> type [

	^ type
]

{ #category : #generated }
DartClassDeclarationNode >> type: aDartTypeNode [

	self type notNil ifTrue: [ self type parent: nil ].
	type := aDartTypeNode.
	self type notNil ifTrue: [ self type parent: self ]
]

{ #category : #generated }
DartClassDeclarationNode >> typedefTokens [

	^ typedefTokens
]

{ #category : #generated }
DartClassDeclarationNode >> typedefTokens: anOrderedCollection [

	typedefTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> upArrowEqualTokens [

	^ upArrowEqualTokens
]

{ #category : #generated }
DartClassDeclarationNode >> upArrowEqualTokens: anOrderedCollection [

	upArrowEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> upArrowTokens [

	^ upArrowTokens
]

{ #category : #generated }
DartClassDeclarationNode >> upArrowTokens: anOrderedCollection [

	upArrowTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> varTokens [

	^ varTokens
]

{ #category : #generated }
DartClassDeclarationNode >> varTokens: anOrderedCollection [

	varTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> voidTokens [

	^ voidTokens
]

{ #category : #generated }
DartClassDeclarationNode >> voidTokens: anOrderedCollection [

	voidTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> whileTokens [

	^ whileTokens
]

{ #category : #generated }
DartClassDeclarationNode >> whileTokens: anOrderedCollection [

	whileTokens := anOrderedCollection
]

{ #category : #generated }
DartClassDeclarationNode >> withToken [

	^ withToken
]

{ #category : #generated }
DartClassDeclarationNode >> withToken: aSmaCCToken [

	withToken := aSmaCCToken
]

{ #category : #generated }
DartClassDeclarationNode >> yieldTokens [

	^ yieldTokens
]

{ #category : #generated }
DartClassDeclarationNode >> yieldTokens: anOrderedCollection [

	yieldTokens := anOrderedCollection
]
