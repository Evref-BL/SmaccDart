Class {
	#name : #DartLibraryDefinitionNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'libraryImports',
		'libraryExports',
		'partTokens',
		'semicolonTokens',
		'_Tokens',
		'stringLiteralWithoutInterpolations',
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
		'classDeclarations',
		'enumTokens',
		'withTokens',
		'expressions',
		'blocks',
		'methodSignatures',
		'functionBodies',
		'declarations',
		'isTokens',
		'plusPlusTokens',
		'minusMinusTokens',
		'sCRIPT_TAGToken',
		'fEFFToken'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartLibraryDefinitionNode >> _Tokens [

	^ _Tokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> _Tokens: anOrderedCollection [

	_Tokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> abstractTokens [

	^ abstractTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> abstractTokens: anOrderedCollection [

	abstractTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitLibraryDefinition: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> andAndTokens [

	^ andAndTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> andAndTokens: anOrderedCollection [

	andAndTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> andEqualTokens [

	^ andEqualTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> andEqualTokens: anOrderedCollection [

	andEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> andTokens [

	^ andTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> andTokens: anOrderedCollection [

	andTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> asTokens [

	^ asTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> asTokens: anOrderedCollection [

	asTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> asyncTokens [

	^ asyncTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> asyncTokens: anOrderedCollection [

	asyncTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> awaitTokens [

	^ awaitTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> awaitTokens: anOrderedCollection [

	awaitTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> bangEqualTokens [

	^ bangEqualTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> bangEqualTokens: anOrderedCollection [

	bangEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> bangTokens [

	^ bangTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> bangTokens: anOrderedCollection [

	bangTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> blocks [

	^ blocks
]

{ #category : #generated }
DartLibraryDefinitionNode >> blocks: anOrderedCollection [

	self setParents: self blocks to: nil.
	blocks := anOrderedCollection.
	self setParents: self blocks to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> classDeclarations [

	^ classDeclarations
]

{ #category : #generated }
DartLibraryDefinitionNode >> classDeclarations: anOrderedCollection [

	self setParent: self classDeclarations to: nil.
	classDeclarations := anOrderedCollection.
	self setParent: self classDeclarations to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> colonTokens [

	^ colonTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> colonTokens: anOrderedCollection [

	colonTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> commaTokens [

	^ commaTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> commaTokens: anOrderedCollection [

	commaTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> compositeNodeVariables [

	^ #( #libraryImports #libraryExports #blocks #functionBodies )
]

{ #category : #generated }
DartLibraryDefinitionNode >> compositeTokenVariables [

	^ #( #partTokens #semicolonTokens #_Tokens #stringLiteralWithoutInterpolations
	     #identifierTokens #dynamicTokens #asyncTokens #hideTokens
	     #ofTokens #onTokens #showTokens #syncTokens #awaitTokens
	     #yieldTokens #periodTokens #functionTokens #newTokens #lessThanTokens
	     #greaterThanTokens #leftParenTokens #rightParenTokens
	     #commaTokens #questionMarkTokens #voidTokens #abstractTokens
	     #asTokens #covariantTokens #deferredTokens #exportTokens
	     #extensionTokens #externalTokens #factoryTokens #getTokens
	     #implementsTokens #importTokens #interfaceTokens
	     #lateTokens #libraryTokens #operatorTokens #mixinTokens
	     #requiredTokens #setTokens #staticTokens #typedefTokens
	     #colonTokens #throwTokens #superTokens #equalTokens #periodPeriodTokens
	     #questionMarkPeriodPeriodTokens #extendsTokens #equalGreaterThanTokens
	     #leftBracketTokens #rightBracketTokens #thisTokens #constTokens
	     #bangTokens #questionMarkPeriodTokens #timesEqualTokens
	     #divideEqualTokens #tildeDivideEqualTokens
	     #percentEqualTokens #plusEqualTokens #minusEqualTokens
	     #lessThanLessThanEqualTokens #andEqualTokens #upArrowEqualTokens
	     #orEqualTokens #questionMarkQuestionMarkEqualTokens
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
	     #inTokens #enumTokens #withTokens #isTokens #plusPlusTokens
	     #minusMinusTokens )
]

{ #category : #generated }
DartLibraryDefinitionNode >> constTokens [

	^ constTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> constTokens: anOrderedCollection [

	constTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> covariantTokens [

	^ covariantTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> covariantTokens: anOrderedCollection [

	covariantTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> declarations [

	^ declarations
]

{ #category : #generated }
DartLibraryDefinitionNode >> declarations: anOrderedCollection [

	self setParent: self declarations to: nil.
	declarations := anOrderedCollection.
	self setParent: self declarations to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> deferredTokens [

	^ deferredTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> deferredTokens: anOrderedCollection [

	deferredTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> divideEqualTokens [

	^ divideEqualTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> divideEqualTokens: anOrderedCollection [

	divideEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> divideTokens [

	^ divideTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> divideTokens: anOrderedCollection [

	divideTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> dynamicTokens [

	^ dynamicTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> dynamicTokens: anOrderedCollection [

	dynamicTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> elseTokens [

	^ elseTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> elseTokens: anOrderedCollection [

	elseTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> enumTokens [

	^ enumTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> enumTokens: anOrderedCollection [

	enumTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> equalEqualTokens [

	^ equalEqualTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> equalEqualTokens: anOrderedCollection [

	equalEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> equalGreaterThanTokens [

	^ equalGreaterThanTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> equalGreaterThanTokens: anOrderedCollection [

	equalGreaterThanTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> equalTokens [

	^ equalTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> equalTokens: anOrderedCollection [

	equalTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> exportTokens [

	^ exportTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> exportTokens: anOrderedCollection [

	exportTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> expressions [

	^ expressions
]

{ #category : #generated }
DartLibraryDefinitionNode >> expressions: anOrderedCollection [

	self setParent: self expressions to: nil.
	expressions := anOrderedCollection.
	self setParent: self expressions to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> extendsTokens [

	^ extendsTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> extendsTokens: anOrderedCollection [

	extendsTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> extensionTokens [

	^ extensionTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> extensionTokens: anOrderedCollection [

	extensionTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> externalTokens [

	^ externalTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> externalTokens: anOrderedCollection [

	externalTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> fEFFToken [

	^ fEFFToken
]

{ #category : #generated }
DartLibraryDefinitionNode >> fEFFToken: aSmaCCToken [

	fEFFToken := aSmaCCToken
]

{ #category : #generated }
DartLibraryDefinitionNode >> factoryTokens [

	^ factoryTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> factoryTokens: anOrderedCollection [

	factoryTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> falseTokens [

	^ falseTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> falseTokens: anOrderedCollection [

	falseTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> finalTokens [

	^ finalTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> finalTokens: anOrderedCollection [

	finalTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> forTokens [

	^ forTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> forTokens: anOrderedCollection [

	forTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> functionBodies [

	^ functionBodies
]

{ #category : #generated }
DartLibraryDefinitionNode >> functionBodies: anOrderedCollection [

	self setParents: self functionBodies to: nil.
	functionBodies := anOrderedCollection.
	self setParents: self functionBodies to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> functionTokens [

	^ functionTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> functionTokens: anOrderedCollection [

	functionTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> getTokens [

	^ getTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> getTokens: anOrderedCollection [

	getTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> greaterThanTokens [

	^ greaterThanTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> greaterThanTokens: anOrderedCollection [

	greaterThanTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> hEX_NUMBERTokens [

	^ hEX_NUMBERTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> hEX_NUMBERTokens: anOrderedCollection [

	hEX_NUMBERTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> hideTokens [

	^ hideTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> hideTokens: anOrderedCollection [

	hideTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> identifierTokens [

	^ identifierTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> identifierTokens: anOrderedCollection [

	identifierTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> ifTokens [

	^ ifTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> ifTokens: anOrderedCollection [

	ifTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> implementsTokens [

	^ implementsTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> implementsTokens: anOrderedCollection [

	implementsTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> importTokens [

	^ importTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> importTokens: anOrderedCollection [

	importTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> inTokens [

	^ inTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> inTokens: anOrderedCollection [

	inTokens := anOrderedCollection
]

{ #category : #'generated-initialize-release' }
DartLibraryDefinitionNode >> initialize [
	super initialize.
	libraryImports := OrderedCollection new: 2.
	libraryExports := OrderedCollection new: 2.
	partTokens := OrderedCollection new: 2.
	semicolonTokens := OrderedCollection new: 2.
	_Tokens := OrderedCollection new: 2.
	stringLiteralWithoutInterpolations := OrderedCollection new: 2.
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
	classDeclarations := OrderedCollection new: 2.
	enumTokens := OrderedCollection new: 2.
	withTokens := OrderedCollection new: 2.
	expressions := OrderedCollection new: 2.
	blocks := OrderedCollection new: 2.
	methodSignatures := OrderedCollection new: 2.
	functionBodies := OrderedCollection new: 2.
	declarations := OrderedCollection new: 2.
	isTokens := OrderedCollection new: 2.
	plusPlusTokens := OrderedCollection new: 2.
	minusMinusTokens := OrderedCollection new: 2.
]

{ #category : #generated }
DartLibraryDefinitionNode >> interfaceTokens [

	^ interfaceTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> interfaceTokens: anOrderedCollection [

	interfaceTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> isTokens [

	^ isTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> isTokens: anOrderedCollection [

	isTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> lateTokens [

	^ lateTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> lateTokens: anOrderedCollection [

	lateTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> lbraceTokens [

	^ lbraceTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> lbraceTokens: anOrderedCollection [

	lbraceTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> leftBracketTokens [

	^ leftBracketTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> leftBracketTokens: anOrderedCollection [

	leftBracketTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> leftParenTokens [

	^ leftParenTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> leftParenTokens: anOrderedCollection [

	leftParenTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> lessThanEqualTokens [

	^ lessThanEqualTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> lessThanEqualTokens: anOrderedCollection [

	lessThanEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> lessThanLessThanEqualTokens [

	^ lessThanLessThanEqualTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> lessThanLessThanEqualTokens: anOrderedCollection [

	lessThanLessThanEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> lessThanLessThanTokens [

	^ lessThanLessThanTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> lessThanLessThanTokens: anOrderedCollection [

	lessThanLessThanTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> lessThanTokens [

	^ lessThanTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> lessThanTokens: anOrderedCollection [

	lessThanTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> libraryExports [

	^ libraryExports
]

{ #category : #generated }
DartLibraryDefinitionNode >> libraryExports: anOrderedCollection [

	self setParents: self libraryExports to: nil.
	libraryExports := anOrderedCollection.
	self setParents: self libraryExports to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> libraryImports [

	^ libraryImports
]

{ #category : #generated }
DartLibraryDefinitionNode >> libraryImports: anOrderedCollection [

	self setParents: self libraryImports to: nil.
	libraryImports := anOrderedCollection.
	self setParents: self libraryImports to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> libraryTokens [

	^ libraryTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> libraryTokens: anOrderedCollection [

	libraryTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> mULTI_LINE_STRING_DQ_BEGIN_ENDTokens [

	^ mULTI_LINE_STRING_DQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> mULTI_LINE_STRING_DQ_BEGIN_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> mULTI_LINE_STRING_DQ_BEGIN_MIDTokens [

	^ mULTI_LINE_STRING_DQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> mULTI_LINE_STRING_DQ_BEGIN_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> mULTI_LINE_STRING_DQ_MID_ENDTokens [

	^ mULTI_LINE_STRING_DQ_MID_ENDTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> mULTI_LINE_STRING_DQ_MID_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> mULTI_LINE_STRING_DQ_MID_MIDTokens [

	^ mULTI_LINE_STRING_DQ_MID_MIDTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> mULTI_LINE_STRING_DQ_MID_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_DQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> mULTI_LINE_STRING_SQ_BEGIN_ENDTokens [

	^ mULTI_LINE_STRING_SQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> mULTI_LINE_STRING_SQ_BEGIN_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> mULTI_LINE_STRING_SQ_BEGIN_MIDTokens [

	^ mULTI_LINE_STRING_SQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> mULTI_LINE_STRING_SQ_BEGIN_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> mULTI_LINE_STRING_SQ_MID_ENDTokens [

	^ mULTI_LINE_STRING_SQ_MID_ENDTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> mULTI_LINE_STRING_SQ_MID_ENDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> mULTI_LINE_STRING_SQ_MID_MIDTokens [

	^ mULTI_LINE_STRING_SQ_MID_MIDTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> mULTI_LINE_STRING_SQ_MID_MIDTokens: anOrderedCollection [

	mULTI_LINE_STRING_SQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> methodSignatures [

	^ methodSignatures
]

{ #category : #generated }
DartLibraryDefinitionNode >> methodSignatures: anOrderedCollection [

	self setParent: self methodSignatures to: nil.
	methodSignatures := anOrderedCollection.
	self setParent: self methodSignatures to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> minusEqualTokens [

	^ minusEqualTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> minusEqualTokens: anOrderedCollection [

	minusEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> minusMinusTokens [

	^ minusMinusTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> minusMinusTokens: anOrderedCollection [

	minusMinusTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> minusTokens [

	^ minusTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> minusTokens: anOrderedCollection [

	minusTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> mixinTokens [

	^ mixinTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> mixinTokens: anOrderedCollection [

	mixinTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> newTokens [

	^ newTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> newTokens: anOrderedCollection [

	newTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> nullTokens [

	^ nullTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> nullTokens: anOrderedCollection [

	nullTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> numberTokens [

	^ numberTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> numberTokens: anOrderedCollection [

	numberTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> ofTokens [

	^ ofTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> ofTokens: anOrderedCollection [

	ofTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> onTokens [

	^ onTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> onTokens: anOrderedCollection [

	onTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> operatorTokens [

	^ operatorTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> operatorTokens: anOrderedCollection [

	operatorTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> orEqualTokens [

	^ orEqualTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> orEqualTokens: anOrderedCollection [

	orEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> orOrTokens [

	^ orOrTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> orOrTokens: anOrderedCollection [

	orOrTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> orTokens [

	^ orTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> orTokens: anOrderedCollection [

	orTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> otherVariables [

	^ #( #statementses #classDeclarations #expressions #methodSignatures
	     #declarations )
]

{ #category : #generated }
DartLibraryDefinitionNode >> partTokens [

	^ partTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> partTokens: anOrderedCollection [

	partTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> percentEqualTokens [

	^ percentEqualTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> percentEqualTokens: anOrderedCollection [

	percentEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> percentTokens [

	^ percentTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> percentTokens: anOrderedCollection [

	percentTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> periodPeriodPeriodQuestionMarkTokens [

	^ periodPeriodPeriodQuestionMarkTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> periodPeriodPeriodQuestionMarkTokens: anOrderedCollection [

	periodPeriodPeriodQuestionMarkTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> periodPeriodPeriodTokens [

	^ periodPeriodPeriodTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> periodPeriodPeriodTokens: anOrderedCollection [

	periodPeriodPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> periodPeriodTokens [

	^ periodPeriodTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> periodPeriodTokens: anOrderedCollection [

	periodPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> periodTokens [

	^ periodTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> periodTokens: anOrderedCollection [

	periodTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> plusEqualTokens [

	^ plusEqualTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> plusEqualTokens: anOrderedCollection [

	plusEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> plusPlusTokens [

	^ plusPlusTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> plusPlusTokens: anOrderedCollection [

	plusPlusTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> plusTokens [

	^ plusTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> plusTokens: anOrderedCollection [

	plusTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> questionMarkPeriodPeriodTokens [

	^ questionMarkPeriodPeriodTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> questionMarkPeriodPeriodTokens: anOrderedCollection [

	questionMarkPeriodPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> questionMarkPeriodTokens [

	^ questionMarkPeriodTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> questionMarkPeriodTokens: anOrderedCollection [

	questionMarkPeriodTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> questionMarkQuestionMarkEqualTokens [

	^ questionMarkQuestionMarkEqualTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> questionMarkQuestionMarkEqualTokens: anOrderedCollection [

	questionMarkQuestionMarkEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> questionMarkQuestionMarkTokens [

	^ questionMarkQuestionMarkTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> questionMarkQuestionMarkTokens: anOrderedCollection [

	questionMarkQuestionMarkTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> questionMarkTokens [

	^ questionMarkTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> questionMarkTokens: anOrderedCollection [

	questionMarkTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> rAW_MULTI_LINE_STRINGTokens [

	^ rAW_MULTI_LINE_STRINGTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> rAW_MULTI_LINE_STRINGTokens: anOrderedCollection [

	rAW_MULTI_LINE_STRINGTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> rAW_SINGLE_LINE_STRINGTokens [

	^ rAW_SINGLE_LINE_STRINGTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> rAW_SINGLE_LINE_STRINGTokens: anOrderedCollection [

	rAW_SINGLE_LINE_STRINGTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> rbraceTokens [

	^ rbraceTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> rbraceTokens: anOrderedCollection [

	rbraceTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> requiredTokens [

	^ requiredTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> requiredTokens: anOrderedCollection [

	requiredTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> rightBracketTokens [

	^ rightBracketTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> rightBracketTokens: anOrderedCollection [

	rightBracketTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> rightParenTokens [

	^ rightParenTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> rightParenTokens: anOrderedCollection [

	rightParenTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> sCRIPT_TAGToken [

	^ sCRIPT_TAGToken
]

{ #category : #generated }
DartLibraryDefinitionNode >> sCRIPT_TAGToken: aSmaCCToken [

	sCRIPT_TAGToken := aSmaCCToken
]

{ #category : #generated }
DartLibraryDefinitionNode >> sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens [

	^ sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens [

	^ sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> sINGLE_LINE_STRING_DQ_MID_ENDTokens [

	^ sINGLE_LINE_STRING_DQ_MID_ENDTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> sINGLE_LINE_STRING_DQ_MID_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> sINGLE_LINE_STRING_DQ_MID_MIDTokens [

	^ sINGLE_LINE_STRING_DQ_MID_MIDTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> sINGLE_LINE_STRING_DQ_MID_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_DQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens [

	^ sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_BEGIN_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens [

	^ sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_BEGIN_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> sINGLE_LINE_STRING_SQ_MID_ENDTokens [

	^ sINGLE_LINE_STRING_SQ_MID_ENDTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> sINGLE_LINE_STRING_SQ_MID_ENDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_MID_ENDTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> sINGLE_LINE_STRING_SQ_MID_MIDTokens [

	^ sINGLE_LINE_STRING_SQ_MID_MIDTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> sINGLE_LINE_STRING_SQ_MID_MIDTokens: anOrderedCollection [

	sINGLE_LINE_STRING_SQ_MID_MIDTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> semicolonTokens [

	^ semicolonTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> semicolonTokens: anOrderedCollection [

	semicolonTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> setTokens [

	^ setTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> setTokens: anOrderedCollection [

	setTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> showTokens [

	^ showTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> showTokens: anOrderedCollection [

	showTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> statementses [

	^ statementses
]

{ #category : #generated }
DartLibraryDefinitionNode >> statementses: anOrderedCollection [

	self setParent: self statementses to: nil.
	statementses := anOrderedCollection.
	self setParent: self statementses to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> staticTokens [

	^ staticTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> staticTokens: anOrderedCollection [

	staticTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> stringLiteralWithoutInterpolations [

	^ stringLiteralWithoutInterpolations
]

{ #category : #generated }
DartLibraryDefinitionNode >> stringLiteralWithoutInterpolations: anOrderedCollection [

	stringLiteralWithoutInterpolations := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> superTokens [

	^ superTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> superTokens: anOrderedCollection [

	superTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> syncTokens [

	^ syncTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> syncTokens: anOrderedCollection [

	syncTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> thisTokens [

	^ thisTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> thisTokens: anOrderedCollection [

	thisTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> throwTokens [

	^ throwTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> throwTokens: anOrderedCollection [

	throwTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> tildeDivideEqualTokens [

	^ tildeDivideEqualTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> tildeDivideEqualTokens: anOrderedCollection [

	tildeDivideEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> tildeDivideTokens [

	^ tildeDivideTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> tildeDivideTokens: anOrderedCollection [

	tildeDivideTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> tildeTokens [

	^ tildeTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> tildeTokens: anOrderedCollection [

	tildeTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> timesEqualTokens [

	^ timesEqualTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> timesEqualTokens: anOrderedCollection [

	timesEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> timesTokens [

	^ timesTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> timesTokens: anOrderedCollection [

	timesTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> tokenVariables [

	^ #( #sCRIPT_TAGToken #fEFFToken )
]

{ #category : #generated }
DartLibraryDefinitionNode >> trueTokens [

	^ trueTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> trueTokens: anOrderedCollection [

	trueTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> typedefTokens [

	^ typedefTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> typedefTokens: anOrderedCollection [

	typedefTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> upArrowEqualTokens [

	^ upArrowEqualTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> upArrowEqualTokens: anOrderedCollection [

	upArrowEqualTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> upArrowTokens [

	^ upArrowTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> upArrowTokens: anOrderedCollection [

	upArrowTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> varTokens [

	^ varTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> varTokens: anOrderedCollection [

	varTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> voidTokens [

	^ voidTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> voidTokens: anOrderedCollection [

	voidTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> withTokens [

	^ withTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> withTokens: anOrderedCollection [

	withTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> yieldTokens [

	^ yieldTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> yieldTokens: anOrderedCollection [

	yieldTokens := anOrderedCollection
]
