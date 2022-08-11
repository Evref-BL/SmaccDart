Class {
	#name : #DartLibraryDefinitionNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'classDeclarations',
		'enumTokens',
		'withTokens',
		'sCRIPT_TAGToken',
		'fEFFToken'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartLibraryDefinitionNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitLibraryDefinition: self
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
DartLibraryDefinitionNode >> compositeTokenVariables [

	^ #( #enumTokens #withTokens #lbraceTokens #equalGreaterThanTokens
	     #semicolonTokens #asyncTokens #timesTokens #syncTokens
	     #_Tokens #staticTokens #rbraceTokens #externalTokens
	     #finalTokens #covariantTokens #varTokens #abstractTokens
	     #constTokens #lateTokens #equalTokens #commaTokens #leftParenTokens
	     #rightParenTokens #colonTokens #factoryTokens #periodTokens
	     #newTokens #getTokens #setTokens #operatorTokens #identifierTokens
	     #hideTokens #ofTokens #onTokens #showTokens #awaitTokens
	     #yieldTokens #functionTokens #questionMarkTokens
	     #thisTokens #superTokens #lessThanTokens #extendsTokens
	     #greaterThanTokens #tildeTokens #leftBracketTokens
	     #rightBracketTokens #throwTokens #periodPeriodTokens
	     #questionMarkPeriodPeriodTokens #voidTokens #dynamicTokens
	     #assertTokens #asTokens #deferredTokens #exportTokens
	     #extensionTokens #implementsTokens #importTokens #interfaceTokens
	     #libraryTokens #mixinTokens #partTokens #requiredTokens
	     #typedefTokens #equalEqualTokens #bangTokens #questionMarkPeriodTokens
	     #timesEqualTokens #divideEqualTokens #tildeDivideEqualTokens
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
DartLibraryDefinitionNode >> enumTokens [

	^ enumTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> enumTokens: anOrderedCollection [

	enumTokens := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> fEFFToken [

	^ fEFFToken
]

{ #category : #generated }
DartLibraryDefinitionNode >> fEFFToken: aSmaCCToken [

	fEFFToken := aSmaCCToken
]

{ #category : #'generated-initialize-release' }
DartLibraryDefinitionNode >> initialize [
	super initialize.
	classDeclarations := OrderedCollection new: 2.
	enumTokens := OrderedCollection new: 2.
	withTokens := OrderedCollection new: 2.
]

{ #category : #generated }
DartLibraryDefinitionNode >> otherVariables [

	^ #( #classDeclarations )
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
DartLibraryDefinitionNode >> tokenVariables [

	^ #( #sCRIPT_TAGToken #fEFFToken )
]

{ #category : #generated }
DartLibraryDefinitionNode >> withTokens [

	^ withTokens
]

{ #category : #generated }
DartLibraryDefinitionNode >> withTokens: anOrderedCollection [

	withTokens := anOrderedCollection
]
