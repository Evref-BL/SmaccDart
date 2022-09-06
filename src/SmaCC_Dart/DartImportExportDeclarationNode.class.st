Class {
	#name : #DartImportExportDeclarationNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'importTokens',
		'showTokens',
		'hideTokens',
		'identifierTokens',
		'asyncTokens',
		'ofTokens',
		'onTokens',
		'syncTokens',
		'awaitTokens',
		'yieldTokens',
		'functionTokens',
		'commaTokens',
		'abstractTokens',
		'asTokens',
		'covariantTokens',
		'deferredTokens',
		'dynamicTokens',
		'exportTokens',
		'extensionTokens',
		'externalTokens',
		'factoryTokens',
		'getTokens',
		'implementsTokens',
		'interfaceTokens',
		'lateTokens',
		'libraryTokens',
		'operatorTokens',
		'mixinTokens',
		'partTokens',
		'requiredTokens',
		'setTokens',
		'staticTokens',
		'typedefTokens'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartImportExportDeclarationNode >> abstractTokens [

	^ abstractTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> abstractTokens: anOrderedCollection [

	abstractTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitImportExportDeclaration: self
]

{ #category : #generated }
DartImportExportDeclarationNode >> asTokens [

	^ asTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> asTokens: anOrderedCollection [

	asTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> asyncTokens [

	^ asyncTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> asyncTokens: anOrderedCollection [

	asyncTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> awaitTokens [

	^ awaitTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> awaitTokens: anOrderedCollection [

	awaitTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> commaTokens [

	^ commaTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> commaTokens: anOrderedCollection [

	commaTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> compositeTokenVariables [

	^ #( #importTokens #showTokens #hideTokens #identifierTokens
	     #asyncTokens #ofTokens #onTokens #syncTokens #awaitTokens
	     #yieldTokens #functionTokens #commaTokens #abstractTokens
	     #asTokens #covariantTokens #deferredTokens #dynamicTokens
	     #exportTokens #extensionTokens #externalTokens #factoryTokens
	     #getTokens #implementsTokens #interfaceTokens #lateTokens
	     #libraryTokens #operatorTokens #mixinTokens #partTokens
	     #requiredTokens #setTokens #staticTokens #typedefTokens )
]

{ #category : #generated }
DartImportExportDeclarationNode >> covariantTokens [

	^ covariantTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> covariantTokens: anOrderedCollection [

	covariantTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> deferredTokens [

	^ deferredTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> deferredTokens: anOrderedCollection [

	deferredTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> dynamicTokens [

	^ dynamicTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> dynamicTokens: anOrderedCollection [

	dynamicTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> exportTokens [

	^ exportTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> exportTokens: anOrderedCollection [

	exportTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> extensionTokens [

	^ extensionTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> extensionTokens: anOrderedCollection [

	extensionTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> externalTokens [

	^ externalTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> externalTokens: anOrderedCollection [

	externalTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> factoryTokens [

	^ factoryTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> factoryTokens: anOrderedCollection [

	factoryTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> functionTokens [

	^ functionTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> functionTokens: anOrderedCollection [

	functionTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> getTokens [

	^ getTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> getTokens: anOrderedCollection [

	getTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> hideTokens [

	^ hideTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> hideTokens: anOrderedCollection [

	hideTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> identifierTokens [

	^ identifierTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> identifierTokens: anOrderedCollection [

	identifierTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> implementsTokens [

	^ implementsTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> implementsTokens: anOrderedCollection [

	implementsTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> importTokens [

	^ importTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> importTokens: anOrderedCollection [

	importTokens := anOrderedCollection
]

{ #category : #'generated-initialize-release' }
DartImportExportDeclarationNode >> initialize [
	super initialize.
	importTokens := OrderedCollection new: 2.
	showTokens := OrderedCollection new: 2.
	hideTokens := OrderedCollection new: 2.
	identifierTokens := OrderedCollection new: 2.
	asyncTokens := OrderedCollection new: 2.
	ofTokens := OrderedCollection new: 2.
	onTokens := OrderedCollection new: 2.
	syncTokens := OrderedCollection new: 2.
	awaitTokens := OrderedCollection new: 2.
	yieldTokens := OrderedCollection new: 2.
	functionTokens := OrderedCollection new: 2.
	commaTokens := OrderedCollection new: 2.
	abstractTokens := OrderedCollection new: 2.
	asTokens := OrderedCollection new: 2.
	covariantTokens := OrderedCollection new: 2.
	deferredTokens := OrderedCollection new: 2.
	dynamicTokens := OrderedCollection new: 2.
	exportTokens := OrderedCollection new: 2.
	extensionTokens := OrderedCollection new: 2.
	externalTokens := OrderedCollection new: 2.
	factoryTokens := OrderedCollection new: 2.
	getTokens := OrderedCollection new: 2.
	implementsTokens := OrderedCollection new: 2.
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
]

{ #category : #generated }
DartImportExportDeclarationNode >> interfaceTokens [

	^ interfaceTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> interfaceTokens: anOrderedCollection [

	interfaceTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> lateTokens [

	^ lateTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> lateTokens: anOrderedCollection [

	lateTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> libraryTokens [

	^ libraryTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> libraryTokens: anOrderedCollection [

	libraryTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> mixinTokens [

	^ mixinTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> mixinTokens: anOrderedCollection [

	mixinTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> ofTokens [

	^ ofTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> ofTokens: anOrderedCollection [

	ofTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> onTokens [

	^ onTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> onTokens: anOrderedCollection [

	onTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> operatorTokens [

	^ operatorTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> operatorTokens: anOrderedCollection [

	operatorTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> partTokens [

	^ partTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> partTokens: anOrderedCollection [

	partTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> requiredTokens [

	^ requiredTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> requiredTokens: anOrderedCollection [

	requiredTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> setTokens [

	^ setTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> setTokens: anOrderedCollection [

	setTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> showTokens [

	^ showTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> showTokens: anOrderedCollection [

	showTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> staticTokens [

	^ staticTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> staticTokens: anOrderedCollection [

	staticTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> syncTokens [

	^ syncTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> syncTokens: anOrderedCollection [

	syncTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> typedefTokens [

	^ typedefTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> typedefTokens: anOrderedCollection [

	typedefTokens := anOrderedCollection
]

{ #category : #generated }
DartImportExportDeclarationNode >> yieldTokens [

	^ yieldTokens
]

{ #category : #generated }
DartImportExportDeclarationNode >> yieldTokens: anOrderedCollection [

	yieldTokens := anOrderedCollection
]
