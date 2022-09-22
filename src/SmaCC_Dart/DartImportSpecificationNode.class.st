Class {
	#name : #DartImportSpecificationNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'importTokens',
		'configurableUri',
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
		'typedefTokens',
		'semicolonToken',
		'identifier'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartImportSpecificationNode >> abstractTokens [

	^ abstractTokens
]

{ #category : #generated }
DartImportSpecificationNode >> abstractTokens: anOrderedCollection [

	abstractTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitImportSpecification: self
]

{ #category : #generated }
DartImportSpecificationNode >> asTokens [

	^ asTokens
]

{ #category : #generated }
DartImportSpecificationNode >> asTokens: anOrderedCollection [

	asTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> asyncTokens [

	^ asyncTokens
]

{ #category : #generated }
DartImportSpecificationNode >> asyncTokens: anOrderedCollection [

	asyncTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> awaitTokens [

	^ awaitTokens
]

{ #category : #generated }
DartImportSpecificationNode >> awaitTokens: anOrderedCollection [

	awaitTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> commaTokens [

	^ commaTokens
]

{ #category : #generated }
DartImportSpecificationNode >> commaTokens: anOrderedCollection [

	commaTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> compositeTokenVariables [

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
DartImportSpecificationNode >> configurableUri [

	^ configurableUri
]

{ #category : #generated }
DartImportSpecificationNode >> configurableUri: aDartConfigurableUriNode [

	self configurableUri notNil ifTrue: [ 
		self configurableUri parent: nil ].
	configurableUri := aDartConfigurableUriNode.
	self configurableUri notNil ifTrue: [ 
		self configurableUri parent: self ]
]

{ #category : #generated }
DartImportSpecificationNode >> covariantTokens [

	^ covariantTokens
]

{ #category : #generated }
DartImportSpecificationNode >> covariantTokens: anOrderedCollection [

	covariantTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> deferredTokens [

	^ deferredTokens
]

{ #category : #generated }
DartImportSpecificationNode >> deferredTokens: anOrderedCollection [

	deferredTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> dynamicTokens [

	^ dynamicTokens
]

{ #category : #generated }
DartImportSpecificationNode >> dynamicTokens: anOrderedCollection [

	dynamicTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> exportTokens [

	^ exportTokens
]

{ #category : #generated }
DartImportSpecificationNode >> exportTokens: anOrderedCollection [

	exportTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> extensionTokens [

	^ extensionTokens
]

{ #category : #generated }
DartImportSpecificationNode >> extensionTokens: anOrderedCollection [

	extensionTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> externalTokens [

	^ externalTokens
]

{ #category : #generated }
DartImportSpecificationNode >> externalTokens: anOrderedCollection [

	externalTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> factoryTokens [

	^ factoryTokens
]

{ #category : #generated }
DartImportSpecificationNode >> factoryTokens: anOrderedCollection [

	factoryTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> functionTokens [

	^ functionTokens
]

{ #category : #generated }
DartImportSpecificationNode >> functionTokens: anOrderedCollection [

	functionTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> getTokens [

	^ getTokens
]

{ #category : #generated }
DartImportSpecificationNode >> getTokens: anOrderedCollection [

	getTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> hideTokens [

	^ hideTokens
]

{ #category : #generated }
DartImportSpecificationNode >> hideTokens: anOrderedCollection [

	hideTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> identifier [

	^ identifier
]

{ #category : #generated }
DartImportSpecificationNode >> identifier: anOrderedCollection [

	self setParent: self identifier to: nil.
	identifier := anOrderedCollection.
	self setParent: self identifier to: self
]

{ #category : #generated }
DartImportSpecificationNode >> identifierTokens [

	^ identifierTokens
]

{ #category : #generated }
DartImportSpecificationNode >> identifierTokens: anOrderedCollection [

	identifierTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> implementsTokens [

	^ implementsTokens
]

{ #category : #generated }
DartImportSpecificationNode >> implementsTokens: anOrderedCollection [

	implementsTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> importTokens [

	^ importTokens
]

{ #category : #generated }
DartImportSpecificationNode >> importTokens: anOrderedCollection [

	importTokens := anOrderedCollection
]

{ #category : #'generated-initialize-release' }
DartImportSpecificationNode >> initialize [
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
DartImportSpecificationNode >> interfaceTokens [

	^ interfaceTokens
]

{ #category : #generated }
DartImportSpecificationNode >> interfaceTokens: anOrderedCollection [

	interfaceTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> lateTokens [

	^ lateTokens
]

{ #category : #generated }
DartImportSpecificationNode >> lateTokens: anOrderedCollection [

	lateTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> libraryTokens [

	^ libraryTokens
]

{ #category : #generated }
DartImportSpecificationNode >> libraryTokens: anOrderedCollection [

	libraryTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> mixinTokens [

	^ mixinTokens
]

{ #category : #generated }
DartImportSpecificationNode >> mixinTokens: anOrderedCollection [

	mixinTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> nodeVariables [

	^ #( #configurableUri )
]

{ #category : #generated }
DartImportSpecificationNode >> ofTokens [

	^ ofTokens
]

{ #category : #generated }
DartImportSpecificationNode >> ofTokens: anOrderedCollection [

	ofTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> onTokens [

	^ onTokens
]

{ #category : #generated }
DartImportSpecificationNode >> onTokens: anOrderedCollection [

	onTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> operatorTokens [

	^ operatorTokens
]

{ #category : #generated }
DartImportSpecificationNode >> operatorTokens: anOrderedCollection [

	operatorTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> otherVariables [

	^ #( #identifier )
]

{ #category : #generated }
DartImportSpecificationNode >> partTokens [

	^ partTokens
]

{ #category : #generated }
DartImportSpecificationNode >> partTokens: anOrderedCollection [

	partTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> requiredTokens [

	^ requiredTokens
]

{ #category : #generated }
DartImportSpecificationNode >> requiredTokens: anOrderedCollection [

	requiredTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> semicolonToken [

	^ semicolonToken
]

{ #category : #generated }
DartImportSpecificationNode >> semicolonToken: aSmaCCToken [

	semicolonToken := aSmaCCToken
]

{ #category : #generated }
DartImportSpecificationNode >> setTokens [

	^ setTokens
]

{ #category : #generated }
DartImportSpecificationNode >> setTokens: anOrderedCollection [

	setTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> showTokens [

	^ showTokens
]

{ #category : #generated }
DartImportSpecificationNode >> showTokens: anOrderedCollection [

	showTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> staticTokens [

	^ staticTokens
]

{ #category : #generated }
DartImportSpecificationNode >> staticTokens: anOrderedCollection [

	staticTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> syncTokens [

	^ syncTokens
]

{ #category : #generated }
DartImportSpecificationNode >> syncTokens: anOrderedCollection [

	syncTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> tokenVariables [

	^ #( #semicolonToken )
]

{ #category : #generated }
DartImportSpecificationNode >> typedefTokens [

	^ typedefTokens
]

{ #category : #generated }
DartImportSpecificationNode >> typedefTokens: anOrderedCollection [

	typedefTokens := anOrderedCollection
]

{ #category : #generated }
DartImportSpecificationNode >> yieldTokens [

	^ yieldTokens
]

{ #category : #generated }
DartImportSpecificationNode >> yieldTokens: anOrderedCollection [

	yieldTokens := anOrderedCollection
]
