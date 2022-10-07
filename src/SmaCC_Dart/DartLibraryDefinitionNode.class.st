Class {
	#name : #DartLibraryDefinitionNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'libraryImports',
		'libraryExports',
		'metadatas',
		'uris',
		'classDeclarations',
		'typeIdentifiers',
		'types',
		'typeParameterses',
		'identifierNotFUNCTIONs',
		'formalParameterParts',
		'identifiers',
		'formalParameterLists',
		'expressions',
		'blocks',
		'staticFinalDeclarations',
		'functionTypes',
		'typeNotFunctions',
		'initializedIdentifiers',
		'idNotFunctions',
		'functions',
		'assignableExpressions',
		'assignmentOperators',
		'assignedExpressions',
		'conditionalExpressions',
		'cascades',
		'throwExpressions',
		'typeNotVoidNotFunctions',
		'typeParameters',
		'argumentParts',
		'argumentses',
		'typeArgumentses',
		'functionPrefixes',
		'nextIdentifiers',
		'nextStaticFinalDeclarations',
		'nextInitializedIdentifiers',
		'methodSignatures',
		'functionBodies',
		'declarations',
		'nextTypeNotVoidNotFunctions',
		'nextTypeParameters',
		'dottedIdentifierList'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartLibraryDefinitionNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitLibraryDefinition: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> argumentParts [

	^ argumentParts
]

{ #category : #generated }
DartLibraryDefinitionNode >> argumentParts: anOrderedCollection [

	self setParent: self argumentParts to: nil.
	argumentParts := anOrderedCollection.
	self setParent: self argumentParts to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> argumentses [

	^ argumentses
]

{ #category : #generated }
DartLibraryDefinitionNode >> argumentses: anOrderedCollection [

	self setParent: self argumentses to: nil.
	argumentses := anOrderedCollection.
	self setParent: self argumentses to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> assignableExpressions [

	^ assignableExpressions
]

{ #category : #generated }
DartLibraryDefinitionNode >> assignableExpressions: anOrderedCollection [

	self setParents: self assignableExpressions to: nil.
	assignableExpressions := anOrderedCollection.
	self setParents: self assignableExpressions to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> assignedExpressions [

	^ assignedExpressions
]

{ #category : #generated }
DartLibraryDefinitionNode >> assignedExpressions: anOrderedCollection [

	self setParent: self assignedExpressions to: nil.
	assignedExpressions := anOrderedCollection.
	self setParent: self assignedExpressions to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> assignmentOperators [

	^ assignmentOperators
]

{ #category : #generated }
DartLibraryDefinitionNode >> assignmentOperators: anOrderedCollection [

	self setParents: self assignmentOperators to: nil.
	assignmentOperators := anOrderedCollection.
	self setParents: self assignmentOperators to: self
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
DartLibraryDefinitionNode >> cascades [

	^ cascades
]

{ #category : #generated }
DartLibraryDefinitionNode >> cascades: anOrderedCollection [

	self setParents: self cascades to: nil.
	cascades := anOrderedCollection.
	self setParents: self cascades to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> classDeclarations [

	^ classDeclarations
]

{ #category : #generated }
DartLibraryDefinitionNode >> classDeclarations: anOrderedCollection [

	self setParents: self classDeclarations to: nil.
	classDeclarations := anOrderedCollection.
	self setParents: self classDeclarations to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> compositeNodeVariables [

	^ #( #libraryImports #libraryExports #uris #classDeclarations
	     #types #typeParameterses #formalParameterParts
	     #identifiers #blocks #staticFinalDeclarations
	     #functionTypes #initializedIdentifiers #assignableExpressions
	     #assignmentOperators #conditionalExpressions
	     #cascades #throwExpressions #typeParameters #typeArgumentses
	     #functionPrefixes #nextIdentifiers #nextStaticFinalDeclarations
	     #nextInitializedIdentifiers #methodSignatures #functionBodies
	     #declarations #nextTypeParameters )
]

{ #category : #generated }
DartLibraryDefinitionNode >> compositeTokenVariables [

	^ #( #typeIdentifiers #identifierNotFUNCTIONs #idNotFunctions
	     #functions )
]

{ #category : #generated }
DartLibraryDefinitionNode >> conditionalExpressions [

	^ conditionalExpressions
]

{ #category : #generated }
DartLibraryDefinitionNode >> conditionalExpressions: anOrderedCollection [

	self setParents: self conditionalExpressions to: nil.
	conditionalExpressions := anOrderedCollection.
	self setParents: self conditionalExpressions to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> declarations [

	^ declarations
]

{ #category : #generated }
DartLibraryDefinitionNode >> declarations: anOrderedCollection [

	self setParents: self declarations to: nil.
	declarations := anOrderedCollection.
	self setParents: self declarations to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> dottedIdentifierList [

	^ dottedIdentifierList
]

{ #category : #generated }
DartLibraryDefinitionNode >> dottedIdentifierList: aDartDottedIdentifierListNode [

	self dottedIdentifierList notNil ifTrue: [ 
		self dottedIdentifierList parent: nil ].
	dottedIdentifierList := aDartDottedIdentifierListNode.
	self dottedIdentifierList notNil ifTrue: [ 
		self dottedIdentifierList parent: self ]
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
DartLibraryDefinitionNode >> formalParameterLists [

	^ formalParameterLists
]

{ #category : #generated }
DartLibraryDefinitionNode >> formalParameterLists: anOrderedCollection [

	self setParent: self formalParameterLists to: nil.
	formalParameterLists := anOrderedCollection.
	self setParent: self formalParameterLists to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> formalParameterParts [

	^ formalParameterParts
]

{ #category : #generated }
DartLibraryDefinitionNode >> formalParameterParts: anOrderedCollection [

	self setParents: self formalParameterParts to: nil.
	formalParameterParts := anOrderedCollection.
	self setParents: self formalParameterParts to: self
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
DartLibraryDefinitionNode >> functionPrefixes [

	^ functionPrefixes
]

{ #category : #generated }
DartLibraryDefinitionNode >> functionPrefixes: anOrderedCollection [

	self setParents: self functionPrefixes to: nil.
	functionPrefixes := anOrderedCollection.
	self setParents: self functionPrefixes to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> functionTypes [

	^ functionTypes
]

{ #category : #generated }
DartLibraryDefinitionNode >> functionTypes: anOrderedCollection [

	self setParents: self functionTypes to: nil.
	functionTypes := anOrderedCollection.
	self setParents: self functionTypes to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> functions [

	^ functions
]

{ #category : #generated }
DartLibraryDefinitionNode >> functions: anOrderedCollection [

	functions := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> idNotFunctions [

	^ idNotFunctions
]

{ #category : #generated }
DartLibraryDefinitionNode >> idNotFunctions: anOrderedCollection [

	idNotFunctions := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> identifierNotFUNCTIONs [

	^ identifierNotFUNCTIONs
]

{ #category : #generated }
DartLibraryDefinitionNode >> identifierNotFUNCTIONs: anOrderedCollection [

	identifierNotFUNCTIONs := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> identifiers [

	^ identifiers
]

{ #category : #generated }
DartLibraryDefinitionNode >> identifiers: anOrderedCollection [

	self setParents: self identifiers to: nil.
	identifiers := anOrderedCollection.
	self setParents: self identifiers to: self
]

{ #category : #'generated-initialize-release' }
DartLibraryDefinitionNode >> initialize [
	super initialize.
	libraryImports := OrderedCollection new: 2.
	libraryExports := OrderedCollection new: 2.
	metadatas := OrderedCollection new: 2.
	uris := OrderedCollection new: 2.
	classDeclarations := OrderedCollection new: 2.
	typeIdentifiers := OrderedCollection new: 2.
	types := OrderedCollection new: 2.
	typeParameterses := OrderedCollection new: 2.
	identifierNotFUNCTIONs := OrderedCollection new: 2.
	formalParameterParts := OrderedCollection new: 2.
	identifiers := OrderedCollection new: 2.
	formalParameterLists := OrderedCollection new: 2.
	expressions := OrderedCollection new: 2.
	blocks := OrderedCollection new: 2.
	staticFinalDeclarations := OrderedCollection new: 2.
	functionTypes := OrderedCollection new: 2.
	typeNotFunctions := OrderedCollection new: 2.
	initializedIdentifiers := OrderedCollection new: 2.
	idNotFunctions := OrderedCollection new: 2.
	functions := OrderedCollection new: 2.
	assignableExpressions := OrderedCollection new: 2.
	assignmentOperators := OrderedCollection new: 2.
	assignedExpressions := OrderedCollection new: 2.
	conditionalExpressions := OrderedCollection new: 2.
	cascades := OrderedCollection new: 2.
	throwExpressions := OrderedCollection new: 2.
	typeNotVoidNotFunctions := OrderedCollection new: 2.
	typeParameters := OrderedCollection new: 2.
	argumentParts := OrderedCollection new: 2.
	argumentses := OrderedCollection new: 2.
	typeArgumentses := OrderedCollection new: 2.
	functionPrefixes := OrderedCollection new: 2.
	nextIdentifiers := OrderedCollection new: 2.
	nextStaticFinalDeclarations := OrderedCollection new: 2.
	nextInitializedIdentifiers := OrderedCollection new: 2.
	methodSignatures := OrderedCollection new: 2.
	functionBodies := OrderedCollection new: 2.
	declarations := OrderedCollection new: 2.
	nextTypeNotVoidNotFunctions := OrderedCollection new: 2.
	nextTypeParameters := OrderedCollection new: 2.
]

{ #category : #generated }
DartLibraryDefinitionNode >> initializedIdentifiers [

	^ initializedIdentifiers
]

{ #category : #generated }
DartLibraryDefinitionNode >> initializedIdentifiers: anOrderedCollection [

	self setParents: self initializedIdentifiers to: nil.
	initializedIdentifiers := anOrderedCollection.
	self setParents: self initializedIdentifiers to: self
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
DartLibraryDefinitionNode >> metadatas [

	^ metadatas
]

{ #category : #generated }
DartLibraryDefinitionNode >> metadatas: anOrderedCollection [

	self setParent: self metadatas to: nil.
	metadatas := anOrderedCollection.
	self setParent: self metadatas to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> methodSignatures [

	^ methodSignatures
]

{ #category : #generated }
DartLibraryDefinitionNode >> methodSignatures: anOrderedCollection [

	self setParents: self methodSignatures to: nil.
	methodSignatures := anOrderedCollection.
	self setParents: self methodSignatures to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> nextIdentifiers [

	^ nextIdentifiers
]

{ #category : #generated }
DartLibraryDefinitionNode >> nextIdentifiers: anOrderedCollection [

	self setParents: self nextIdentifiers to: nil.
	nextIdentifiers := anOrderedCollection.
	self setParents: self nextIdentifiers to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> nextInitializedIdentifiers [

	^ nextInitializedIdentifiers
]

{ #category : #generated }
DartLibraryDefinitionNode >> nextInitializedIdentifiers: anOrderedCollection [

	self setParents: self nextInitializedIdentifiers to: nil.
	nextInitializedIdentifiers := anOrderedCollection.
	self setParents: self nextInitializedIdentifiers to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> nextStaticFinalDeclarations [

	^ nextStaticFinalDeclarations
]

{ #category : #generated }
DartLibraryDefinitionNode >> nextStaticFinalDeclarations: anOrderedCollection [

	self setParents: self nextStaticFinalDeclarations to: nil.
	nextStaticFinalDeclarations := anOrderedCollection.
	self setParents: self nextStaticFinalDeclarations to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> nextTypeNotVoidNotFunctions [

	^ nextTypeNotVoidNotFunctions
]

{ #category : #generated }
DartLibraryDefinitionNode >> nextTypeNotVoidNotFunctions: anOrderedCollection [

	self setParent: self nextTypeNotVoidNotFunctions to: nil.
	nextTypeNotVoidNotFunctions := anOrderedCollection.
	self setParent: self nextTypeNotVoidNotFunctions to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> nextTypeParameters [

	^ nextTypeParameters
]

{ #category : #generated }
DartLibraryDefinitionNode >> nextTypeParameters: anOrderedCollection [

	self setParents: self nextTypeParameters to: nil.
	nextTypeParameters := anOrderedCollection.
	self setParents: self nextTypeParameters to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> nodeVariables [

	^ #( #dottedIdentifierList )
]

{ #category : #generated }
DartLibraryDefinitionNode >> otherVariables [

	^ #( #metadatas #formalParameterLists #expressions #typeNotFunctions
	     #assignedExpressions #typeNotVoidNotFunctions
	     #argumentParts #argumentses #nextTypeNotVoidNotFunctions )
]

{ #category : #generated }
DartLibraryDefinitionNode >> staticFinalDeclarations [

	^ staticFinalDeclarations
]

{ #category : #generated }
DartLibraryDefinitionNode >> staticFinalDeclarations: anOrderedCollection [

	self setParents: self staticFinalDeclarations to: nil.
	staticFinalDeclarations := anOrderedCollection.
	self setParents: self staticFinalDeclarations to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> throwExpressions [

	^ throwExpressions
]

{ #category : #generated }
DartLibraryDefinitionNode >> throwExpressions: anOrderedCollection [

	self setParents: self throwExpressions to: nil.
	throwExpressions := anOrderedCollection.
	self setParents: self throwExpressions to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> typeArgumentses [

	^ typeArgumentses
]

{ #category : #generated }
DartLibraryDefinitionNode >> typeArgumentses: anOrderedCollection [

	self setParents: self typeArgumentses to: nil.
	typeArgumentses := anOrderedCollection.
	self setParents: self typeArgumentses to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> typeIdentifiers [

	^ typeIdentifiers
]

{ #category : #generated }
DartLibraryDefinitionNode >> typeIdentifiers: anOrderedCollection [

	typeIdentifiers := anOrderedCollection
]

{ #category : #generated }
DartLibraryDefinitionNode >> typeNotFunctions [

	^ typeNotFunctions
]

{ #category : #generated }
DartLibraryDefinitionNode >> typeNotFunctions: anOrderedCollection [

	self setParent: self typeNotFunctions to: nil.
	typeNotFunctions := anOrderedCollection.
	self setParent: self typeNotFunctions to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> typeNotVoidNotFunctions [

	^ typeNotVoidNotFunctions
]

{ #category : #generated }
DartLibraryDefinitionNode >> typeNotVoidNotFunctions: anOrderedCollection [

	self setParent: self typeNotVoidNotFunctions to: nil.
	typeNotVoidNotFunctions := anOrderedCollection.
	self setParent: self typeNotVoidNotFunctions to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> typeParameters [

	^ typeParameters
]

{ #category : #generated }
DartLibraryDefinitionNode >> typeParameters: anOrderedCollection [

	self setParents: self typeParameters to: nil.
	typeParameters := anOrderedCollection.
	self setParents: self typeParameters to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> typeParameterses [

	^ typeParameterses
]

{ #category : #generated }
DartLibraryDefinitionNode >> typeParameterses: anOrderedCollection [

	self setParents: self typeParameterses to: nil.
	typeParameterses := anOrderedCollection.
	self setParents: self typeParameterses to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> types [

	^ types
]

{ #category : #generated }
DartLibraryDefinitionNode >> types: anOrderedCollection [

	self setParents: self types to: nil.
	types := anOrderedCollection.
	self setParents: self types to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> uris [

	^ uris
]

{ #category : #generated }
DartLibraryDefinitionNode >> uris: anOrderedCollection [

	self setParents: self uris to: nil.
	uris := anOrderedCollection.
	self setParents: self uris to: self
]
