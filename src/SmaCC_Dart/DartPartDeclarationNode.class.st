Class {
	#name : #DartPartDeclarationNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'identifiers',
		'stringLiteralWithoutInterpolation',
		'classDeclarations',
		'identifierNotFUNCTIONs',
		'formalParameterParts',
		'types',
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
		'metadatas',
		'argumentParts',
		'argumentses',
		'typeArgumentses',
		'typeIdentifiers',
		'typeParameterses',
		'nextIdentifiers',
		'nextStaticFinalDeclarations',
		'nextInitializedIdentifiers',
		'methodSignatures',
		'functionBodies',
		'declarations',
		'nextTypeNotVoidNotFunctions',
		'nextTypeParameters',
		'functionPrefixes'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartPartDeclarationNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitPartDeclaration: self
]

{ #category : #generated }
DartPartDeclarationNode >> argumentParts [

	^ argumentParts
]

{ #category : #generated }
DartPartDeclarationNode >> argumentParts: anOrderedCollection [

	self setParent: self argumentParts to: nil.
	argumentParts := anOrderedCollection.
	self setParent: self argumentParts to: self
]

{ #category : #generated }
DartPartDeclarationNode >> argumentses [

	^ argumentses
]

{ #category : #generated }
DartPartDeclarationNode >> argumentses: anOrderedCollection [

	self setParent: self argumentses to: nil.
	argumentses := anOrderedCollection.
	self setParent: self argumentses to: self
]

{ #category : #generated }
DartPartDeclarationNode >> assignableExpressions [

	^ assignableExpressions
]

{ #category : #generated }
DartPartDeclarationNode >> assignableExpressions: anOrderedCollection [

	self setParents: self assignableExpressions to: nil.
	assignableExpressions := anOrderedCollection.
	self setParents: self assignableExpressions to: self
]

{ #category : #generated }
DartPartDeclarationNode >> assignedExpressions [

	^ assignedExpressions
]

{ #category : #generated }
DartPartDeclarationNode >> assignedExpressions: anOrderedCollection [

	self setParent: self assignedExpressions to: nil.
	assignedExpressions := anOrderedCollection.
	self setParent: self assignedExpressions to: self
]

{ #category : #generated }
DartPartDeclarationNode >> assignmentOperators [

	^ assignmentOperators
]

{ #category : #generated }
DartPartDeclarationNode >> assignmentOperators: anOrderedCollection [

	self setParents: self assignmentOperators to: nil.
	assignmentOperators := anOrderedCollection.
	self setParents: self assignmentOperators to: self
]

{ #category : #generated }
DartPartDeclarationNode >> blocks [

	^ blocks
]

{ #category : #generated }
DartPartDeclarationNode >> blocks: anOrderedCollection [

	self setParents: self blocks to: nil.
	blocks := anOrderedCollection.
	self setParents: self blocks to: self
]

{ #category : #generated }
DartPartDeclarationNode >> cascades [

	^ cascades
]

{ #category : #generated }
DartPartDeclarationNode >> cascades: anOrderedCollection [

	self setParents: self cascades to: nil.
	cascades := anOrderedCollection.
	self setParents: self cascades to: self
]

{ #category : #generated }
DartPartDeclarationNode >> classDeclarations [

	^ classDeclarations
]

{ #category : #generated }
DartPartDeclarationNode >> classDeclarations: anOrderedCollection [

	self setParents: self classDeclarations to: nil.
	classDeclarations := anOrderedCollection.
	self setParents: self classDeclarations to: self
]

{ #category : #generated }
DartPartDeclarationNode >> compositeNodeVariables [

	^ #( #identifiers #classDeclarations #formalParameterParts
	     #types #blocks #staticFinalDeclarations #functionTypes
	     #initializedIdentifiers #assignableExpressions
	     #assignmentOperators #conditionalExpressions
	     #cascades #throwExpressions #typeParameters #typeArgumentses
	     #typeParameterses #nextIdentifiers #nextStaticFinalDeclarations
	     #nextInitializedIdentifiers #methodSignatures #functionBodies
	     #declarations #nextTypeParameters #functionPrefixes )
]

{ #category : #generated }
DartPartDeclarationNode >> compositeTokenVariables [

	^ #( #identifierNotFUNCTIONs #idNotFunctions #functions #typeIdentifiers )
]

{ #category : #generated }
DartPartDeclarationNode >> conditionalExpressions [

	^ conditionalExpressions
]

{ #category : #generated }
DartPartDeclarationNode >> conditionalExpressions: anOrderedCollection [

	self setParents: self conditionalExpressions to: nil.
	conditionalExpressions := anOrderedCollection.
	self setParents: self conditionalExpressions to: self
]

{ #category : #generated }
DartPartDeclarationNode >> declarations [

	^ declarations
]

{ #category : #generated }
DartPartDeclarationNode >> declarations: anOrderedCollection [

	self setParents: self declarations to: nil.
	declarations := anOrderedCollection.
	self setParents: self declarations to: self
]

{ #category : #generated }
DartPartDeclarationNode >> expressions [

	^ expressions
]

{ #category : #generated }
DartPartDeclarationNode >> expressions: anOrderedCollection [

	self setParent: self expressions to: nil.
	expressions := anOrderedCollection.
	self setParent: self expressions to: self
]

{ #category : #generated }
DartPartDeclarationNode >> formalParameterLists [

	^ formalParameterLists
]

{ #category : #generated }
DartPartDeclarationNode >> formalParameterLists: anOrderedCollection [

	self setParent: self formalParameterLists to: nil.
	formalParameterLists := anOrderedCollection.
	self setParent: self formalParameterLists to: self
]

{ #category : #generated }
DartPartDeclarationNode >> formalParameterParts [

	^ formalParameterParts
]

{ #category : #generated }
DartPartDeclarationNode >> formalParameterParts: anOrderedCollection [

	self setParents: self formalParameterParts to: nil.
	formalParameterParts := anOrderedCollection.
	self setParents: self formalParameterParts to: self
]

{ #category : #generated }
DartPartDeclarationNode >> functionBodies [

	^ functionBodies
]

{ #category : #generated }
DartPartDeclarationNode >> functionBodies: anOrderedCollection [

	self setParents: self functionBodies to: nil.
	functionBodies := anOrderedCollection.
	self setParents: self functionBodies to: self
]

{ #category : #generated }
DartPartDeclarationNode >> functionPrefixes [

	^ functionPrefixes
]

{ #category : #generated }
DartPartDeclarationNode >> functionPrefixes: anOrderedCollection [

	self setParents: self functionPrefixes to: nil.
	functionPrefixes := anOrderedCollection.
	self setParents: self functionPrefixes to: self
]

{ #category : #generated }
DartPartDeclarationNode >> functionTypes [

	^ functionTypes
]

{ #category : #generated }
DartPartDeclarationNode >> functionTypes: anOrderedCollection [

	self setParents: self functionTypes to: nil.
	functionTypes := anOrderedCollection.
	self setParents: self functionTypes to: self
]

{ #category : #generated }
DartPartDeclarationNode >> functions [

	^ functions
]

{ #category : #generated }
DartPartDeclarationNode >> functions: anOrderedCollection [

	functions := anOrderedCollection
]

{ #category : #generated }
DartPartDeclarationNode >> idNotFunctions [

	^ idNotFunctions
]

{ #category : #generated }
DartPartDeclarationNode >> idNotFunctions: anOrderedCollection [

	idNotFunctions := anOrderedCollection
]

{ #category : #generated }
DartPartDeclarationNode >> identifierNotFUNCTIONs [

	^ identifierNotFUNCTIONs
]

{ #category : #generated }
DartPartDeclarationNode >> identifierNotFUNCTIONs: anOrderedCollection [

	identifierNotFUNCTIONs := anOrderedCollection
]

{ #category : #generated }
DartPartDeclarationNode >> identifiers [

	^ identifiers
]

{ #category : #generated }
DartPartDeclarationNode >> identifiers: anOrderedCollection [

	self setParents: self identifiers to: nil.
	identifiers := anOrderedCollection.
	self setParents: self identifiers to: self
]

{ #category : #'generated-initialize-release' }
DartPartDeclarationNode >> initialize [
	super initialize.
	identifiers := OrderedCollection new: 2.
	classDeclarations := OrderedCollection new: 2.
	identifierNotFUNCTIONs := OrderedCollection new: 2.
	formalParameterParts := OrderedCollection new: 2.
	types := OrderedCollection new: 2.
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
	metadatas := OrderedCollection new: 2.
	argumentParts := OrderedCollection new: 2.
	argumentses := OrderedCollection new: 2.
	typeArgumentses := OrderedCollection new: 2.
	typeIdentifiers := OrderedCollection new: 2.
	typeParameterses := OrderedCollection new: 2.
	nextIdentifiers := OrderedCollection new: 2.
	nextStaticFinalDeclarations := OrderedCollection new: 2.
	nextInitializedIdentifiers := OrderedCollection new: 2.
	methodSignatures := OrderedCollection new: 2.
	functionBodies := OrderedCollection new: 2.
	declarations := OrderedCollection new: 2.
	nextTypeNotVoidNotFunctions := OrderedCollection new: 2.
	nextTypeParameters := OrderedCollection new: 2.
	functionPrefixes := OrderedCollection new: 2.
]

{ #category : #generated }
DartPartDeclarationNode >> initializedIdentifiers [

	^ initializedIdentifiers
]

{ #category : #generated }
DartPartDeclarationNode >> initializedIdentifiers: anOrderedCollection [

	self setParents: self initializedIdentifiers to: nil.
	initializedIdentifiers := anOrderedCollection.
	self setParents: self initializedIdentifiers to: self
]

{ #category : #generated }
DartPartDeclarationNode >> metadatas [

	^ metadatas
]

{ #category : #generated }
DartPartDeclarationNode >> metadatas: anOrderedCollection [

	self setParent: self metadatas to: nil.
	metadatas := anOrderedCollection.
	self setParent: self metadatas to: self
]

{ #category : #generated }
DartPartDeclarationNode >> methodSignatures [

	^ methodSignatures
]

{ #category : #generated }
DartPartDeclarationNode >> methodSignatures: anOrderedCollection [

	self setParents: self methodSignatures to: nil.
	methodSignatures := anOrderedCollection.
	self setParents: self methodSignatures to: self
]

{ #category : #generated }
DartPartDeclarationNode >> nextIdentifiers [

	^ nextIdentifiers
]

{ #category : #generated }
DartPartDeclarationNode >> nextIdentifiers: anOrderedCollection [

	self setParents: self nextIdentifiers to: nil.
	nextIdentifiers := anOrderedCollection.
	self setParents: self nextIdentifiers to: self
]

{ #category : #generated }
DartPartDeclarationNode >> nextInitializedIdentifiers [

	^ nextInitializedIdentifiers
]

{ #category : #generated }
DartPartDeclarationNode >> nextInitializedIdentifiers: anOrderedCollection [

	self setParents: self nextInitializedIdentifiers to: nil.
	nextInitializedIdentifiers := anOrderedCollection.
	self setParents: self nextInitializedIdentifiers to: self
]

{ #category : #generated }
DartPartDeclarationNode >> nextStaticFinalDeclarations [

	^ nextStaticFinalDeclarations
]

{ #category : #generated }
DartPartDeclarationNode >> nextStaticFinalDeclarations: anOrderedCollection [

	self setParents: self nextStaticFinalDeclarations to: nil.
	nextStaticFinalDeclarations := anOrderedCollection.
	self setParents: self nextStaticFinalDeclarations to: self
]

{ #category : #generated }
DartPartDeclarationNode >> nextTypeNotVoidNotFunctions [

	^ nextTypeNotVoidNotFunctions
]

{ #category : #generated }
DartPartDeclarationNode >> nextTypeNotVoidNotFunctions: anOrderedCollection [

	self setParent: self nextTypeNotVoidNotFunctions to: nil.
	nextTypeNotVoidNotFunctions := anOrderedCollection.
	self setParent: self nextTypeNotVoidNotFunctions to: self
]

{ #category : #generated }
DartPartDeclarationNode >> nextTypeParameters [

	^ nextTypeParameters
]

{ #category : #generated }
DartPartDeclarationNode >> nextTypeParameters: anOrderedCollection [

	self setParents: self nextTypeParameters to: nil.
	nextTypeParameters := anOrderedCollection.
	self setParents: self nextTypeParameters to: self
]

{ #category : #generated }
DartPartDeclarationNode >> otherVariables [

	^ #( #formalParameterLists #expressions #typeNotFunctions
	     #assignedExpressions #typeNotVoidNotFunctions
	     #metadatas #argumentParts #argumentses #nextTypeNotVoidNotFunctions )
]

{ #category : #generated }
DartPartDeclarationNode >> staticFinalDeclarations [

	^ staticFinalDeclarations
]

{ #category : #generated }
DartPartDeclarationNode >> staticFinalDeclarations: anOrderedCollection [

	self setParents: self staticFinalDeclarations to: nil.
	staticFinalDeclarations := anOrderedCollection.
	self setParents: self staticFinalDeclarations to: self
]

{ #category : #generated }
DartPartDeclarationNode >> stringLiteralWithoutInterpolation [

	^ stringLiteralWithoutInterpolation
]

{ #category : #generated }
DartPartDeclarationNode >> stringLiteralWithoutInterpolation: aSmaCCToken [

	stringLiteralWithoutInterpolation := aSmaCCToken
]

{ #category : #generated }
DartPartDeclarationNode >> throwExpressions [

	^ throwExpressions
]

{ #category : #generated }
DartPartDeclarationNode >> throwExpressions: anOrderedCollection [

	self setParents: self throwExpressions to: nil.
	throwExpressions := anOrderedCollection.
	self setParents: self throwExpressions to: self
]

{ #category : #generated }
DartPartDeclarationNode >> tokenVariables [

	^ #( #stringLiteralWithoutInterpolation )
]

{ #category : #generated }
DartPartDeclarationNode >> typeArgumentses [

	^ typeArgumentses
]

{ #category : #generated }
DartPartDeclarationNode >> typeArgumentses: anOrderedCollection [

	self setParents: self typeArgumentses to: nil.
	typeArgumentses := anOrderedCollection.
	self setParents: self typeArgumentses to: self
]

{ #category : #generated }
DartPartDeclarationNode >> typeIdentifiers [

	^ typeIdentifiers
]

{ #category : #generated }
DartPartDeclarationNode >> typeIdentifiers: anOrderedCollection [

	typeIdentifiers := anOrderedCollection
]

{ #category : #generated }
DartPartDeclarationNode >> typeNotFunctions [

	^ typeNotFunctions
]

{ #category : #generated }
DartPartDeclarationNode >> typeNotFunctions: anOrderedCollection [

	self setParent: self typeNotFunctions to: nil.
	typeNotFunctions := anOrderedCollection.
	self setParent: self typeNotFunctions to: self
]

{ #category : #generated }
DartPartDeclarationNode >> typeNotVoidNotFunctions [

	^ typeNotVoidNotFunctions
]

{ #category : #generated }
DartPartDeclarationNode >> typeNotVoidNotFunctions: anOrderedCollection [

	self setParent: self typeNotVoidNotFunctions to: nil.
	typeNotVoidNotFunctions := anOrderedCollection.
	self setParent: self typeNotVoidNotFunctions to: self
]

{ #category : #generated }
DartPartDeclarationNode >> typeParameters [

	^ typeParameters
]

{ #category : #generated }
DartPartDeclarationNode >> typeParameters: anOrderedCollection [

	self setParents: self typeParameters to: nil.
	typeParameters := anOrderedCollection.
	self setParents: self typeParameters to: self
]

{ #category : #generated }
DartPartDeclarationNode >> typeParameterses [

	^ typeParameterses
]

{ #category : #generated }
DartPartDeclarationNode >> typeParameterses: anOrderedCollection [

	self setParents: self typeParameterses to: nil.
	typeParameterses := anOrderedCollection.
	self setParents: self typeParameterses to: self
]

{ #category : #generated }
DartPartDeclarationNode >> types [

	^ types
]

{ #category : #generated }
DartPartDeclarationNode >> types: anOrderedCollection [

	self setParents: self types to: nil.
	types := anOrderedCollection.
	self setParents: self types to: self
]
