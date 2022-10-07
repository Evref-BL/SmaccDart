Class {
	#name : #DartDeclarationNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'finalVarOrType',
		'identifierList',
		'varOrType',
		'functionType',
		'typeNotFunction',
		'identifier',
		'nextIdentifiers',
		'staticFinalDeclarationList',
		'initializedIdentifierList',
		'type',
		'finalToken',
		'constantConstructorSignature',
		'redirection',
		'initializers',
		'constructorSignature'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartDeclarationNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitDeclaration: self
]

{ #category : #generated }
DartDeclarationNode >> compositeNodeVariables [

	^ #( #nextIdentifiers )
]

{ #category : #generated }
DartDeclarationNode >> constantConstructorSignature [

	^ constantConstructorSignature
]

{ #category : #generated }
DartDeclarationNode >> constantConstructorSignature: aDartConstantConstructorSignatureNode [

	self constantConstructorSignature notNil ifTrue: [ 
		self constantConstructorSignature parent: nil ].
	constantConstructorSignature := aDartConstantConstructorSignatureNode.
	self constantConstructorSignature notNil ifTrue: [ 
		self constantConstructorSignature parent: self ]
]

{ #category : #generated }
DartDeclarationNode >> constructorSignature [

	^ constructorSignature
]

{ #category : #generated }
DartDeclarationNode >> constructorSignature: aDartConstructorSignatureNode [

	self constructorSignature notNil ifTrue: [ 
		self constructorSignature parent: nil ].
	constructorSignature := aDartConstructorSignatureNode.
	self constructorSignature notNil ifTrue: [ 
		self constructorSignature parent: self ]
]

{ #category : #generated }
DartDeclarationNode >> finalToken [

	^ finalToken
]

{ #category : #generated }
DartDeclarationNode >> finalToken: aSmaCCToken [

	finalToken := aSmaCCToken
]

{ #category : #generated }
DartDeclarationNode >> finalVarOrType [

	^ finalVarOrType
]

{ #category : #generated }
DartDeclarationNode >> finalVarOrType: anObject [

	self setParent: self finalVarOrType to: nil.
	finalVarOrType := anObject.
	self setParent: self finalVarOrType to: self
]

{ #category : #generated }
DartDeclarationNode >> functionType [

	^ functionType
]

{ #category : #generated }
DartDeclarationNode >> functionType: aDartProgramNode [

	self functionType notNil ifTrue: [ self functionType parent: nil ].
	functionType := aDartProgramNode.
	self functionType notNil ifTrue: [ self functionType parent: self ]
]

{ #category : #generated }
DartDeclarationNode >> identifier [

	^ identifier
]

{ #category : #generated }
DartDeclarationNode >> identifier: aDartIdentifierNode [

	self identifier notNil ifTrue: [ self identifier parent: nil ].
	identifier := aDartIdentifierNode.
	self identifier notNil ifTrue: [ self identifier parent: self ]
]

{ #category : #generated }
DartDeclarationNode >> identifierList [

	^ identifierList
]

{ #category : #generated }
DartDeclarationNode >> identifierList: aDartIdentifierListNode [

	self identifierList notNil ifTrue: [ self identifierList parent: nil ].
	identifierList := aDartIdentifierListNode.
	self identifierList notNil ifTrue: [ 
		self identifierList parent: self ]
]

{ #category : #'generated-initialize-release' }
DartDeclarationNode >> initialize [
	super initialize.
	nextIdentifiers := OrderedCollection new: 2.
]

{ #category : #generated }
DartDeclarationNode >> initializedIdentifierList [

	^ initializedIdentifierList
]

{ #category : #generated }
DartDeclarationNode >> initializedIdentifierList: aDartInitializedIdentifierListNode [

	self initializedIdentifierList notNil ifTrue: [ 
		self initializedIdentifierList parent: nil ].
	initializedIdentifierList := aDartInitializedIdentifierListNode.
	self initializedIdentifierList notNil ifTrue: [ 
		self initializedIdentifierList parent: self ]
]

{ #category : #generated }
DartDeclarationNode >> initializers [

	^ initializers
]

{ #category : #generated }
DartDeclarationNode >> initializers: aDartInitializersNode [

	self initializers notNil ifTrue: [ self initializers parent: nil ].
	initializers := aDartInitializersNode.
	self initializers notNil ifTrue: [ self initializers parent: self ]
]

{ #category : #generated }
DartDeclarationNode >> nextIdentifiers [

	^ nextIdentifiers
]

{ #category : #generated }
DartDeclarationNode >> nextIdentifiers: anOrderedCollection [

	self setParents: self nextIdentifiers to: nil.
	nextIdentifiers := anOrderedCollection.
	self setParents: self nextIdentifiers to: self
]

{ #category : #generated }
DartDeclarationNode >> nodeVariables [

	^ #( #identifierList #functionType #identifier #staticFinalDeclarationList
	     #initializedIdentifierList #type #constantConstructorSignature
	     #redirection #initializers #constructorSignature )
]

{ #category : #generated }
DartDeclarationNode >> otherVariables [

	^ #( #finalVarOrType #varOrType #typeNotFunction )
]

{ #category : #generated }
DartDeclarationNode >> redirection [

	^ redirection
]

{ #category : #generated }
DartDeclarationNode >> redirection: aDartRedirectionNode [

	self redirection notNil ifTrue: [ self redirection parent: nil ].
	redirection := aDartRedirectionNode.
	self redirection notNil ifTrue: [ self redirection parent: self ]
]

{ #category : #generated }
DartDeclarationNode >> staticFinalDeclarationList [

	^ staticFinalDeclarationList
]

{ #category : #generated }
DartDeclarationNode >> staticFinalDeclarationList: aDartStaticFinalDeclarationListNode [

	self staticFinalDeclarationList notNil ifTrue: [ 
		self staticFinalDeclarationList parent: nil ].
	staticFinalDeclarationList := aDartStaticFinalDeclarationListNode.
	self staticFinalDeclarationList notNil ifTrue: [ 
		self staticFinalDeclarationList parent: self ]
]

{ #category : #generated }
DartDeclarationNode >> tokenVariables [

	^ #( #finalToken )
]

{ #category : #generated }
DartDeclarationNode >> type [

	^ type
]

{ #category : #generated }
DartDeclarationNode >> type: aDartTypeNode [

	self type notNil ifTrue: [ self type parent: nil ].
	type := aDartTypeNode.
	self type notNil ifTrue: [ self type parent: self ]
]

{ #category : #generated }
DartDeclarationNode >> typeNotFunction [

	^ typeNotFunction
]

{ #category : #generated }
DartDeclarationNode >> typeNotFunction: anObject [

	self setParent: self typeNotFunction to: nil.
	typeNotFunction := anObject.
	self setParent: self typeNotFunction to: self
]

{ #category : #generated }
DartDeclarationNode >> varOrType [

	^ varOrType
]

{ #category : #generated }
DartDeclarationNode >> varOrType: anObject [

	self setParent: self varOrType to: nil.
	varOrType := anObject.
	self setParent: self varOrType to: self
]
