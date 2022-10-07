Class {
	#name : #DartInitializedVariableDeclarationNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'declaredIdentifier',
		'initializedIdentifiers',
		'initializeExpression'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartInitializedVariableDeclarationNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitInitializedVariableDeclaration: self
]

{ #category : #generated }
DartInitializedVariableDeclarationNode >> compositeNodeVariables [

	^ #( #initializedIdentifiers )
]

{ #category : #generated }
DartInitializedVariableDeclarationNode >> declaredIdentifier [

	^ declaredIdentifier
]

{ #category : #generated }
DartInitializedVariableDeclarationNode >> declaredIdentifier: aDartDeclaredIdentifierNode [

	self declaredIdentifier notNil ifTrue: [ 
		self declaredIdentifier parent: nil ].
	declaredIdentifier := aDartDeclaredIdentifierNode.
	self declaredIdentifier notNil ifTrue: [ 
		self declaredIdentifier parent: self ]
]

{ #category : #'generated-initialize-release' }
DartInitializedVariableDeclarationNode >> initialize [
	super initialize.
	initializedIdentifiers := OrderedCollection new: 2.
]

{ #category : #generated }
DartInitializedVariableDeclarationNode >> initializeExpression [

	^ initializeExpression
]

{ #category : #generated }
DartInitializedVariableDeclarationNode >> initializeExpression: anOrderedCollection [

	self setParent: self initializeExpression to: nil.
	initializeExpression := anOrderedCollection.
	self setParent: self initializeExpression to: self
]

{ #category : #generated }
DartInitializedVariableDeclarationNode >> initializedIdentifiers [

	^ initializedIdentifiers
]

{ #category : #generated }
DartInitializedVariableDeclarationNode >> initializedIdentifiers: anOrderedCollection [

	self setParents: self initializedIdentifiers to: nil.
	initializedIdentifiers := anOrderedCollection.
	self setParents: self initializedIdentifiers to: self
]

{ #category : #generated }
DartInitializedVariableDeclarationNode >> nodeVariables [

	^ #( #declaredIdentifier )
]

{ #category : #generated }
DartInitializedVariableDeclarationNode >> otherVariables [

	^ #( #initializeExpression )
]
