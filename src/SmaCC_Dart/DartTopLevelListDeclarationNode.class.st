Class {
	#name : #DartTopLevelListDeclarationNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'list',
		'type',
		'identifier',
		'initializedIdentifiers',
		'expression'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartTopLevelListDeclarationNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitTopLevelListDeclaration: self
]

{ #category : #generated }
DartTopLevelListDeclarationNode >> compositeNodeVariables [

	^ #( #initializedIdentifiers )
]

{ #category : #generated }
DartTopLevelListDeclarationNode >> expression [

	^ expression
]

{ #category : #generated }
DartTopLevelListDeclarationNode >> expression: anOrderedCollection [

	self setParent: self expression to: nil.
	expression := anOrderedCollection.
	self setParent: self expression to: self
]

{ #category : #generated }
DartTopLevelListDeclarationNode >> identifier [

	^ identifier
]

{ #category : #generated }
DartTopLevelListDeclarationNode >> identifier: aDartIdentifierNode [

	self identifier notNil ifTrue: [ self identifier parent: nil ].
	identifier := aDartIdentifierNode.
	self identifier notNil ifTrue: [ self identifier parent: self ]
]

{ #category : #'generated-initialize-release' }
DartTopLevelListDeclarationNode >> initialize [
	super initialize.
	initializedIdentifiers := OrderedCollection new: 2.
]

{ #category : #generated }
DartTopLevelListDeclarationNode >> initializedIdentifiers [

	^ initializedIdentifiers
]

{ #category : #generated }
DartTopLevelListDeclarationNode >> initializedIdentifiers: anOrderedCollection [

	self setParents: self initializedIdentifiers to: nil.
	initializedIdentifiers := anOrderedCollection.
	self setParents: self initializedIdentifiers to: self
]

{ #category : #generated }
DartTopLevelListDeclarationNode >> list [

	^ list
]

{ #category : #generated }
DartTopLevelListDeclarationNode >> list: aDartProgramNode [

	self list notNil ifTrue: [ self list parent: nil ].
	list := aDartProgramNode.
	self list notNil ifTrue: [ self list parent: self ]
]

{ #category : #generated }
DartTopLevelListDeclarationNode >> nodeVariables [

	^ #( #list #identifier )
]

{ #category : #generated }
DartTopLevelListDeclarationNode >> otherVariables [

	^ #( #type #expression )
]

{ #category : #generated }
DartTopLevelListDeclarationNode >> type [

	^ type
]

{ #category : #generated }
DartTopLevelListDeclarationNode >> type: anObject [

	self setParent: self type to: nil.
	type := anObject.
	self setParent: self type to: self
]
