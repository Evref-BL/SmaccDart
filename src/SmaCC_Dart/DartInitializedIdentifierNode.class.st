Class {
	#name : #DartInitializedIdentifierNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'identifier',
		'expression'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartInitializedIdentifierNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitInitializedIdentifier: self
]

{ #category : #generated }
DartInitializedIdentifierNode >> expression [

	^ expression
]

{ #category : #generated }
DartInitializedIdentifierNode >> expression: anOrderedCollection [

	self setParent: self expression to: nil.
	expression := anOrderedCollection.
	self setParent: self expression to: self
]

{ #category : #generated }
DartInitializedIdentifierNode >> identifier [

	^ identifier
]

{ #category : #generated }
DartInitializedIdentifierNode >> identifier: aDartIdentifierNode [

	self identifier notNil ifTrue: [ self identifier parent: nil ].
	identifier := aDartIdentifierNode.
	self identifier notNil ifTrue: [ self identifier parent: self ]
]

{ #category : #generated }
DartInitializedIdentifierNode >> nodeVariables [

	^ #( #identifier )
]

{ #category : #generated }
DartInitializedIdentifierNode >> otherVariables [

	^ #( #expression )
]
