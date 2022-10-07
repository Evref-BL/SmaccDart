Class {
	#name : #DartStaticFinalDeclarationNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'identifier',
		'expression'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartStaticFinalDeclarationNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitStaticFinalDeclaration: self
]

{ #category : #generated }
DartStaticFinalDeclarationNode >> expression [

	^ expression
]

{ #category : #generated }
DartStaticFinalDeclarationNode >> expression: anOrderedCollection [

	self setParent: self expression to: nil.
	expression := anOrderedCollection.
	self setParent: self expression to: self
]

{ #category : #generated }
DartStaticFinalDeclarationNode >> identifier [

	^ identifier
]

{ #category : #generated }
DartStaticFinalDeclarationNode >> identifier: aDartIdentifierNode [

	self identifier notNil ifTrue: [ self identifier parent: nil ].
	identifier := aDartIdentifierNode.
	self identifier notNil ifTrue: [ self identifier parent: self ]
]

{ #category : #generated }
DartStaticFinalDeclarationNode >> nodeVariables [

	^ #( #identifier )
]

{ #category : #generated }
DartStaticFinalDeclarationNode >> otherVariables [

	^ #( #expression )
]
