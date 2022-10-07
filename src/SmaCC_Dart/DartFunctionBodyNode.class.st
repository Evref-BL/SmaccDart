Class {
	#name : #DartFunctionBodyNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'expression',
		'block'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartFunctionBodyNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitFunctionBody: self
]

{ #category : #generated }
DartFunctionBodyNode >> block [

	^ block
]

{ #category : #generated }
DartFunctionBodyNode >> block: aDartBlockNode [

	self block notNil ifTrue: [ self block parent: nil ].
	block := aDartBlockNode.
	self block notNil ifTrue: [ self block parent: self ]
]

{ #category : #generated }
DartFunctionBodyNode >> expression [

	^ expression
]

{ #category : #generated }
DartFunctionBodyNode >> expression: anOrderedCollection [

	self setParent: self expression to: nil.
	expression := anOrderedCollection.
	self setParent: self expression to: self
]

{ #category : #generated }
DartFunctionBodyNode >> nodeVariables [

	^ #( #block )
]

{ #category : #generated }
DartFunctionBodyNode >> otherVariables [

	^ #( #expression )
]
