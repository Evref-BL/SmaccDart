Class {
	#name : #DartAsyncFunctionBodyNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'expression',
		'block'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartAsyncFunctionBodyNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitAsyncFunctionBody: self
]

{ #category : #generated }
DartAsyncFunctionBodyNode >> block [

	^ block
]

{ #category : #generated }
DartAsyncFunctionBodyNode >> block: aDartBlockNode [

	self block notNil ifTrue: [ self block parent: nil ].
	block := aDartBlockNode.
	self block notNil ifTrue: [ self block parent: self ]
]

{ #category : #generated }
DartAsyncFunctionBodyNode >> expression [

	^ expression
]

{ #category : #generated }
DartAsyncFunctionBodyNode >> expression: anOrderedCollection [

	self setParent: self expression to: nil.
	expression := anOrderedCollection.
	self setParent: self expression to: self
]

{ #category : #generated }
DartAsyncFunctionBodyNode >> nodeVariables [

	^ #( #block )
]

{ #category : #generated }
DartAsyncFunctionBodyNode >> otherVariables [

	^ #( #expression )
]
