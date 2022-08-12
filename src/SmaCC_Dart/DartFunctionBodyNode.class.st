Class {
	#name : #DartFunctionBodyNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'equalGreaterThanToken',
		'expression',
		'semicolonToken',
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
DartFunctionBodyNode >> equalGreaterThanToken [

	^ equalGreaterThanToken
]

{ #category : #generated }
DartFunctionBodyNode >> equalGreaterThanToken: aSmaCCToken [

	equalGreaterThanToken := aSmaCCToken
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

{ #category : #generated }
DartFunctionBodyNode >> semicolonToken [

	^ semicolonToken
]

{ #category : #generated }
DartFunctionBodyNode >> semicolonToken: aSmaCCToken [

	semicolonToken := aSmaCCToken
]

{ #category : #generated }
DartFunctionBodyNode >> tokenVariables [

	^ #( #equalGreaterThanToken #semicolonToken )
]
