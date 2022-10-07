Class {
	#name : #DartPostfixExpressionNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'assignableExpression',
		'postfixOperator',
		'primary',
		'selectors'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartPostfixExpressionNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitPostfixExpression: self
]

{ #category : #generated }
DartPostfixExpressionNode >> assignableExpression [

	^ assignableExpression
]

{ #category : #generated }
DartPostfixExpressionNode >> assignableExpression: aDartAssignableExpressionNode [

	self assignableExpression notNil ifTrue: [ 
		self assignableExpression parent: nil ].
	assignableExpression := aDartAssignableExpressionNode.
	self assignableExpression notNil ifTrue: [ 
		self assignableExpression parent: self ]
]

{ #category : #'generated-initialize-release' }
DartPostfixExpressionNode >> initialize [
	super initialize.
	selectors := OrderedCollection new: 2.
]

{ #category : #generated }
DartPostfixExpressionNode >> nodeVariables [

	^ #( #assignableExpression #primary )
]

{ #category : #generated }
DartPostfixExpressionNode >> otherVariables [

	^ #( #selectors )
]

{ #category : #generated }
DartPostfixExpressionNode >> postfixOperator [

	^ postfixOperator
]

{ #category : #generated }
DartPostfixExpressionNode >> postfixOperator: aSmaCCToken [

	postfixOperator := aSmaCCToken
]

{ #category : #generated }
DartPostfixExpressionNode >> primary [

	^ primary
]

{ #category : #generated }
DartPostfixExpressionNode >> primary: aDartProgramNode [

	self primary notNil ifTrue: [ self primary parent: nil ].
	primary := aDartProgramNode.
	self primary notNil ifTrue: [ self primary parent: self ]
]

{ #category : #generated }
DartPostfixExpressionNode >> selectors [

	^ selectors
]

{ #category : #generated }
DartPostfixExpressionNode >> selectors: anOrderedCollection [

	self setParent: self selectors to: nil.
	selectors := anOrderedCollection.
	self setParent: self selectors to: self
]

{ #category : #generated }
DartPostfixExpressionNode >> tokenVariables [

	^ #( #postfixOperator )
]
