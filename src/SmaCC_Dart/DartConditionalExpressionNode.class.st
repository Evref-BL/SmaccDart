Class {
	#name : #DartConditionalExpressionNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'testExpression',
		'trueExpression',
		'falseExpression'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartConditionalExpressionNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitConditionalExpression: self
]

{ #category : #generated }
DartConditionalExpressionNode >> falseExpression [

	^ falseExpression
]

{ #category : #generated }
DartConditionalExpressionNode >> falseExpression: aDartProgramNode [

	self falseExpression notNil ifTrue: [ 
		self falseExpression parent: nil ].
	falseExpression := aDartProgramNode.
	self falseExpression notNil ifTrue: [ 
		self falseExpression parent: self ]
]

{ #category : #generated }
DartConditionalExpressionNode >> nodeVariables [

	^ #( #testExpression #trueExpression #falseExpression )
]

{ #category : #generated }
DartConditionalExpressionNode >> testExpression [

	^ testExpression
]

{ #category : #generated }
DartConditionalExpressionNode >> testExpression: aDartProgramNode [

	self testExpression notNil ifTrue: [ self testExpression parent: nil ].
	testExpression := aDartProgramNode.
	self testExpression notNil ifTrue: [ 
		self testExpression parent: self ]
]

{ #category : #generated }
DartConditionalExpressionNode >> trueExpression [

	^ trueExpression
]

{ #category : #generated }
DartConditionalExpressionNode >> trueExpression: aDartProgramNode [

	self trueExpression notNil ifTrue: [ self trueExpression parent: nil ].
	trueExpression := aDartProgramNode.
	self trueExpression notNil ifTrue: [ 
		self trueExpression parent: self ]
]
