Class {
	#name : #DartIfNullExpressionNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'testNullExpression',
		'logicalOrExpression'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartIfNullExpressionNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitIfNullExpression: self
]

{ #category : #generated }
DartIfNullExpressionNode >> logicalOrExpression [

	^ logicalOrExpression
]

{ #category : #generated }
DartIfNullExpressionNode >> logicalOrExpression: aDartProgramNode [

	self logicalOrExpression notNil ifTrue: [ 
		self logicalOrExpression parent: nil ].
	logicalOrExpression := aDartProgramNode.
	self logicalOrExpression notNil ifTrue: [ 
		self logicalOrExpression parent: self ]
]

{ #category : #generated }
DartIfNullExpressionNode >> nodeVariables [

	^ #( #testNullExpression #logicalOrExpression )
]

{ #category : #generated }
DartIfNullExpressionNode >> testNullExpression [

	^ testNullExpression
]

{ #category : #generated }
DartIfNullExpressionNode >> testNullExpression: aDartProgramNode [

	self testNullExpression notNil ifTrue: [ 
		self testNullExpression parent: nil ].
	testNullExpression := aDartProgramNode.
	self testNullExpression notNil ifTrue: [ 
		self testNullExpression parent: self ]
]
