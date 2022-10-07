Class {
	#name : #DartAwaitExpressionNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'unaryExpression'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartAwaitExpressionNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitAwaitExpression: self
]

{ #category : #generated }
DartAwaitExpressionNode >> nodeVariables [

	^ #( #unaryExpression )
]

{ #category : #generated }
DartAwaitExpressionNode >> unaryExpression [

	^ unaryExpression
]

{ #category : #generated }
DartAwaitExpressionNode >> unaryExpression: aDartProgramNode [

	self unaryExpression notNil ifTrue: [ 
		self unaryExpression parent: nil ].
	unaryExpression := aDartProgramNode.
	self unaryExpression notNil ifTrue: [ 
		self unaryExpression parent: self ]
]
