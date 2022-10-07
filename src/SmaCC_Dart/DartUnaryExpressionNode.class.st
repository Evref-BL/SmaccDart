Class {
	#name : #DartUnaryExpressionNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'prefixOperator',
		'unaryExpression',
		'minusOperator',
		'tildeOperator',
		'incrementOperator',
		'assignableExpression'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartUnaryExpressionNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitUnaryExpression: self
]

{ #category : #generated }
DartUnaryExpressionNode >> assignableExpression [

	^ assignableExpression
]

{ #category : #generated }
DartUnaryExpressionNode >> assignableExpression: aDartAssignableExpressionNode [

	self assignableExpression notNil ifTrue: [ 
		self assignableExpression parent: nil ].
	assignableExpression := aDartAssignableExpressionNode.
	self assignableExpression notNil ifTrue: [ 
		self assignableExpression parent: self ]
]

{ #category : #generated }
DartUnaryExpressionNode >> incrementOperator [

	^ incrementOperator
]

{ #category : #generated }
DartUnaryExpressionNode >> incrementOperator: aSmaCCToken [

	incrementOperator := aSmaCCToken
]

{ #category : #generated }
DartUnaryExpressionNode >> minusOperator [

	^ minusOperator
]

{ #category : #generated }
DartUnaryExpressionNode >> minusOperator: aSmaCCToken [

	minusOperator := aSmaCCToken
]

{ #category : #generated }
DartUnaryExpressionNode >> nodeVariables [

	^ #( #unaryExpression #assignableExpression )
]

{ #category : #generated }
DartUnaryExpressionNode >> prefixOperator [

	^ prefixOperator
]

{ #category : #generated }
DartUnaryExpressionNode >> prefixOperator: aSmaCCToken [

	prefixOperator := aSmaCCToken
]

{ #category : #generated }
DartUnaryExpressionNode >> tildeOperator [

	^ tildeOperator
]

{ #category : #generated }
DartUnaryExpressionNode >> tildeOperator: aSmaCCToken [

	tildeOperator := aSmaCCToken
]

{ #category : #generated }
DartUnaryExpressionNode >> tokenVariables [

	^ #( #prefixOperator #minusOperator #tildeOperator #incrementOperator )
]

{ #category : #generated }
DartUnaryExpressionNode >> unaryExpression [

	^ unaryExpression
]

{ #category : #generated }
DartUnaryExpressionNode >> unaryExpression: aDartProgramNode [

	self unaryExpression notNil ifTrue: [ 
		self unaryExpression parent: nil ].
	unaryExpression := aDartProgramNode.
	self unaryExpression notNil ifTrue: [ 
		self unaryExpression parent: self ]
]
