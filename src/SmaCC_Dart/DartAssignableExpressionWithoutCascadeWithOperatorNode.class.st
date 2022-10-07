Class {
	#name : #DartAssignableExpressionWithoutCascadeWithOperatorNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'assignableExpression',
		'assignmentOperator',
		'expressionWithoutCascade'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartAssignableExpressionWithoutCascadeWithOperatorNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor 
		  visitAssignableExpressionWithoutCascadeWithOperator: self
]

{ #category : #generated }
DartAssignableExpressionWithoutCascadeWithOperatorNode >> assignableExpression [

	^ assignableExpression
]

{ #category : #generated }
DartAssignableExpressionWithoutCascadeWithOperatorNode >> assignableExpression: aDartAssignableExpressionNode [

	self assignableExpression notNil ifTrue: [ 
		self assignableExpression parent: nil ].
	assignableExpression := aDartAssignableExpressionNode.
	self assignableExpression notNil ifTrue: [ 
		self assignableExpression parent: self ]
]

{ #category : #generated }
DartAssignableExpressionWithoutCascadeWithOperatorNode >> assignmentOperator [

	^ assignmentOperator
]

{ #category : #generated }
DartAssignableExpressionWithoutCascadeWithOperatorNode >> assignmentOperator: aDartAssignmentOperatorNode [

	self assignmentOperator notNil ifTrue: [ 
		self assignmentOperator parent: nil ].
	assignmentOperator := aDartAssignmentOperatorNode.
	self assignmentOperator notNil ifTrue: [ 
		self assignmentOperator parent: self ]
]

{ #category : #generated }
DartAssignableExpressionWithoutCascadeWithOperatorNode >> expressionWithoutCascade [

	^ expressionWithoutCascade
]

{ #category : #generated }
DartAssignableExpressionWithoutCascadeWithOperatorNode >> expressionWithoutCascade: aDartProgramNode [

	self expressionWithoutCascade notNil ifTrue: [ 
		self expressionWithoutCascade parent: nil ].
	expressionWithoutCascade := aDartProgramNode.
	self expressionWithoutCascade notNil ifTrue: [ 
		self expressionWithoutCascade parent: self ]
]

{ #category : #generated }
DartAssignableExpressionWithoutCascadeWithOperatorNode >> nodeVariables [

	^ #( #assignableExpression #assignmentOperator #expressionWithoutCascade )
]
