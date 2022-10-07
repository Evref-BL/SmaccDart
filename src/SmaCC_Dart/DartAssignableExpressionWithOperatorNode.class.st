Class {
	#name : #DartAssignableExpressionWithOperatorNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'assignableExpression',
		'assignmentOperator',
		'assigmnentExpression'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartAssignableExpressionWithOperatorNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitAssignableExpressionWithOperator: self
]

{ #category : #generated }
DartAssignableExpressionWithOperatorNode >> assigmnentExpression [

	^ assigmnentExpression
]

{ #category : #generated }
DartAssignableExpressionWithOperatorNode >> assigmnentExpression: anOrderedCollection [

	self setParent: self assigmnentExpression to: nil.
	assigmnentExpression := anOrderedCollection.
	self setParent: self assigmnentExpression to: self
]

{ #category : #generated }
DartAssignableExpressionWithOperatorNode >> assignableExpression [

	^ assignableExpression
]

{ #category : #generated }
DartAssignableExpressionWithOperatorNode >> assignableExpression: aDartAssignableExpressionNode [

	self assignableExpression notNil ifTrue: [ 
		self assignableExpression parent: nil ].
	assignableExpression := aDartAssignableExpressionNode.
	self assignableExpression notNil ifTrue: [ 
		self assignableExpression parent: self ]
]

{ #category : #generated }
DartAssignableExpressionWithOperatorNode >> assignmentOperator [

	^ assignmentOperator
]

{ #category : #generated }
DartAssignableExpressionWithOperatorNode >> assignmentOperator: aDartAssignmentOperatorNode [

	self assignmentOperator notNil ifTrue: [ 
		self assignmentOperator parent: nil ].
	assignmentOperator := aDartAssignmentOperatorNode.
	self assignmentOperator notNil ifTrue: [ 
		self assignmentOperator parent: self ]
]

{ #category : #generated }
DartAssignableExpressionWithOperatorNode >> nodeVariables [

	^ #( #assignableExpression #assignmentOperator )
]

{ #category : #generated }
DartAssignableExpressionWithOperatorNode >> otherVariables [

	^ #( #assigmnentExpression )
]
