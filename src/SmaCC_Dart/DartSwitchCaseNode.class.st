Class {
	#name : #DartSwitchCaseNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'LabelIdentifiers',
		'assignableExpression',
		'assignmentOperator',
		'assignedExpression',
		'conditionalExpression',
		'cascade',
		'throwExpression',
		'statementsSets'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartSwitchCaseNode >> LabelIdentifiers [

	^ LabelIdentifiers
]

{ #category : #generated }
DartSwitchCaseNode >> LabelIdentifiers: anOrderedCollection [

	self setParents: self LabelIdentifiers to: nil.
	LabelIdentifiers := anOrderedCollection.
	self setParents: self LabelIdentifiers to: self
]

{ #category : #generated }
DartSwitchCaseNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitSwitchCase: self
]

{ #category : #generated }
DartSwitchCaseNode >> assignableExpression [

	^ assignableExpression
]

{ #category : #generated }
DartSwitchCaseNode >> assignableExpression: aDartAssignableExpressionNode [

	self assignableExpression notNil ifTrue: [ 
		self assignableExpression parent: nil ].
	assignableExpression := aDartAssignableExpressionNode.
	self assignableExpression notNil ifTrue: [ 
		self assignableExpression parent: self ]
]

{ #category : #generated }
DartSwitchCaseNode >> assignedExpression [

	^ assignedExpression
]

{ #category : #generated }
DartSwitchCaseNode >> assignedExpression: anOrderedCollection [

	self setParent: self assignedExpression to: nil.
	assignedExpression := anOrderedCollection.
	self setParent: self assignedExpression to: self
]

{ #category : #generated }
DartSwitchCaseNode >> assignmentOperator [

	^ assignmentOperator
]

{ #category : #generated }
DartSwitchCaseNode >> assignmentOperator: aDartAssignmentOperatorNode [

	self assignmentOperator notNil ifTrue: [ 
		self assignmentOperator parent: nil ].
	assignmentOperator := aDartAssignmentOperatorNode.
	self assignmentOperator notNil ifTrue: [ 
		self assignmentOperator parent: self ]
]

{ #category : #generated }
DartSwitchCaseNode >> cascade [

	^ cascade
]

{ #category : #generated }
DartSwitchCaseNode >> cascade: aDartCascadeNode [

	self cascade notNil ifTrue: [ self cascade parent: nil ].
	cascade := aDartCascadeNode.
	self cascade notNil ifTrue: [ self cascade parent: self ]
]

{ #category : #generated }
DartSwitchCaseNode >> compositeNodeVariables [

	^ #( #LabelIdentifiers #statementsSets )
]

{ #category : #generated }
DartSwitchCaseNode >> conditionalExpression [

	^ conditionalExpression
]

{ #category : #generated }
DartSwitchCaseNode >> conditionalExpression: aDartProgramNode [

	self conditionalExpression notNil ifTrue: [ 
		self conditionalExpression parent: nil ].
	conditionalExpression := aDartProgramNode.
	self conditionalExpression notNil ifTrue: [ 
		self conditionalExpression parent: self ]
]

{ #category : #'generated-initialize-release' }
DartSwitchCaseNode >> initialize [
	super initialize.
	LabelIdentifiers := OrderedCollection new: 2.
	statementsSets := OrderedCollection new: 2.
]

{ #category : #generated }
DartSwitchCaseNode >> nodeVariables [

	^ #( #assignableExpression #assignmentOperator #conditionalExpression
	     #cascade #throwExpression )
]

{ #category : #generated }
DartSwitchCaseNode >> otherVariables [

	^ #( #assignedExpression )
]

{ #category : #generated }
DartSwitchCaseNode >> statementsSets [

	^ statementsSets
]

{ #category : #generated }
DartSwitchCaseNode >> statementsSets: anOrderedCollection [

	self setParents: self statementsSets to: nil.
	statementsSets := anOrderedCollection.
	self setParents: self statementsSets to: self
]

{ #category : #generated }
DartSwitchCaseNode >> throwExpression [

	^ throwExpression
]

{ #category : #generated }
DartSwitchCaseNode >> throwExpression: aDartThrowExpressionNode [

	self throwExpression notNil ifTrue: [ 
		self throwExpression parent: nil ].
	throwExpression := aDartThrowExpressionNode.
	self throwExpression notNil ifTrue: [ 
		self throwExpression parent: self ]
]
