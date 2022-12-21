Class {
	#name : #DartSwitchCaseNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'labels',
		'expression',
		'statements'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartSwitchCaseNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitSwitchCase: self
]

{ #category : #generated }
DartSwitchCaseNode >> compositeNodeVariables [

	^ #( #labels )
]

{ #category : #generated }
DartSwitchCaseNode >> expression [

	^ expression
]

{ #category : #generated }
DartSwitchCaseNode >> expression: anOrderedCollection [

	self setParent: self expression to: nil.
	expression := anOrderedCollection.
	self setParent: self expression to: self
]

{ #category : #'generated-initialize-release' }
DartSwitchCaseNode >> initialize [
	super initialize.
	labels := OrderedCollection new: 2.
]

{ #category : #generated }
DartSwitchCaseNode >> labels [

	^ labels
]

{ #category : #generated }
DartSwitchCaseNode >> labels: anOrderedCollection [

	self setParents: self labels to: nil.
	labels := anOrderedCollection.
	self setParents: self labels to: self
]

{ #category : #generated }
DartSwitchCaseNode >> nodeVariables [

	^ #( #statements )
]

{ #category : #generated }
DartSwitchCaseNode >> otherVariables [

	^ #( #expression )
]

{ #category : #generated }
DartSwitchCaseNode >> statements [

	^ statements
]

{ #category : #generated }
DartSwitchCaseNode >> statements: aDartStatementsNode [

	self statements notNil ifTrue: [ self statements parent: nil ].
	statements := aDartStatementsNode.
	self statements notNil ifTrue: [ self statements parent: self ]
]
