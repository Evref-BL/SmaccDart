Class {
	#name : #DartSwitchStatementNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'switchCaseExpression',
		'caseses',
		'defaultCase'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartSwitchStatementNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitSwitchStatement: self
]

{ #category : #generated }
DartSwitchStatementNode >> caseses [

	^ caseses
]

{ #category : #generated }
DartSwitchStatementNode >> caseses: anOrderedCollection [

	self setParents: self caseses to: nil.
	caseses := anOrderedCollection.
	self setParents: self caseses to: self
]

{ #category : #generated }
DartSwitchStatementNode >> compositeNodeVariables [

	^ #( #caseses )
]

{ #category : #generated }
DartSwitchStatementNode >> defaultCase [

	^ defaultCase
]

{ #category : #generated }
DartSwitchStatementNode >> defaultCase: aDartDefaultCaseNode [

	self defaultCase notNil ifTrue: [ self defaultCase parent: nil ].
	defaultCase := aDartDefaultCaseNode.
	self defaultCase notNil ifTrue: [ self defaultCase parent: self ]
]

{ #category : #'generated-initialize-release' }
DartSwitchStatementNode >> initialize [
	super initialize.
	caseses := OrderedCollection new: 2.
]

{ #category : #generated }
DartSwitchStatementNode >> nodeVariables [

	^ #( #defaultCase )
]

{ #category : #generated }
DartSwitchStatementNode >> otherVariables [

	^ #( #switchCaseExpression )
]

{ #category : #generated }
DartSwitchStatementNode >> switchCaseExpression [

	^ switchCaseExpression
]

{ #category : #generated }
DartSwitchStatementNode >> switchCaseExpression: anOrderedCollection [

	self setParent: self switchCaseExpression to: nil.
	switchCaseExpression := anOrderedCollection.
	self setParent: self switchCaseExpression to: self
]
