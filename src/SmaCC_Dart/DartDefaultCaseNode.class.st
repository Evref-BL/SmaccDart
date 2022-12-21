Class {
	#name : #DartDefaultCaseNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'labels',
		'statements'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartDefaultCaseNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitDefaultCase: self
]

{ #category : #generated }
DartDefaultCaseNode >> compositeNodeVariables [

	^ #( #labels )
]

{ #category : #'generated-initialize-release' }
DartDefaultCaseNode >> initialize [
	super initialize.
	labels := OrderedCollection new: 2.
]

{ #category : #generated }
DartDefaultCaseNode >> labels [

	^ labels
]

{ #category : #generated }
DartDefaultCaseNode >> labels: anOrderedCollection [

	self setParents: self labels to: nil.
	labels := anOrderedCollection.
	self setParents: self labels to: self
]

{ #category : #generated }
DartDefaultCaseNode >> nodeVariables [

	^ #( #statements )
]

{ #category : #generated }
DartDefaultCaseNode >> statements [

	^ statements
]

{ #category : #generated }
DartDefaultCaseNode >> statements: aDartStatementsNode [

	self statements notNil ifTrue: [ self statements parent: nil ].
	statements := aDartStatementsNode.
	self statements notNil ifTrue: [ self statements parent: self ]
]
