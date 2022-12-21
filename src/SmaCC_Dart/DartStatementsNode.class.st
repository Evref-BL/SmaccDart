Class {
	#name : #DartStatementsNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'statements'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartStatementsNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitStatements: self
]

{ #category : #generated }
DartStatementsNode >> compositeNodeVariables [

	^ #( #statements )
]

{ #category : #'generated-initialize-release' }
DartStatementsNode >> initialize [
	super initialize.
	statements := OrderedCollection new: 2.
]

{ #category : #generated }
DartStatementsNode >> statements [

	^ statements
]

{ #category : #generated }
DartStatementsNode >> statements: anOrderedCollection [

	self setParents: self statements to: nil.
	statements := anOrderedCollection.
	self setParents: self statements to: self
]
