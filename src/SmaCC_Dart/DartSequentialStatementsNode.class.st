Class {
	#name : #DartSequentialStatementsNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'statementsSets'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartSequentialStatementsNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitSequentialStatements: self
]

{ #category : #generated }
DartSequentialStatementsNode >> compositeNodeVariables [

	^ #( #statementsSets )
]

{ #category : #'generated-initialize-release' }
DartSequentialStatementsNode >> initialize [
	super initialize.
	statementsSets := OrderedCollection new: 2.
]

{ #category : #generated }
DartSequentialStatementsNode >> statementsSets [

	^ statementsSets
]

{ #category : #generated }
DartSequentialStatementsNode >> statementsSets: anOrderedCollection [

	self setParents: self statementsSets to: nil.
	statementsSets := anOrderedCollection.
	self setParents: self statementsSets to: self
]
