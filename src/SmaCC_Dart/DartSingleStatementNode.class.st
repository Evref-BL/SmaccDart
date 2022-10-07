Class {
	#name : #DartSingleStatementNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'labels',
		'nonLabelledStatement'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartSingleStatementNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitSingleStatement: self
]

{ #category : #generated }
DartSingleStatementNode >> compositeNodeVariables [

	^ #( #labels )
]

{ #category : #'generated-initialize-release' }
DartSingleStatementNode >> initialize [
	super initialize.
	labels := OrderedCollection new: 2.
]

{ #category : #generated }
DartSingleStatementNode >> labels [

	^ labels
]

{ #category : #generated }
DartSingleStatementNode >> labels: anOrderedCollection [

	self setParents: self labels to: nil.
	labels := anOrderedCollection.
	self setParents: self labels to: self
]

{ #category : #generated }
DartSingleStatementNode >> nonLabelledStatement [

	^ nonLabelledStatement
]

{ #category : #generated }
DartSingleStatementNode >> nonLabelledStatement: anObject [

	self setParent: self nonLabelledStatement to: nil.
	nonLabelledStatement := anObject.
	self setParent: self nonLabelledStatement to: self
]

{ #category : #generated }
DartSingleStatementNode >> otherVariables [

	^ #( #nonLabelledStatement )
]
