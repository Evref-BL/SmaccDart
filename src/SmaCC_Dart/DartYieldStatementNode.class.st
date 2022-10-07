Class {
	#name : #DartYieldStatementNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'yieldExpression'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartYieldStatementNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitYieldStatement: self
]

{ #category : #generated }
DartYieldStatementNode >> otherVariables [

	^ #( #yieldExpression )
]

{ #category : #generated }
DartYieldStatementNode >> yieldExpression [

	^ yieldExpression
]

{ #category : #generated }
DartYieldStatementNode >> yieldExpression: anOrderedCollection [

	self setParent: self yieldExpression to: nil.
	yieldExpression := anOrderedCollection.
	self setParent: self yieldExpression to: self
]
