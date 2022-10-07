Class {
	#name : #DartYieldEachStatementNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'yieldEachExpression'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartYieldEachStatementNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitYieldEachStatement: self
]

{ #category : #generated }
DartYieldEachStatementNode >> otherVariables [

	^ #( #yieldEachExpression )
]

{ #category : #generated }
DartYieldEachStatementNode >> yieldEachExpression [

	^ yieldEachExpression
]

{ #category : #generated }
DartYieldEachStatementNode >> yieldEachExpression: anOrderedCollection [

	self setParent: self yieldEachExpression to: nil.
	yieldEachExpression := anOrderedCollection.
	self setParent: self yieldEachExpression to: self
]
