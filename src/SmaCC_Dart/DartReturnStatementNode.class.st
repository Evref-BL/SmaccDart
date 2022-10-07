Class {
	#name : #DartReturnStatementNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'returnExpression'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartReturnStatementNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitReturnStatement: self
]

{ #category : #generated }
DartReturnStatementNode >> otherVariables [

	^ #( #returnExpression )
]

{ #category : #generated }
DartReturnStatementNode >> returnExpression [

	^ returnExpression
]

{ #category : #generated }
DartReturnStatementNode >> returnExpression: anOrderedCollection [

	self setParent: self returnExpression to: nil.
	returnExpression := anOrderedCollection.
	self setParent: self returnExpression to: self
]
