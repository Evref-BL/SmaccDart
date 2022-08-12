Class {
	#name : #DartYieldEachStatementNode,
	#superclass : #DartYieldStatementNode,
	#instVars : [
		'timesToken'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartYieldEachStatementNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitYieldEachStatement: self
]

{ #category : #generated }
DartYieldEachStatementNode >> timesToken [

	^ timesToken
]

{ #category : #generated }
DartYieldEachStatementNode >> timesToken: aSmaCCToken [

	timesToken := aSmaCCToken
]

{ #category : #generated }
DartYieldEachStatementNode >> tokenVariables [

	^ #( #timesToken #yieldToken #semicolonToken )
]
