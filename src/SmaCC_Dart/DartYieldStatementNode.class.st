Class {
	#name : #DartYieldStatementNode,
	#superclass : #DartStatementNode,
	#instVars : [
		'yieldToken',
		'expression',
		'semicolonToken'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartYieldStatementNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitYieldStatement: self
]

{ #category : #generated }
DartYieldStatementNode >> expression [

	^ expression
]

{ #category : #generated }
DartYieldStatementNode >> expression: anOrderedCollection [

	self setParent: self expression to: nil.
	expression := anOrderedCollection.
	self setParent: self expression to: self
]

{ #category : #generated }
DartYieldStatementNode >> otherVariables [

	^ #( #expression )
]

{ #category : #generated }
DartYieldStatementNode >> semicolonToken [

	^ semicolonToken
]

{ #category : #generated }
DartYieldStatementNode >> semicolonToken: aSmaCCToken [

	semicolonToken := aSmaCCToken
]

{ #category : #generated }
DartYieldStatementNode >> tokenVariables [

	^ #( #yieldToken #semicolonToken )
]

{ #category : #generated }
DartYieldStatementNode >> yieldToken [

	^ yieldToken
]

{ #category : #generated }
DartYieldStatementNode >> yieldToken: aSmaCCToken [

	yieldToken := aSmaCCToken
]
