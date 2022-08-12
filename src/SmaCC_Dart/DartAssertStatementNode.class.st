Class {
	#name : #DartAssertStatementNode,
	#superclass : #DartStatementNode,
	#instVars : [
		'assertion',
		'semicolonToken'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartAssertStatementNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitAssertStatement: self
]

{ #category : #generated }
DartAssertStatementNode >> assertion [

	^ assertion
]

{ #category : #generated }
DartAssertStatementNode >> assertion: anOrderedCollection [

	self setParent: self assertion to: nil.
	assertion := anOrderedCollection.
	self setParent: self assertion to: self
]

{ #category : #generated }
DartAssertStatementNode >> otherVariables [

	^ #( #assertion )
]

{ #category : #generated }
DartAssertStatementNode >> semicolonToken [

	^ semicolonToken
]

{ #category : #generated }
DartAssertStatementNode >> semicolonToken: aSmaCCToken [

	semicolonToken := aSmaCCToken
]

{ #category : #generated }
DartAssertStatementNode >> tokenVariables [

	^ #( #semicolonToken )
]
