Class {
	#name : #DartBlockNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'lbraceToken',
		'statements',
		'rbraceToken'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartBlockNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitBlock: self
]

{ #category : #generated }
DartBlockNode >> lbraceToken [

	^ lbraceToken
]

{ #category : #generated }
DartBlockNode >> lbraceToken: aSmaCCToken [

	lbraceToken := aSmaCCToken
]

{ #category : #generated }
DartBlockNode >> otherVariables [

	^ #( #statements )
]

{ #category : #generated }
DartBlockNode >> rbraceToken [

	^ rbraceToken
]

{ #category : #generated }
DartBlockNode >> rbraceToken: aSmaCCToken [

	rbraceToken := aSmaCCToken
]

{ #category : #generated }
DartBlockNode >> statements [

	^ statements
]

{ #category : #generated }
DartBlockNode >> statements: anOrderedCollection [

	self setParent: self statements to: nil.
	statements := anOrderedCollection.
	self setParent: self statements to: self
]

{ #category : #generated }
DartBlockNode >> tokenVariables [

	^ #( #lbraceToken #rbraceToken )
]
