Class {
	#name : #DartNumericLiteralNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'number',
		'hexNumber'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartNumericLiteralNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitNumericLiteral: self
]

{ #category : #generated }
DartNumericLiteralNode >> hexNumber [

	^ hexNumber
]

{ #category : #generated }
DartNumericLiteralNode >> hexNumber: aSmaCCToken [

	hexNumber := aSmaCCToken
]

{ #category : #generated }
DartNumericLiteralNode >> number [

	^ number
]

{ #category : #generated }
DartNumericLiteralNode >> number: aSmaCCToken [

	number := aSmaCCToken
]

{ #category : #generated }
DartNumericLiteralNode >> tokenVariables [

	^ #( #number #hexNumber )
]
