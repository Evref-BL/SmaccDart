Class {
	#name : #DartUriNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'stringLiteralWithoutInterpolation'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartUriNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitUri: self
]

{ #category : #generated }
DartUriNode >> stringLiteralWithoutInterpolation [

	^ stringLiteralWithoutInterpolation
]

{ #category : #generated }
DartUriNode >> stringLiteralWithoutInterpolation: aSmaCCToken [

	stringLiteralWithoutInterpolation := aSmaCCToken
]

{ #category : #generated }
DartUriNode >> tokenVariables [

	^ #( #stringLiteralWithoutInterpolation )
]
