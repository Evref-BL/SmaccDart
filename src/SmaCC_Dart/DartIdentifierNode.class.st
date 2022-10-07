Class {
	#name : #DartIdentifierNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'idNotFunction',
		'function'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartIdentifierNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitIdentifier: self
]

{ #category : #generated }
DartIdentifierNode >> function [

	^ function
]

{ #category : #generated }
DartIdentifierNode >> function: aSmaCCToken [

	function := aSmaCCToken
]

{ #category : #generated }
DartIdentifierNode >> idNotFunction [

	^ idNotFunction
]

{ #category : #generated }
DartIdentifierNode >> idNotFunction: aSmaCCToken [

	idNotFunction := aSmaCCToken
]

{ #category : #generated }
DartIdentifierNode >> tokenVariables [

	^ #( #idNotFunction #function )
]
