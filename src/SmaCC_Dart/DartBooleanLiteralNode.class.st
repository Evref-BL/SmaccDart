Class {
	#name : #DartBooleanLiteralNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'_true',
		'_false'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartBooleanLiteralNode >> _false [

	^ _false
]

{ #category : #generated }
DartBooleanLiteralNode >> _false: aSmaCCToken [

	_false := aSmaCCToken
]

{ #category : #generated }
DartBooleanLiteralNode >> _true [

	^ _true
]

{ #category : #generated }
DartBooleanLiteralNode >> _true: aSmaCCToken [

	_true := aSmaCCToken
]

{ #category : #generated }
DartBooleanLiteralNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitBooleanLiteral: self
]

{ #category : #generated }
DartBooleanLiteralNode >> tokenVariables [

	^ #( #_true #_false )
]
