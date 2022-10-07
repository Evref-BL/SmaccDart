Class {
	#name : #DartTypeParameterNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'metadata',
		'typeIdentifier',
		'typeNotVoid'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartTypeParameterNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitTypeParameter: self
]

{ #category : #generated }
DartTypeParameterNode >> metadata [

	^ metadata
]

{ #category : #generated }
DartTypeParameterNode >> metadata: anObject [

	self setParent: self metadata to: nil.
	metadata := anObject.
	self setParent: self metadata to: self
]

{ #category : #generated }
DartTypeParameterNode >> otherVariables [

	^ #( #metadata #typeNotVoid )
]

{ #category : #generated }
DartTypeParameterNode >> tokenVariables [

	^ #( #typeIdentifier )
]

{ #category : #generated }
DartTypeParameterNode >> typeIdentifier [

	^ typeIdentifier
]

{ #category : #generated }
DartTypeParameterNode >> typeIdentifier: aSmaCCToken [

	typeIdentifier := aSmaCCToken
]

{ #category : #generated }
DartTypeParameterNode >> typeNotVoid [

	^ typeNotVoid
]

{ #category : #generated }
DartTypeParameterNode >> typeNotVoid: anObject [

	self setParent: self typeNotVoid to: nil.
	typeNotVoid := anObject.
	self setParent: self typeNotVoid to: self
]
