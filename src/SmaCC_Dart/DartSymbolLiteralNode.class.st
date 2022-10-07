Class {
	#name : #DartSymbolLiteralNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'operator',
		'identifiers'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartSymbolLiteralNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitSymbolLiteral: self
]

{ #category : #generated }
DartSymbolLiteralNode >> compositeNodeVariables [

	^ #( #identifiers )
]

{ #category : #generated }
DartSymbolLiteralNode >> identifiers [

	^ identifiers
]

{ #category : #generated }
DartSymbolLiteralNode >> identifiers: anOrderedCollection [

	self setParents: self identifiers to: nil.
	identifiers := anOrderedCollection.
	self setParents: self identifiers to: self
]

{ #category : #'generated-initialize-release' }
DartSymbolLiteralNode >> initialize [
	super initialize.
	identifiers := OrderedCollection new: 2.
]

{ #category : #generated }
DartSymbolLiteralNode >> operator [

	^ operator
]

{ #category : #generated }
DartSymbolLiteralNode >> operator: aSmaCCToken [

	operator := aSmaCCToken
]

{ #category : #generated }
DartSymbolLiteralNode >> tokenVariables [

	^ #( #operator )
]
