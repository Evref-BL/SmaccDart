Class {
	#name : #DartExportDeclarationNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'stringLiteralWithoutInterpolation',
		'identifiers',
		'nextIdentifiers'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartExportDeclarationNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitExportDeclaration: self
]

{ #category : #generated }
DartExportDeclarationNode >> compositeNodeVariables [

	^ #( #identifiers #nextIdentifiers )
]

{ #category : #generated }
DartExportDeclarationNode >> identifiers [

	^ identifiers
]

{ #category : #generated }
DartExportDeclarationNode >> identifiers: anOrderedCollection [

	self setParents: self identifiers to: nil.
	identifiers := anOrderedCollection.
	self setParents: self identifiers to: self
]

{ #category : #'generated-initialize-release' }
DartExportDeclarationNode >> initialize [
	super initialize.
	identifiers := OrderedCollection new: 2.
	nextIdentifiers := OrderedCollection new: 2.
]

{ #category : #generated }
DartExportDeclarationNode >> nextIdentifiers [

	^ nextIdentifiers
]

{ #category : #generated }
DartExportDeclarationNode >> nextIdentifiers: anOrderedCollection [

	self setParents: self nextIdentifiers to: nil.
	nextIdentifiers := anOrderedCollection.
	self setParents: self nextIdentifiers to: self
]

{ #category : #generated }
DartExportDeclarationNode >> stringLiteralWithoutInterpolation [

	^ stringLiteralWithoutInterpolation
]

{ #category : #generated }
DartExportDeclarationNode >> stringLiteralWithoutInterpolation: aSmaCCToken [

	stringLiteralWithoutInterpolation := aSmaCCToken
]

{ #category : #generated }
DartExportDeclarationNode >> tokenVariables [

	^ #( #stringLiteralWithoutInterpolation )
]
